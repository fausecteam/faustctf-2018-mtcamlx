open Yojson.Safe
open Yojson.Safe.Util

type public_params =
  {
    group     : Z.t;
    generator : Z.t;
    publickey : Z.t
  } [@@deriving fields]

type client_keys =
  {
    elgkey : Z.t
  } [@@deriving fields]


type server_keys =
  {
    prfkey : Z.t;
    elgkey : Z.t
  } [@@deriving fields]


let parseServer filename : server_keys =
  let infile = open_in filename in
  let json = from_channel infile in
  {
    elgkey = Z.of_string_base 16 (json |> member "elgkey" |> to_string);
    prfkey = Z.of_string_base 16 (json |> member "prfkey" |> to_string);
  }

let parseClient filename : client_keys =
  let infile = open_in filename in
  let json = from_channel infile in
  {
    elgkey = Z.of_string_base 16 (json |> member "elgkey" |> to_string);
  }


let parsePublic filename : public_params =
  let infile = open_in filename in
  let json = from_channel infile in
  {
    group     = Z.of_string_base 16 (json |> member "group"     |> to_string);
    generator = Z.of_string_base 16 (json |> member "generator" |> to_string);
    publickey = Z.of_string_base 16 (json |> member "publickey" |> to_string);
  }

let parseSchnorr filename : (Z.t list) =
  let infile = open_in filename in
  let json = from_channel infile in
  let (thelist : Yojson.Safe.json list)  = json |> to_list in
  let mapf (x : Yojson.Safe.json) : Z.t =
    Z.of_string_base 16 (x |> to_string)
  in
  List.map mapf thelist


(*
 * Schnorr Identification Scheme
 * See RFC 8235
 *)
let schnorrId (keys : client_keys) (public : public_params) : string =
  let v = FaustMath.randZ () in
  let a = keys.elgkey in
  let g2v = Z.powm_sec public.generator v public.group in
  let g2a = Z.powm_sec public.generator a public.group in
  let ctx = Sha256.init () in

  Sha256.update_string ctx (Z.format "%x" public.generator);
  Sha256.update_string ctx "|";

  Sha256.update_string ctx (Z.format "%x" public.group);
  Sha256.update_string ctx "|";

  Sha256.update_string ctx (Z.format "%x" g2v);
  Sha256.update_string ctx "|";

  Sha256.update_string ctx (Z.format "%x" g2a);
  Sha256.update_string ctx "|";

  let c = Z.of_string_base 16 (Sha256.to_hex (Sha256.finalize ctx)) in
  let r = FaustMath.normalize Z.(v - a * c) Z.(public.group - ~$1) in

  let (id : Yojson.Basic.json) =
    `Assoc [("r", `String (Z.format "%x" r));
            ("V", `String (Z.format "%x" g2v));
           ] in
  Yojson.Basic.pretty_to_string id

let schnorrValidate pkey public jsons =
  let json = from_string jsons in
  let ctx  = Sha256.init () in
  let r    = Z.of_string_base 16 (json |> member "r" |> to_string); in
  let g2v  = Z.of_string_base 16 (json |> member "V" |> to_string); in
  let g2r  = Z.powm_sec public.generator r public.group in

  Sha256.update_string ctx (Z.format "%x" public.generator);
  Sha256.update_string ctx "|";

  Sha256.update_string ctx (Z.format "%x" public.group);
  Sha256.update_string ctx "|";

  Sha256.update_string ctx (Z.format "%x" g2v);
  Sha256.update_string ctx "|";

  Sha256.update_string ctx (Z.format "%x" pkey);
  Sha256.update_string ctx "|";

  let c    = Z.of_string_base 16 (Sha256.to_hex (Sha256.finalize ctx)) in
  let a2c  = Z.powm_sec pkey c public.group in
  let g2v' = Z.rem Z.(g2r * a2c) public.group in

  Z.equal g2v g2v'
