open Printf
open List
open Http_types
open FaustAuthenticate

let opad = Z.of_string_base 16 "5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c"
let ipad = Z.of_string_base 16 "363636363636363636363636363636363636363636363636363636363636"


(* M. Naor, B. Pinkas, and O. Reingold *)
(* Distributed pseudo-random functions and kdcs *)
(* EUROCRYPT 99 *)
(* For Domain = G *)
(* let npr_prf (key : Z.t) (input : string) : Z.t = *)
(*   let hashstr = Sha256.to_hex (Sha256.string input) in *)
(*   let hash = Z.of_string_base 16 hashstr in *)
(*   Z.powm_sec hash key !group *)


(* H. Krawczyk, M. Bellare, R. Canetti *)
(* RFC 2104 *)
(* For Domain = Z_|G| *)
let hmac (key : Z.t) (input : string) : Z.t =
  let ikey = Z.format "%X" (Z.logxor key ipad) in
  let okey = Z.format "%X" (Z.logxor key opad) in

  let istr = Sha256.to_hex (Sha256.string (ikey ^ input)) in
  let resstring = Sha256.to_hex (Sha256.string (okey ^ istr)) in
  Z.of_string_base 16 resstring


(* J. Schneider, N. Fleischhacker, D. Schröder, M. Backes *)
(* Efficient Cryptographic Password Hardening Services From Partially Oblivious Commitments *)
(* Proceedings of the 23rd ACM Conference on Computer and Communications Security (CCS '16), 2016 *)
let enroll pp sp req outchan : string =
  let t = assoc "t" req#params_GET in
  let v = Z.of_string_base 16 (assoc "v" req#params_GET) in

  let y = FaustMath.randZ () in
  let s = hmac sp.prfkey t in

  let c1 = Z.powm_sec pp.generator y pp.group in
  let c2 = Z.powm_sec pp.generator (Z.mul s y) pp.group in
  let prec3 = (Z.powm_sec pp.generator (Z.mul s (Z.mul y sp.elgkey)) pp.group) in
  let c3 = Z.rem (Z.mul prec3 v) pp.group in

  let gen_proof s x y : Yojson.Basic.json =
    let l = FaustMath.randZ () in
    let a = Z.powm_sec pp.generator l pp.group in
    let b = Z.powm_sec a x pp.group in
    let ctx = Sha256.init () in
    Sha256.update_string ctx (Z.format "%x|" pp.generator);
    Sha256.update_string ctx (Z.format "%x|" pp.group);
    Sha256.update_string ctx (Z.format "%x|" (Z.powm_sec pp.generator x pp.group));
    Sha256.update_string ctx (Z.format "%x|" c2);
    Sha256.update_string ctx (Z.format "%x|" prec3);
    Sha256.update_string ctx (Z.format "%x|" a);
    Sha256.update_string ctx (Z.format "%x|" b);
    let h = Z.of_string_base 16 (Sha256.to_hex (Sha256.finalize ctx)) in
    let z = FaustMath.normalize (Z.sub l (Z.mul h (Z.mul s y))) pp.group in
    `Assoc [("z", `String (Z.format "%x" z));
            ("A", `String (Z.format "%x" a));
            ("B", `String (Z.format "%x" b))]
  in

  let (person : Yojson.Basic.json) =
    `Assoc [("c1", `String (Z.format "%x" c1));
            ("c2", `String (Z.format "%x" c2));
            ("c3", `String (Z.format "%x" c3));
            ("proof", (gen_proof s sp.elgkey y))] in
  Yojson.Basic.pretty_to_string person


let validate pp sp req outchan : string =
  let t = assoc "t" req#params_GET in
  let v = Z.of_string_base 16 (assoc "v" req#params_GET) in
  let t1 = Z.of_string_base 16 (assoc "t1" req#params_GET) in

  let y = FaustMath.randZ () in
  let s = hmac sp.prfkey t in

  let c1 = Z.powm_sec pp.generator y pp.group in
  let c2 = Z.powm_sec pp.generator Z.(s * y) pp.group in
  let prec3 = Z.powm_sec pp.generator Z.(s * y * sp.elgkey) pp.group in
  let c3 = Z.rem Z.(prec3 * v) pp.group in

  let gen_proof_1 s x y : Yojson.Basic.json =
    let l = FaustMath.randZ () in
    let a = Z.powm_sec t1 l pp.group in
    let b = Z.powm_sec pp.generator Z.(y * l) pp.group in
    let ctx = Sha256.init () in

    (* ========== Parameter ========== *)
    Sha256.update_string ctx (Z.format "%x" pp.group);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" pp.generator);
    Sha256.update_string ctx "|";

    (* Sha256.update_string ctx (Z.format "%x" (Z.powm_sec pp.generator x pp.group)); *)
    (* Sha256.update_string ctx "|"; *)

    (* ========== Proof ========== *)
    Sha256.update_string ctx (Z.format "%x" a);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" b);
    Sha256.update_string ctx "|";

    (* ========== C ========== *)
    Sha256.update_string ctx (Z.format "%x" c1);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" (Z.powm_sec pp.generator Z.(y * s) pp.group));
    Sha256.update_string ctx "|";

    (* ========== T ========== *)
    Sha256.update_string ctx (Z.format "%x" t1);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" (Z.powm_sec t1 s pp.group));
    Sha256.update_string ctx "|";

    let hex = (Sha256.to_hex (Sha256.finalize ctx)) in
    let h = Z.of_string_base 16 hex in

    let z = FaustMath.normalize Z.(l - h * s) Z.(pp.group - ~$1) in
    `Assoc [("z", `String (Z.format "%x" z));
            ("A", `String (Z.format "%x" a));
            ("B", `String (Z.format "%x" b))]
  in

  let gen_proof_2 s x y : Yojson.Basic.json =
    let l = FaustMath.randZ () in
    let a = Z.powm_sec t1 l pp.group in
    let sx = Z.mul s x in
    let b = Z.powm_sec pp.generator (Z.mul y l) pp.group in
    let ctx = Sha256.init () in

    (* ========== Parameter ========== *)
    Sha256.update_string ctx (Z.format "%x" pp.group);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" pp.generator);
    Sha256.update_string ctx "|";

    (* Sha256.update_string ctx (Z.format "%x" (Z.powm_sec pp.generator x pp.group)); *)
    (* Sha256.update_string ctx "|"; *)

    (* ========== Proof ========== *)
    Sha256.update_string ctx (Z.format "%x" a);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" b);
    Sha256.update_string ctx "|";

    (* ========== C ========== *)
    Sha256.update_string ctx (Z.format "%x" c1);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" (Z.powm_sec pp.generator (Z.mul y sx) pp.group));
    Sha256.update_string ctx "|";

    (* ========== T ========== *)
    Sha256.update_string ctx (Z.format "%x" t1);
    Sha256.update_string ctx "|";

    Sha256.update_string ctx (Z.format "%x" (Z.powm_sec t1 sx pp.group));
    Sha256.update_string ctx "|";

    let h = Z.of_string_base 16 (Sha256.to_hex (Sha256.finalize ctx)) in
    let z = FaustMath.normalize Z.(l - h * sx) Z.(pp.group - ~$1) in
    `Assoc [("z", `String (Z.format "%x" z));
            ("A", `String (Z.format "%x" a));
            ("B", `String (Z.format "%x" b))]
  in

  let (person : Yojson.Basic.json) =
    `Assoc [("c1", `String (Z.format "%x" c1));
            ("c2", `String (Z.format "%x" c2));
            ("c3", `String (Z.format "%x" c3));
            ("proof1", (gen_proof_1 s sp.elgkey y));
            ("proof2", (gen_proof_2 s sp.elgkey y))] in
  Yojson.Basic.pretty_to_string person


let dispatch pp sp pkeylist (req : Http_types.request) (outchan : out_channel) : unit =
  (* Forking server and OCaml doesn't re-init its RNG on fork *)
  Random.self_init ();

(*
  let authh = req#header "Authentication" in
  let result = exists (fun pkey -> FaustAuthenticate.schnorrValidate pkey pp authh) pkeylist in
 *)
  let result = true in
  
  let response =
    (match req#path with
     | "/enroll" -> enroll pp sp req outchan
     | "/validate" -> validate pp sp req outchan
     | bad_request -> "Nope"
    ) in

  if result then
    Http_daemon.respond ~code:(`Code 200) ~body: response outchan
  else Http_daemon.respond ~code:(`Code 403) ~body: "Nope" outchan


let _ =
  let pp = FaustAuthenticate.parsePublic "public.json" in
  let sp = FaustAuthenticate.parseServer "secret.json" in
  let pkeylist = FaustAuthenticate.parseSchnorr "schnorr.json" in

  let spec =
    { Http_daemon.default_spec with
      address = "127.0.0.66";
      callback = dispatch pp sp pkeylist;
      port = 9997;
    }
  in
  Http_daemon.main spec
