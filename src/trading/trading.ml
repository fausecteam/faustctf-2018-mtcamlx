open Http_types
open List

let currencieslist = [ ("RIA",  "Tether (IRR)")
                     ; ("AST",  "Astral Ledger")
                     ; ("BTT",  "Butt Coin")
                     ; ("CAM",  "Caml Coin")
                     ; ("FAU",  "Faust Coin")
                     ; ("HVY",  "Heavy Coin")
                     ; ("IDI",  "IDIOTA")
                     ; ("KLO",  "Kloetzchen Kette")
                     ; ("LOK",  "Loki Cash")
                     ; ("RST",  "RESTchain")
                     ; ("SCI",  "Scientology")
                     ]


let currencies req outchan =
  let (entries : Yojson.Basic.json list) = map (fun (short, name) -> `String short) currencieslist in
  let (currencies : Yojson.Basic.json) = `List entries in
  Yojson.Basic.pretty_to_string currencies


(* TODO: At some pont do some more fancy logic on
 *       values
 *)
let info req outchan =
  let coin = assoc "coin" req#params_GET in
  let name = assoc coin currencieslist in
  let cap = Random.float 100000000000.0 in
  let value = Random.float 20000.0 in
  let (info : Yojson.Basic.json) =
    `Assoc [ ("short", `String coin)
           ; ("name", `String name)
           ; ("cap",   `Float cap)
           ; ("value", `Float value)
           ] in
  Yojson.Basic.pretty_to_string info


let buy req outchan =
  "not implemented yet"


let sell req outchan =
  "not implemented yet"

  
let dispatch pp pkeylist (req : Http_types.request) (outchan : out_channel) : unit =
  (* Forking server and OCaml doesn't re-init its RNG on fork *)
  Random.self_init ();

  (*
  let authh = req#header "Authentication" in
  let result = exists (fun pkey -> FaustAuthenticate.schnorrValidate pkey pp authh) pkeylist in 
   *)
  let result = true in

  let response =
    (match req#path with
     | "/currencies" -> currencies req outchan
     | "/info"       -> info req outchan
     | "/buy"        -> buy req outchan
     | "/sell"       -> sell req outchan
     | bad_request -> "Nope"
    ) in

  if result then
    Http_daemon.respond ~code:(`Code 200) ~body: response outchan
  else Http_daemon.respond ~code:(`Code 403) ~body: "Nope" outchan



let _ =
  let pkeylist = FaustAuthenticate.parseSchnorr "schnorr.json" in
  let pp = FaustAuthenticate.parsePublic "public.json" in

  let spec =
    { Http_daemon.default_spec with
      address = "127.0.0.66";
      callback = dispatch pp pkeylist;
      port = 9999;
    }
  in
  Http_daemon.main spec
