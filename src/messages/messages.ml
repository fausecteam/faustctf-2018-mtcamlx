module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id)
module S = Sqlexpr
open Http_types
open List

let init_db db : unit =
  S.execute db sqlc"CREATE TABLE IF NOT EXISTS messages (\"from\" VARCHAR[100], \"to\" VARCHAR[100], message TEXT, time TIMESTAMP)"

let db = S.open_db "messages.sqlite"


let add_message req outchan : unit =
  try
    let fromuser = assoc "from" req#params_GET in
    let touser = assoc "to" req#params_GET in
    let message = assoc "message" req#params_GET in
    S.execute db sqlc"INSERT INTO messages (\"from\", \"to\", message, time) VALUES (%s, %s, %s, datetime('now'))" fromuser touser message;
    Http_daemon.respond ~code:(`Status (`Success `OK)) ~body: "OK" outchan
  with
    Not_found -> Http_daemon.respond ~code:(`Status (`Client_error `Bad_request)) ~body: "NOPE" outchan


let get_messages req outchan : unit =
  try
    let user = assoc "user" req#params_GET in
    let result = S.select db sqlc"SELECT @s{\"from\"}, @s{message}, @s{time} from messages where \"to\" = %s" user in
    let response = `List (map (fun (source, message, time) ->
                              `Assoc [ ("from", `String source) ;
                                       ("message", `String message) ;
                                       ("time", `String time) ])
                              result) in
    Http_daemon.respond ~code:(`Code 200) ~body: (Yojson.Basic.pretty_to_string response) outchan
  with
    Not_found -> Http_daemon.respond ~code:(`Status (`Client_error `Bad_request)) ~body: "NOPE" outchan


let dispatch pp pkeylist (req : Http_types.request) (outchan : out_channel) : unit =
  (* Forking server and OCaml doesn't re-init its RNG on fork *)
  Random.self_init ();
  let authh = req#header "Authentication" in
  let result = exists (fun pkey -> FaustAuthenticate.schnorrValidate pkey pp authh) pkeylist in

  if result then
    (match req#path with
     | "/post" -> add_message req outchan
     | "/get" -> get_messages req outchan
     | bad_request -> Http_daemon.respond ~code:(`Code 200) ~body: "Nope" outchan)
  else Http_daemon.respond ~code:(`Code 403) ~body: "Nope" outchan


let _ =
  init_db db;
  let pp = FaustAuthenticate.parsePublic "public.json" in
  let pkeylist = FaustAuthenticate.parseSchnorr "schnorr.json" in
  
  let spec =
    { Http_daemon.default_spec with
      address = "127.0.0.66";
      callback = dispatch pp pkeylist;
      port = 9998;
    }
  in
  Http_daemon.main spec
