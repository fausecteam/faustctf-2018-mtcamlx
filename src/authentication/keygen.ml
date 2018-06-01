open FaustAuthenticate

let group = Z.of_string_base 16 "b34eb256adc3f6c5bda1a8b272522d30cf3882a9bc4fe6581be350b8c7c68ad513558bae0455fd30ac0b72f59fdd6c07f9cf80682d5ac8aab800e2074a7fdaaefe01bf96b46ba91615f2d94193a6b04ce4a5b902849de7987aebb143d4f5afa838219184134048902de30a8d49a675fee8e7043291d847584360876a8145b3fda6eb5a099ff8aa5e62c7949c5edd999874359f19ea169ad991fe565ba0323f472f3bdfaa611305ff3bc0654e93dbf5c04b27e4767114caaada101cf8aeb320917d12a88faeedb57a6424d4df5b2ec4964b43ef3b44c0b7a70ee37261c5ae69bd5b9f10b7449469ee201ee387ad486ee25aee41e3aa0068bc00efdd38057bc23b"
let generator = Z.of_string_base 16 "2"


let publicParam oc =
  let sp = parseServer Sys.argv.(3) in

  let (publicJson : Yojson.Basic.json) =
    `Assoc [
       ("generator", `String (Z.format "%x" generator));
       ("group", `String (Z.format "%x" group));
       ("publickey", `String (Z.format "%x" (Z.powm_sec generator sp.elgkey group)))
     ] in
  output_string oc (Yojson.Basic.pretty_to_string publicJson)


let clientParam oc =
  let elgkey = FaustMath.randZ () in

  let (clientJson : Yojson.Basic.json) =
    `Assoc [
       ("elgkey", `String (Z.format "%x" elgkey));
     ] in
  output_string oc (Yojson.Basic.pretty_to_string clientJson)


let serverParam oc =
  let prfkey = FaustMath.randZ () in
  let elgkey = FaustMath.randZ () in

  let (serverJson : Yojson.Basic.json) =
    `Assoc [
       ("elgkey", `String (Z.format "%x" elgkey));
       ("prfkey", `String (Z.format "%x" prfkey))
     ] in
  output_string oc (Yojson.Basic.pretty_to_string serverJson)


let schnorrPublic oc =
  let k1 = parseClient Sys.argv.(3) in
  let k2 = parseClient Sys.argv.(4) in
  let k3 = parseClient Sys.argv.(5) in

  let (schnorrJson : Yojson.Basic.json) =
    `List [
       `String (Z.format "%x" (Z.powm_sec generator k1.elgkey group));
       `String (Z.format "%x" (Z.powm_sec generator k2.elgkey group));
       `String (Z.format "%x" (Z.powm_sec generator k3.elgkey group));
     ] in

  output_string oc (Yojson.Basic.pretty_to_string schnorrJson)


let _ =
  Random.self_init ();

  let oc = open_out Sys.argv.(2) in

  match Sys.argv.(1) with
  | "serverParam" -> serverParam oc
  | "publicParam" -> publicParam oc
  | "clientParam" -> clientParam oc
  | "schnorrPublic" -> schnorrPublic oc
