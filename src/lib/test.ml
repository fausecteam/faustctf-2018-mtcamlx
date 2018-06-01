open OUnit2
open FaustAuthenticate

let public_json : public_params = {
    group = Z.of_string_base 16 "b34eb256adc3f6c5bda1a8b272522d30cf3882a9bc4fe6581be350b8c7c68ad513558bae0455fd30ac0b72f59fdd6c07f9cf80682d5ac8aab800e2074a7fdaaefe01bf96b46ba91615f2d94193a6b04ce4a5b902849de7987aebb143d4f5afa838219184134048902de30a8d49a675fee8e7043291d847584360876a8145b3fda6eb5a099ff8aa5e62c7949c5edd999874359f19ea169ad991fe565ba0323f472f3bdfaa611305ff3bc0654e93dbf5c04b27e4767114caaada101cf8aeb320917d12a88faeedb57a6424d4df5b2ec4964b43ef3b44c0b7a70ee37261c5ae69bd5b9f10b7449469ee201ee387ad486ee25aee41e3aa0068bc00efdd38057bc23b";
    generator = Z.of_string_base 16 "2";
    publickey = Z.of_string_base 16 "b2be3faa9150c604fe22374dad73a45dd900d5516bd474e5cfd8b2b5f98eab0ca3c07697856eb697b74cff4b8431ffb4cdb1f548a7e78d8287fdf7dcebad883028543780964a35cabf9da9accdf5bd0bef79c4a34e423352235beaf81f34c389b612220498f36b40abb5bb4681b1b3d744df1c3fddce5e9eb9b8b33e8117bce86755f1d5f97737c5a205ff5f0b81a0af9201c04a218697725c118af3739503a0a26ff14c7fc494f4738ae1406aa573d34df68bbdee6fc404d17730ff3e66ef1e9581cc37e3a42f4db6880ec45ffba25e648f6cbc4a88f8993396774591c51601740fe6ce8e83abdd80f55f3b60e361cf9c39de7b3a5f8674745f0f107d3d662f";
  }


let test_normalize ctx =
  let smallgroup = Z.of_int 17 in
  let (element : Z.t) = Z.of_int (-2) in
  let norm = FaustMath.normalize element smallgroup in
  assert_equal norm (Z.of_int 15)


let test_order_assumption ctx =
  let elem = Z.of_int 2 in
  let order = Z.((public_json.group - ~$1)) in
  let exp = Z.powm_sec elem order public_json.group in
  assert_equal Z.one exp


let test_schnorr_id_correct ctx =
  let secret : client_keys = {
      elgkey = FaustMath.randZ ()
    } in
  let idString = schnorrId secret public_json in
  let pkey = Z.powm_sec public_json.generator secret.elgkey public_json.group in
  assert_bool "schnorr ID doesnt verify" (schnorrValidate pkey public_json idString)


let suite =
  "suite">:::
    ["test_normalize"          >:: test_normalize;
     "test_order_assumption"   >:: test_order_assumption;
     "test_schnorr_id_correct" >:: test_schnorr_id_correct;
    ]


let () =
  run_test_tt_main suite
