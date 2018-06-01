open Fieldslib

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

val parseServer : string -> server_keys
val parseClient : string -> client_keys
val parsePublic : string -> public_params
val parseSchnorr : string -> Z.t list

val schnorrId : client_keys -> public_params -> string
val schnorrValidate : Z.t -> public_params -> string -> bool
