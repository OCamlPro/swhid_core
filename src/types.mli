module Scheme_version : sig
  type t

  val of_string : string -> (t, string) Result.t

  val of_int : int -> (t, string) Result.t

  val to_int : t -> int

  val default : t
end

module Object_type : sig
  type t =
    | Content of string
    | Directory
    | Release
    | Revision
    | Snapshot

  val of_string : string -> (t, string) Result.t
end

module Object_hash : sig
  type t

  val of_string : string -> (t, string) Result.t

  val to_string : t -> string
end

module Object_core_identifier : sig
  type t

  val of_string : string -> (t, string) Result.t

  val mk : Scheme_version.t -> Object_type.t -> Object_hash.t -> t

  val to_string : t -> string

  val get_scheme : t -> Scheme_version.t

  val get_type : t -> Object_type.t

  val get_hash : t -> Object_hash.t
end

module Qualifier : sig
  type t =
    | Anchor of Object_core_identifier.t
    | Origin of string
    | Path of string
    | Visit of Object_core_identifier.t
    | Fragment of (int * int option)

  val of_string : string -> (t, string) Result.t
end

type t

val of_string : string -> (t, string) Result.t

val mk : Object_core_identifier.t -> Qualifier.t list -> t

val get_core : t -> Object_core_identifier.t

val get_qualifiers : t -> Qualifier.t list
