module Scheme_version : sig
  type t

  val of_string : string -> (t, string) Result.t

  val of_int : int -> (t, string) Result.t

  val to_int : t -> int

  val pp : Format.formatter -> t -> unit

  val default : t
end

module Type : sig
  type t =
    | Content of string
    | Directory
    | Release
    | Revision
    | Snapshot

  val of_string : string -> (t, string) Result.t

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

module Hash : sig
  type t

  val of_string : string -> (t, string) Result.t

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

module Core_identifier : sig
  type t

  val of_string : string -> (t, string) Result.t

  val mk : Scheme_version.t -> Type.t -> Hash.t -> t

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string

  val get_scheme : t -> Scheme_version.t

  val get_type : t -> Type.t

  val get_hash : t -> Hash.t
end

module Qualifier : sig
  type t =
    | Anchor of Core_identifier.t
    | Origin of string
    | Path of string
    | Visit of Core_identifier.t
    | Fragment of (int * int option)

  val of_string : string -> (t, string) Result.t

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

type t

val of_string : string -> (t, string) Result.t

val mk : Core_identifier.t -> Qualifier.t list -> t

val get_core : t -> Core_identifier.t

val get_scheme : t -> Scheme_version.t

val get_type : t -> Type.t

val get_hash : t -> Hash.t

val get_qualifiers : t -> Qualifier.t list

val pp : Format.formatter -> t -> unit

val to_string : t -> string
