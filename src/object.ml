module Scheme_version = struct
  type t = int

  let of_string = function
    | "1" -> Ok 1
    | invalid -> Error (Format.sprintf "invalid scheme version `%s`" invalid)

  let of_int = function
    | 1 -> Ok 1
    | invalid -> Error (Format.sprintf "invalid scheme version `%d`" invalid)

  let to_int = Fun.id

  let pp fmt v = Format.fprintf fmt "%d" v

  let default = 1
end

module Type = struct
  (** The kinds of objects represented by swhids, see the
      {{:https://docs.softwareheritage.org/devel/swh-model/data-model.html#software-artifacts}
      software heritage model documentation}. *)
  type t =
    | Content of string
        (** The string parameter is the hash function name used for the
            computation, defaults to "sha1_git" (in most cases, you don't care
            about it) *)
    | Directory
    | Release
    | Revision
    | Snapshot

  let of_string = function
    | "cnt" -> Ok (Content "sha1_git")
    | "dir" -> Ok Directory
    | "rel" -> Ok Release
    | "rev" -> Ok Revision
    | "snp" -> Ok Snapshot
    | invalid -> Error (Format.sprintf "invalid object type `%s`" invalid)

  let to_string = function
    | Content _f -> "cnt"
    | Directory -> "dir"
    | Release -> "rel"
    | Revision -> "rev"
    | Snapshot -> "snp"

  let pp fmt v = Format.fprintf fmt "%s" (to_string v)
end

module Hash = struct
  type t = string

  let of_string s =
    let len = ref 0 in
    try
      String.iter
        (function 'a' .. 'f' -> incr len | _invalid_char -> raise Exit)
        s;
      if !len = 40 then Ok s else raise Exit
    with Exit -> Error (Format.sprintf "invalid object hash `%s`" s)

  let to_string = Fun.id

  let pp fmt v = Format.fprintf fmt "%s" v
end

module Core_identifier = struct
  type t = Scheme_version.t * Type.t * Hash.t

  let of_string s =
    match String.split_on_char ':' s with
    | [ "swh"; "1"; t; hash ] -> begin
      match Type.of_string t with
      | Error _msg as e -> e
      | Ok t -> begin
        match Hash.of_string hash with
        | Error _msg as e -> e
        | Ok hash ->
          let scheme = Scheme_version.default in
          Ok (scheme, t, hash)
      end
    end
    | _whatever -> Error "invalid core identifier"

  let mk scheme typ hash = (scheme, typ, hash)

  let pp fmt (scheme, typ, hash) =
    Format.fprintf fmt "swh:%a:%a:%a" Scheme_version.pp scheme Type.pp typ
      Hash.pp hash

  let to_string v = Format.asprintf "%a" pp v

  let get_scheme (scheme, _typ, _hash) = scheme

  let get_type (_scheme, typ, _hash) = typ

  let get_hash (_scheme, _typ, hash) = hash
end

module Qualifier = struct
  (** See
      {{:https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html#qualifiers}
      swh documentation about qualifiers}.*)
  type t =
    | Anchor of Core_identifier.t
        (** a designated node in the Merkle DAG relative to which a path to the
            object is specified, as the core identifier of a directory, a
            revision, a release or a snapshot *)
    | Origin of string
        (** the software origin where an object has been found or observed in
            the wild, as an URI *)
    | Path of string
        (** the absolute file path, from the root directory associated to the
            anchor node, to the object; when the anchor denotes a directory or a
            revision, and almost always when it’s a release, the root directory
            is uniquely determined; when the anchor denotes a snapshot, the root
            directory is the one pointed to by HEAD (possibly indirectly), and
            undefined if such a reference is missing *)
    | Visit of Core_identifier.t
        (** the core identifier of a snapshot corresponding to a specific visit
            of a repository containing the designated object *)
    | Fragment of (int * int option)
        (** or a fragment (a line number or two) *)

  let of_string s =
    match String.split_on_char '=' s with
    | "lines" :: lines -> begin
      match String.split_on_char '-' (String.concat "" lines) with
      | [ l1 ] -> begin
        match int_of_string_opt l1 with
        | None -> Error "invalid qualifier"
        | Some i -> Ok (Fragment (i, None))
      end
      | [ l1; l2 ] -> begin
        match (int_of_string_opt l1, int_of_string_opt l2) with
        | Some i1, Some i2 -> Ok (Fragment (i1, Some i2))
        | _, _ -> Error "invalid qualifier"
      end
      | _whatever -> Error "invalid qualifier"
    end
    | "path" :: path ->
      (* TODO: check RFC 3987 IRI compliance *)
      let path = String.concat "" path in
      Ok (Path path)
    | "origin" :: url ->
      (* TODO: check RFC 3987 absolute path compliance *)
      let url = String.concat "" url in
      Ok (Origin url)
    | "visit" :: id -> (
      let id = String.concat "" id in
      match Core_identifier.of_string id with
      | Error _msg as e -> e
      | Ok id -> Ok (Visit id) )
    | "anchor" :: id -> (
      let id = String.concat "" id in
      match Core_identifier.of_string id with
      | Error _msg as e -> e
      | Ok id -> Ok (Anchor id) )
    | _whatever -> Error "invalid qualifier"

  let pp fmt = function
    | Anchor id -> Format.fprintf fmt "anchor=%a" Core_identifier.pp id
    | Origin uri -> Format.fprintf fmt "origin=%s" uri
    | Path path -> Format.fprintf fmt "path=%s" path
    | Visit id -> Format.fprintf fmt "visit=%a" Core_identifier.pp id
    | Fragment (l1, l2) ->
      Format.fprintf fmt "lines=%d" l1;
      Option.iter (fun l2 -> Format.fprintf fmt "-%d" l2) l2

  let to_string q = Format.asprintf "%a" pp q
end

(** The type for full swhids. *)
type t = Core_identifier.t * Qualifier.t list

let of_string s =
  match String.split_on_char ';' s with
  | id :: qualifiers -> begin
    match Core_identifier.of_string id with
    | Error _msg as e -> e
    | Ok object_core_identifier -> begin
      let qualifiers = List.map Qualifier.of_string qualifiers in
      match List.find_opt Result.is_error qualifiers with
      | Some (Error _msg as e) -> e
      | Some _ -> assert false
      | None ->
        let qualifiers = List.map Result.get_ok qualifiers in
        Ok (object_core_identifier, qualifiers)
    end
  end
  | _whatever -> Error "invalid swhid"

let mk object_core_identifier qualifiers = (object_core_identifier, qualifiers)

let get_core (core, _qualifiers) = core

let get_qualifiers (_core, qualifiers) = qualifiers

let pp_qualifiers fmt q = List.iter (Format.fprintf fmt ";%a" Qualifier.pp) q

let pp fmt id =
  let i = get_core id in
  let q = get_qualifiers id in
  Format.fprintf fmt "%a%a" Core_identifier.pp i pp_qualifiers q

let to_string id = Format.asprintf "%a" pp id
