
include Pvem
include Nonstd
module String = Sosa.Native_string

let failwithf fmt = ksprintf failwith fmt

module Exn = struct
  let to_string = Printexc.to_string
end

module Time = struct
  type t = float
  [@@deriving show, yojson]
end

module Unique_id = struct
  type t = Int64.t
  [@@deriving show, yojson]
  let to_string s = sprintf "0x%Lx" s

end

module Pointer: sig
  type id = Unique_id.t
  [@@deriving show, yojson]
  type 'a t
  [@@deriving show, yojson]
  val create: id -> 'a t
  val to_string: 'a t -> string
  val id: 'a t -> id
end = struct
  type id = Unique_id.t
  [@@deriving show, yojson]
  type 'a t = {id : id}
  [@@deriving show, yojson]
  let create id = {id}
  let to_string {id} = Unique_id.to_string id
  let id {id} = id
end

module Sequence: sig
  type t (* 1-based pseudo-strings *)
  [@@deriving show, yojson]
  val empty: t
  val of_string_exn: string -> t
  val to_string: t -> string
  val length: t -> int

  val split_exn: t -> before:int -> (t * t)
  (** In Vim: i^M :) *)
end = struct
  type t = string
  [@@deriving show, yojson]
  let empty = ""
  let of_string_exn s = s
  let to_string a = a
  let length = String.length
  let split_exn s ~before =
    (String.sub_exn s ~index:0 ~length:(before - 1),
     String.sub_exn s ~index:(before - 1) ~length:(String.length s - before +  1))
end

