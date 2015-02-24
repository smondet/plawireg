
open Pvem
open Nonstd
module String = Sosa.Native_string


module Base = struct
  type t =  A | C | G | T | N
end
module Sequence = struct
  type t = Base.t array
end
module Pointer = struct
  type id = string
  type 'a t = {id : id}
end
module Node = struct
  type t = {
    kind: [ `Reference | `Db_snp of string | `Cosmic of string ];
    base : Sequence.t Pointer.t;
    next: t Pointer.t array;
  }
end
module Cache = struct
  type 'a t = (Pointer.id, 'a) Hashtbl.t
  let store: 'a t -> 'a -> 'a Pointer.t =
    fun _ -> assert false
  let get: 'a t -> 'a Pointer.t -> 'a =
    fun _ -> assert false
end
module Graph = struct
  type t = {
    sequences: Sequence.t Cache.t;
    nodes: Node.t Cache.t;
    roots: (string * string) list;
  }
  let load_reference: path:string -> t =
    assert false
  let add_vcf: t -> path:string -> unit =
    assert false
end
