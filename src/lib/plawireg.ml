
open Pvem
open Pvem_lwt_unix
open Deferred_result
open Nonstd
module String = Sosa.Native_string

let dbg fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
let failwithf fmt = ksprintf failwith fmt

module Exn = struct
  let to_string = Printexc.to_string
end
module Time = struct
  type t = float
  let now () : t = Unix.gettimeofday ()
  let to_filename f =
    let open Unix in
    let tm = gmtime f in
    sprintf "%04d-%02d-%02d-%02dh%02dm%02ds%03dms-UTC"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      (tm.tm_mday)
      (tm.tm_hour + 1)
      (tm.tm_min + 1)
      (tm.tm_sec)
      ((f -. (floor f)) *. 1000. |> int_of_float)
end


(** Provide pseudo-unique identifiers. *)
module Unique_id = struct
  type t = string
  (** Create a fresh filename-compliant identifier. *)
  let create () =
    sprintf "%s_%09d" Time.(now () |> to_filename) (Random.int 1_000_000_000)
end

module Base = struct
  type t =  A | C | G | T | N
  let of_char_exn = function
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | 'N' -> N
  | other -> failwithf "Invalid Base.of_char_exn: %c" other
  let to_char = function
  | A -> 'A'
  | C -> 'C'
  | G -> 'G'
  | T -> 'T'
  | N -> 'N'
end
module Sequence = struct
  type t = Base.t array
  let of_string_exn s =
    let a = Array.make (String.length s) Base.N in
    let _ =
      String.fold ~init:0 s
        ~f:(fun prev c -> Array.set a prev (Base.of_char_exn c); prev + 1)
    in
    a
  let to_string a =
    let s = Buffer.create 42 in
    Array.iter a ~f:(fun c -> Buffer.add_char s (Base.to_char c));
    Buffer.contents s

end
module Pointer = struct
  type id = Unique_id.t
  type 'a t = {id : id}
  let create id = {id}
end
module Node = struct
  type t = {
    id: Pointer.id;
    kind: [ `Reference | `Db_snp of string | `Cosmic of string ];
    base : Sequence.t Pointer.t;
    next: t Pointer.t array;
  }
  let pointer t = Pointer.create t.id
end
module Cache = struct
  type 'a t = (Pointer.id, 'a) Hashtbl.t
  let create () = Hashtbl.create 42
  let store: 'a t -> at:Pointer.id -> value:'a -> (unit, _) Deferred_result.t =
    fun cache ~at ~value ->
      Hashtbl.add cache at value;
      return ()
  let get: 'a t -> Pointer.id -> ('a, _) Deferred_result.t =
    fun cache id ->
      match Hashtbl.find  cache id with
      | some -> return some
      | exception Not_found -> fail (`Cache (`Not_found id))
  module Error = struct
    let to_string = function
    | `Not_found id ->
      sprintf "Data-not-found at: %S" id
  end
end
module Graph = struct
  type t = {
    sequences: Sequence.t Cache.t;
    nodes: Node.t Cache.t;
    mutable roots: (string * Node.t Pointer.t option) list;
  }
  let create () =
    let nodes = Cache.create () in
    let sequences = Cache.create () in
    return {sequences; nodes; roots = []}

  let fold_lines path ~on_exn ~init ~f : (_, _) Deferred_result.t  =
    let stream = Lwt_io.lines_of_file path in
    let rec loop prev = 
      wrap_deferred ~on_exn (fun () -> Lwt_stream.get stream)
      >>= begin function
      | Some s ->
        f prev s
        >>= fun next ->
        loop next
      | None -> return prev
      end
    in
    loop init

  let load_reference: t -> path:string -> (unit, _) Deferred_result.t =
    fun t ~path ->
      (* let next_id = ref (Unique_id.create ()) in *)
      (* let current_root = ref None in *)
      let node_of_line line =
        begin match Sequence.of_string_exn line with
        | s -> return s
        | exception e -> fail (`Graph (`Load_fasta (path, e)))
        end
        >>= fun sequence ->
        let sequence_id = Unique_id.create () in
        Cache.store t.sequences ~at:sequence_id ~value:sequence
        >>= fun () ->
        let node_id = Unique_id.create () in
        let node = {Node. id = node_id;
                    kind = `Reference;
                    base = {Pointer.id = sequence_id};
                    next = [|  |] } in
        return node in
      fold_lines path ~init:(`None) ~f:(fun prev line ->
          dbg "line: %S" line;
          match prev, line with
          | `None, line when String.get line 0 = Some '>' ->
            let root_name =
              String.(sub_exn line ~index:1 ~length:(length line - 1)) in
            return (`New_name root_name)
          | `None, l -> fail (`Graph (`Wrong_fasta (`First_line l)))
          | `New_name n, line when String.get line 0 = Some '>' ->
            (* empty fasta sequence *)
            t.roots  <- t.roots @ [(n, None)];
            let root_name =
              String.(sub_exn line ~index:1 ~length:(length line - 1)) in
            return (`New_name root_name)
          | `New_name n, line ->
            (* first node of the new root *)
            node_of_line line
            >>= fun node ->
            t.roots <- t.roots @ [n, Some (Node.pointer node)];
            return (`Node node)
          | `Node node, line when String.get line 0 = Some '>' ->
            Cache.store t.nodes ~at:node.Node.id ~value:node
            >>= fun () ->
            let root_name =
              String.(sub_exn line ~index:1 ~length:(length line - 1)) in
            return (`New_name root_name)
          | `Node node, line ->
            node_of_line line
            >>= fun (new_node) ->
            let to_store =
              {node with Node.next = [| Node.pointer new_node |] } in
            Cache.store t.nodes ~at:node.Node.id ~value:to_store
            >>= fun () ->
            return (`Node new_node)
        )
        ~on_exn:(fun e -> `Graph (`Load_fasta (path, e)))
      >>= function
      | `New_name name ->
        t.roots  <- t.roots @ [(name, None)]; return ()
      | `Node node ->
        Cache.store t.nodes ~at:node.Node.id ~value:node
      | `None -> return ()

  let add_vcf: t -> path:string -> unit =
    fun t ~path ->
      assert false

  let fold t ~init ~f =
    let rec visit_node_pointer to_fold node_p =
      Cache.get t.nodes node_p.Pointer.id
      >>= fun node ->
      Cache.get t.sequences node.Node.base.Pointer.id
      >>= fun seq ->
      f to_fold (`Node Node.(node.id, node.kind, seq))
      >>= fun next ->
      Array.fold_left node.Node.next ~init:(return next) ~f:(fun prev p ->
          prev >>= fun to_fold ->
          visit_node_pointer to_fold p)
    in
    List.fold ~init:(return init) t.roots ~f:(fun prev_m (name, node_p) ->
        prev_m >>= fun prev ->
        f prev (`Name name)
        >>= fun next ->
        match node_p with
        | None -> return prev
        | Some np -> visit_node_pointer next np)

  module Error = struct
    let to_string = function
    | `Load_fasta (file, e) ->
      sprintf "Load_fasta: %S: %s" file (Exn.to_string e)
    | `Wrong_fasta (`First_line l) ->
      sprintf "Wrong_fasta: first-line %S" l
  end
end
