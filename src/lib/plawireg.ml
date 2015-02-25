
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
module Unique_id : sig
  type t
  val create: unit -> t
  val to_string: t -> string
  val test: unit -> float
end = struct
  type t = Int64.t

  let circular_int = ref 0L
  let create () =
    (* we take a piece of 
       the mantissa (bits 51 to 0) of the current time 
       and stick an increasing number to its right *)
    let now = Unix.gettimeofday () in
    let shift_size = 24 in
    Int64.(
      let to_add = !circular_int in
      let () =
        circular_int :=
          rem (add !circular_int 1L) (shift_left 1L (shift_size + 1)) in
      add
        (shift_left (bits_of_float now) shift_size)
        to_add
    )

  let to_string s = sprintf "0x%Lx" s

  let test () =
    circular_int := 0L;
    let start = Unix.gettimeofday () in
    let first = create () in
    while !circular_int <> 0L do
      if create () = first then
        failwithf "got %s again" (to_string first)
      else
        ()
    done;
    let the_end = Unix.gettimeofday () in
    (the_end -. start)
end
module Pointer = struct
  type id = Unique_id.t
  type 'a t = {id : id}
  let create id = {id}
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
      sprintf "Data-not-found at: %S" (Unique_id.to_string id)
  end
end

module Sequence: sig
  type t
  val of_string_exn: string -> t
  val to_string: t -> string
end = struct
  type t = string
  let of_string_exn s = s
  let to_string a = a
end
module Node = struct
  type t = {
    id: Pointer.id;
    kind: [ `Reference | `Db_snp of string | `Cosmic of string ];
    sequence: Sequence.t Pointer.t;
    next: t Pointer.t array;
    parent: t Pointer.t option;
  }
  let pointer t = Pointer.create t.id
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

  let root_names t =
    List.map t.roots ~f:fst

  let roots t = t.roots

  let stream_of_node_pointer t node_p =
    let stack_of_nodes = ref [node_p] in
    (fun () ->
       let open Node in
       begin match !stack_of_nodes with
       | node_p :: more ->
         Cache.get t.nodes node_p.Pointer.id
         >>= fun node ->
         (* inline implementation of a reverse-append-array-to-stack-list: *)
         stack_of_nodes := more;
         for i = Array.length node.next - 1 downto 0 do
           stack_of_nodes := node.next.(i) :: !stack_of_nodes
         done;
         return (Some node)
       | [] -> return None
       end)

  let stream_of_root t ~name = 
    match List.find t.roots ~f:(fun (n, _) -> n = name) with
    | None
    | Some (_, None) -> (fun () -> return None)
    | Some (_, Some node_p) ->
      stream_of_node_pointer t node_p

  let count_nodes t =
    List.fold ~init:(return []) t.roots ~f:(fun prev root ->
        prev >>= fun prev_list ->
        match root with
        | (name, None) -> return ((name, 0) :: prev_list)
        | (name, Some np) -> 
          let stream = stream_of_node_pointer t np in
          let c = ref 0 in
          let rec count () =
            stream () >>= function
            | Some node -> incr c; count ()
            | None -> return ()
          in
          count ()
          >>= fun () ->
          return ((name, !c) :: prev_list))

  let fold t ~init ~f =
    List.fold ~init:(return init) t.roots ~f:(fun prev root ->
        prev >>= fun prev ->
        match root with
        | (name, None) -> f prev (`Name name)
        | (name, Some np) -> 
          f prev (`Name name)
          >>= fun next ->
          let stream = stream_of_node_pointer t np in
          let c = ref 0 in
          let rec go current =
            stream () >>= function
            | Some node ->
              f current (`Node node)
              >>= fun next ->
              go next
            | None -> return current
          in
          go next)

  let expand_node t ~node =
    let open Node in
    Cache.get t.sequences node.sequence.Pointer.id
    >>= fun sequence ->
    return (node.id, node.kind, sequence)

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
      let node_of_line ?parent line =
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
                    sequence = {Pointer.id = sequence_id};
                    next = [|  |];
                    parent;} in
        return node in
      fold_lines path ~init:(`None) ~f:(fun prev line ->
          (* dbg "line: %S" line; *)
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
            node_of_line ?parent:None line
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
            node_of_line ~parent:(Node.pointer node) line
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

  module Error = struct
    let to_string = function
    | `Load_fasta (file, e) ->
      sprintf "Load_fasta: %S: %s" file (Exn.to_string e)
    | `Wrong_fasta (`First_line l) ->
      sprintf "Wrong_fasta: first-line %S" l
  end
end
