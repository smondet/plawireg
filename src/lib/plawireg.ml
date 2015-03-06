
open Pvem
open Pvem_lwt_unix
open Deferred_result
open Nonstd
module String = Sosa.Native_string

let dbg fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
let is_babbling =
  try Sys.getenv "VERBOSE" = "true" with _ -> false
let babble fmt =
  ksprintf (fun s ->
      if is_babbling then dbg "%s" s else ()
    ) fmt

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
  let to_string {id} = Unique_id.to_string id
end
module Cache = struct
  type 'a t = (Pointer.id, 'a) Hashtbl.t
  let create () = Hashtbl.create 42
  let store: 'a t -> at:Pointer.id -> value:'a -> (unit, _) Deferred_result.t =
    fun cache ~at ~value ->
      Hashtbl.replace cache at value;
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
  val length: t -> int
end = struct
  type t = string
  let of_string_exn s = s
  let to_string a = a
  let length = String.length
end
module Node = struct
  type t = {
    id: Pointer.id;
    kind: [ `Reference | `Db_snp of string | `Cosmic of string ];
    sequence: Sequence.t Pointer.t;
    next: t Pointer.t array;
    prev: t Pointer.t array;
  }
  let pointer t = Pointer.create t.id
  let to_string { id; kind; sequence; next; prev } =
    let of_array name arr =
      match Array.to_list arr with
       | [] -> "no-" ^ name
       | a ->
         name ^ ":" ^ String.concat ~sep:"," (List.map ~f:Pointer.to_string a)
    in
    sprintf "{%s: %s (%s, %s)}"
      (Unique_id.to_string id)
      (match kind with
       | `Db_snp s -> sprintf "DB:%s" s
       | `Reference -> sprintf "Ref"
       | `Cosmic s -> sprintf "DB:%s" s)
      (of_array "next" next)
      (of_array "prev" prev)
end
module Variant = struct
  type t = {
    position: string * int;
    action: [
      | `Replace of Sequence.t * Sequence.t
      | `Delete of int
      | `Insert of Sequence.t
    ]}
  let create ~position action = {position; action}
  let replace ~at (ref, alt) =
    create ~position:at (`Replace (ref, alt))
  let insert ~at alt = create ~position:at (`Insert alt)
  let delete ~at nb = create ~position:at (`Delete nb)

  let to_string {position = (chr, pos); action} =
    match action with
    | `Replace (a, b) ->
      sprintf "%s:%ds/%S/%S" chr pos
        (Sequence.to_string a) (Sequence.to_string b)
    | `Insert (s) -> sprintf "%s:%di/%S" chr pos (Sequence.to_string s)
    | `Delete (nb) -> sprintf "%s:%dd%d" chr pos nb

  let of_vcf_row_exn row =
    let columns = String.split ~on:(`Character '\t') row in
    (* dbg "Row: %s" (String.concat ~sep:", " columns); *)
    let chromosome, position, name,
        reference, alt_l (* and that's all for now *) =
      let ios s =
        Int.of_string s
        |> Option.value_exn ~msg:(sprintf "not an int: %s" s) in
      match columns with
      | chrs :: poss :: name :: ref :: alt :: _ ->
        (chrs, ios poss, name, ref, String.split ~on:(`Character ',') alt)
      | other -> 
        failwithf "Not enough columns: %S" row
    in
    let variants =
      List.map alt_l ~f:(fun alt ->
          let common_prefix_length =
            let i = ref 0 in
            match String.find reference ~f:(fun c ->
                match String.get alt !i with
                | Some cc -> incr i; c <> cc
                | None -> true)
            with
            | Some index -> index
            | None -> String.length reference in
          let actual_ref =
            String.sub_exn reference ~index:common_prefix_length
              ~length:(String.length reference - common_prefix_length) in
          let actual_alt =
            String.sub_exn alt ~index:common_prefix_length
              ~length:(String.length alt - common_prefix_length) in
          let at = chromosome, position + common_prefix_length in
          babble "Variant: pos:%d common:%d %S → %S"
            position common_prefix_length actual_ref actual_alt;
          let fail () =
            failwithf "Unexpected variant ref: %s, alt: %s at %s:%d."
              reference alt chromosome position in
          match actual_ref, actual_alt with
          | "", "" -> fail ()
          | "", more -> insert ~at (Sequence.of_string_exn more)
          | more, "" -> delete ~at (String.length more)
          | _, _ ->
            replace ~at (Sequence.of_string_exn actual_ref,
                         Sequence.of_string_exn actual_alt)
        )
    in
    variants

end
module Graph = struct
  type root_key = {chromosome: string; comment: string}
  type t = {
    sequences: Sequence.t Cache.t;
    nodes: Node.t Cache.t;
    mutable roots: (root_key * Node.t Pointer.t option) list;
  }
  module Error = struct
    let rec to_string = function
    | `Load_fasta (file, line, e) ->
      sprintf "Load_fasta: %S:%d %s" file line (Exn.to_string e)
    | `Load_vcf (file, line, e) ->
      sprintf "Load_vcf: %S:%d %s" file line (Exn.to_string e)
    | `Wrong_fasta (`First_line (path, line, l)) ->
      sprintf "Wrong_fasta: %S:%d: first-line not '>' %S" path line l
    | `Node_not_found (chr, pos) ->
      sprintf "Node_not_found %s:%d" chr pos
    | `Graph e -> to_string e
    | `Cache e -> Cache.Error.to_string e
  end

  let create () =
    let nodes = Cache.create () in
    let sequences = Cache.create () in
    return {sequences; nodes; roots = []}

  let chromosome_names t =
    List.map t.roots ~f:(fun ({chromosome; _}, _) -> chromosome)

  let roots t = t.roots

  let stream_of_node_pointer t node_p =
    let stack_of_nodes = ref [node_p] in
    let visited_nodes = ref [] in
    (fun () ->
       let open Node in
       begin match !stack_of_nodes with
       | node_p :: more ->
         visited_nodes := node_p.Pointer.id :: !visited_nodes;
         Cache.get t.nodes node_p.Pointer.id
         >>= fun node ->
         (* inline implementation of a reverse-append-array-to-stack-list: *)
         stack_of_nodes := more;
         for i = Array.length node.next - 1 downto 0 do
           if List.mem node.next.(i).Pointer.id ~set:!visited_nodes
           then ()
           else stack_of_nodes := node.next.(i) :: !stack_of_nodes
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
    let rec loop line_number prev = 
      wrap_deferred ~on_exn:(on_exn ~line_number)
        (fun () -> Lwt_stream.get stream)
      >>= begin function
      | Some s ->
        (try f ~line_number prev s with e -> fail (on_exn ~line_number e))
        >>= fun next ->
        loop (line_number + 1) next
      | None -> return prev
      end
    in
    loop 1 init

  let store_new_sequence t ~sequence =
    let sequence_id = Unique_id.create () in
    Cache.store t.sequences ~at:sequence_id ~value:sequence
    >>= fun () ->
    return sequence_id

  let new_node t ~kind  ~sequence_id ~next ~prev =
    let node_id = Unique_id.create () in
    let node = {
      Node. id = node_id;
      kind = `Reference;
      sequence = {Pointer.id = sequence_id};
      next;
      prev;
    } in
    (node)

  let load_reference: t -> path:string -> (unit, _) Deferred_result.t =
    fun t ~path ->
      (* let next_id = ref (Unique_id.create ()) in *)
      (* let current_root = ref None in *)
      let node_of_line ~line_number ?parent line =
        begin match Sequence.of_string_exn line with
        | s -> return s
        | exception e -> fail (`Graph (`Load_fasta (path, line_number, e)))
        end
        >>= fun sequence ->
        store_new_sequence t ~sequence
        >>= fun sequence_id ->
        let prev =
          Option.value_map ~default:[| |] parent ~f:(fun e -> [| e|]) in
        return (new_node t ~kind:`Reference ~sequence_id ~next:[| |] ~prev)
      in
      let root_key_of_line line =
        match
          line |> String.split ~on:(`Character ' ')
          |> List.filter_map
            ~f:(fun s -> match String.strip s with "" -> None | st -> Some st)
        with
        | chromosome :: more :: than_one ->
          {chromosome; comment = String.concat ~sep:" " (more :: than_one)}
        | other -> failwithf "Cannot find chromosome name: %S" line
      in
      fold_lines path ~init:(`None) ~f:(fun ~line_number prev line ->
          (* dbg "line: %S" line; *)
          match prev, line with
          | `None, line when String.get line 0 = Some '>' ->
            let root_name =
              String.(sub_exn line ~index:1 ~length:(length line - 1)) in
            return (`New_name root_name)
          | `None, l ->
            fail (`Graph (`Wrong_fasta (`First_line (path, line_number, l))))
          | `New_name n, line when String.get line 0 = Some '>' ->
            (* empty fasta sequence *)
            t.roots  <- t.roots @ [(root_key_of_line n, None)];
            let root_name =
              String.(sub_exn line ~index:1 ~length:(length line - 1)) in
            return (`New_name root_name)
          | `New_name n, line ->
            (* first node of the new root *)
            node_of_line ~line_number ?parent:None line
            >>= fun node ->
            t.roots <- t.roots @ [root_key_of_line n, Some (Node.pointer node)];
            return (`Node node)
          | `Node node, line when String.get line 0 = Some '>' ->
            Cache.store t.nodes ~at:node.Node.id ~value:node
            >>= fun () ->
            let root_name =
              String.(sub_exn line ~index:1 ~length:(length line - 1)) in
            return (`New_name root_name)
          | `Node node, line ->
            node_of_line ~line_number ~parent:(Node.pointer node) line
            >>= fun (new_node) ->
            let to_store =
              {node with Node.next = [| Node.pointer new_node |] } in
            Cache.store t.nodes ~at:node.Node.id ~value:to_store
            >>= fun () ->
            return (`Node new_node)
        )
        ~on_exn:(fun ~line_number e ->
            `Graph (`Load_fasta (path, line_number, e)))
      >>= function
      | `New_name name ->
        t.roots  <- t.roots @ [(root_key_of_line name, None)]; return ()
      | `Node node ->
        Cache.store t.nodes ~at:node.Node.id ~value:node
      | `None -> return ()

  let find_reference_node t ~chromosome ~position =
    (* very stupid implementation,
       and later we'll optimize with the right "addressing" *)
    begin match List.find t.roots (fun (r, _) -> r.chromosome = chromosome) with
    | None -> return None
    | Some (_, None) -> return None
    | Some (_, Some node_p) ->
      let stream = stream_of_node_pointer t node_p in
      let rec find_stupidly current_position =
        stream ()
        >>= begin function
        | None -> return None
        | Some node when node.Node.kind <> `Reference ->
          (* if not in the reference “path” we don't adavance *)
          find_stupidly current_position
        | Some node ->
          Cache.get t.sequences node.Node.sequence.Pointer.id
          >>= fun seq ->
          let lgth = Sequence.length seq in
          if current_position + lgth - 1 >=  position
          then return (Some (node, position - current_position + 1, seq))
          else find_stupidly (current_position + lgth)
        end
      in
      find_stupidly 1
    end
    >>= function
    | Some n -> return n
    | None -> fail (`Graph (`Node_not_found (chromosome, position)))

  let insert_node t ~split ~at ~new_path ~kind =
    match at with
    | 1 ->
      (*
         - add new child
         - find node corresponding the end of new_path
         - reonnect end of the new path to that one
           (potentially insert another node)
      *)
      return ()
    | _ ->
      (* change sequence of split
      *)
      return ()

  let get_parent_nodes t ~node =
    Array.fold_left ~init:(return []) node.Node.prev ~f:(fun l_m pointer ->
        l_m >>= fun l ->
        Cache.get t.nodes (pointer.Pointer.id)
        >>= fun parent ->
        return (parent :: l))

  let integrate_variant t  ~variant =
    (* TODO *)
    let chromosome, position = variant.Variant.position in
    begin match variant.Variant.action with
    | `Insert ins ->
      (* find node, split there if needed, insert new path *)
      find_reference_node t ~chromosome ~position
      >>= fun  (node, index, seq) ->
      babble "Integrating in %s (+%d %S)"
        (Node.to_string node) index (Sequence.to_string seq);
      begin match index with
      | 1 ->
        (* insert from there:
           - find parents of node
           - parents.next += new_node
           - new_node.parent = parents
           - node.parents += new_node
           - new_node.next = node
        *)
        get_parent_nodes t ~node
        >>= fun parents ->
        store_new_sequence t ~sequence:ins
        >>= fun sequence_id ->
        let new_node =
          let next = [| Node.pointer node |] in
          let prev = Array.copy node.Node.prev in
          new_node t ~kind:(`Db_snp "dbsnp-todo") ~sequence_id ~next ~prev
        in
        babble "New node: %s" (Node.to_string new_node);
        let open Node in
        Cache.store t.nodes ~at:new_node.id ~value:new_node
        >>= fun () ->
        List.fold parents ~init:(return ()) ~f:(fun unitm parent ->
            unitm
            >>= fun () ->
            let value =
              {parent with
               next = Array.concat [parent.next; [| pointer new_node |] ]}
            in
            babble "Updated parent: %s" (Node.to_string value);
            Cache.store t.nodes ~at:parent.id ~value)
        >>= fun () ->
        Cache.store t.nodes ~at:node.id
          ~value:
            {node with prev = Array.concat [node.prev; [| pointer new_node |] ]}
      | _ ->
        return ()
      end
    | `Delete _
    | `Replace _ ->
      return ()
    end


  let add_vcf: t -> path:string -> (unit, _) Deferred_result.t =
    fun t ~path ->
      let temp = ref [] in
      fold_lines path ~init:() ~f:begin fun ~line_number () line ->
        match line with
        | comment when String.get comment ~index:0 = Some '#' ->
          dbg "Comment: %s" comment;
          return ()
        | row ->
          let variants = Variant.of_vcf_row_exn row in
          temp := variants :: !temp;
          Deferred_list.for_concurrent variants ~f:(fun variant ->
              babble "Variant: %s" (Variant.to_string variant);
              if line_number mod 10_000 = 0 then
                dbg "[%s] %s:%d Variant: %s"
                  Time.(now () |> to_filename) path line_number
                  (Variant.to_string variant);
              integrate_variant t ~variant)
          >>= fun ((_ : unit list), errors) ->
          begin match errors with
          | [] -> return ()
          | e ->
            dbg "Errors!:\n    - %s"
              (List.map e ~f:Error.to_string |> String.concat ~sep:"\n    -");
            return ()
          end
      end
        ~on_exn:(fun ~line_number e ->
            `Graph (`Load_vcf (path, line_number, e)))
      >>= fun () ->
      let snv, ins, del, repl =
        List.fold !temp ~init:(0,0,0,0) ~f:(fun init variants ->
            List.fold variants ~init ~f:(fun (snv, ins, del, repl) v ->
                match v.Variant.action with
                | `Replace (a, b)
                  when Sequence.length a = 1 && Sequence.length b = 1 ->
                  (snv + 1, ins, del, repl)
                | `Replace _ -> (snv, ins, del, repl + 1)
                | `Insert _ -> (snv, ins + 1, del, repl)
                | `Delete _ -> (snv, ins, del + 1, repl)))
      in
      dbg "Variants: %d “lines”, %d snvs, %d ins, %d dels, %d repls"
        (List.length !temp) snv ins del repl;
      return ()

end
