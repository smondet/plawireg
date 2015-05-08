
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
  [@@deriving show, yojson]
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
  [@@deriving show, yojson]
  val create: unit -> t
  val to_string: t -> string
  val test: unit -> float
end = struct
  type t = Int64.t
  [@@deriving show, yojson]

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
module Variant = struct
  type t = {
    name: string;
    position: string * int;
    action: [
      | `Replace of Sequence.t * Sequence.t
      | `Delete of int
      | `Insert of Sequence.t
    ];
  }
  [@@deriving show, yojson]
  let create ~name ~position action = {name; position; action}
  let replace ~name ~at (ref, alt) =
    create ~name ~position:at (`Replace (ref, alt))
  let insert ~name ~at alt = create ~name ~position:at (`Insert alt)
  let delete ~name ~at nb = create ~name ~position:at (`Delete nb)

  let to_string {name; position = (chr, pos); action} =
    sprintf "{%s:%s@%s:%d}" name
      (match action with
      | `Replace (a, b) ->
        sprintf "s/%s/%s" (Sequence.to_string a) (Sequence.to_string b)
      | `Insert (s) -> sprintf "i/%s" (Sequence.to_string s)
      | `Delete (nb) -> sprintf "%dx" nb)
      chr pos

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
          | "", more -> insert ~name ~at (Sequence.of_string_exn more)
          | more, "" -> delete ~name ~at (String.length more)
          | _, _ ->
            replace ~name ~at (Sequence.of_string_exn actual_ref,
                               Sequence.of_string_exn actual_alt)
        )
    in
    variants

end

module Node = struct
  type t = {
    id: Pointer.id;
    kind: [ `Reference | `Db_snp of Variant.t | `Cosmic of string ];
    sequence: Sequence.t Pointer.t;
    next: t Pointer.t array;
    prev: t Pointer.t array;
  }
  [@@deriving yojson]
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
       | `Db_snp s -> sprintf "DB:%s" (Variant.to_string s)
       | `Reference -> sprintf "Ref"
       | `Cosmic s -> sprintf "DB:%s" s)
      (of_array "next" next)
      (of_array "prev" prev)
end
module Graph = struct
  type root_key = {chromosome: string; comment: string}
  [@@deriving show, yojson]
  type t = {
    sequences: Sequence.t Cache.t;
    nodes: Node.t Cache.t;
    mutable roots: (root_key * Node.t Pointer.t) list;
    empty_sequence_id: Sequence.t Pointer.t;
  }
  module Error = struct
    let rec to_string = function
    | `Load_fasta (file, line, e) ->
      sprintf "Load_fasta: %S:%d %s" file line (Exn.to_string e)
    | `Load_vcf (file, line, e) ->
      sprintf "Load_vcf: %S:%d %s" file line (Exn.to_string e)
    | `Wrong_fasta (`First_line (path, line, l)) ->
      sprintf "Wrong_fasta: %S:%d: first-line not '>' %S" path line l
    | `Node_not_found (`Global_position (chr, pos)) ->
      sprintf "Node_not_found %s:%d" chr pos
    | `Graph e -> to_string e
    | `Cache e -> Cache.Error.to_string e
  end

  let create () =
    let nodes = Cache.create () in
    let sequences = Cache.create () in
    let sequence_id = Unique_id.create () in
    Cache.store sequences ~at:sequence_id ~value:Sequence.empty
    >>= fun () ->
    return {sequences; nodes; roots = [];
            empty_sequence_id = Pointer.create sequence_id}

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
         visited_nodes := Pointer.id node_p :: !visited_nodes;
         Cache.get t.nodes (Pointer.id node_p)
         >>= fun node ->
         (* inline implementation of a reverse-append-array-to-stack-list: *)
         stack_of_nodes := more;
         for i = Array.length node.next - 1 downto 0 do
           if List.mem (Pointer.id node.next.(i)) ~set:!visited_nodes
           then ()
           else stack_of_nodes := node.next.(i) :: !stack_of_nodes
         done;
         return (Some node)
       | [] -> return None
       end)

  let stream_of_root t ~name = 
    match List.find t.roots ~f:(fun (n, _) -> n = name) with
    | None -> (fun () -> return None)
    | Some (_, node_p) ->
      stream_of_node_pointer t node_p

  let count_nodes t =
    List.fold ~init:(return []) t.roots ~f:(fun prev (name, np) ->
        prev >>= fun prev_list ->
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
    List.fold ~init:(return init) t.roots ~f:(fun prev (name, np) ->
        prev >>= fun prev ->
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
    Cache.get t.sequences (Pointer.id node.sequence)
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
    if Sequence.length sequence = 0
    then return t.empty_sequence_id
    else begin
      let sequence_id = Unique_id.create () in
      Cache.store t.sequences ~at:sequence_id ~value:sequence
      >>= fun () ->
      return (Pointer.create sequence_id)
    end

  let new_node t ~kind  ~sequence ~next ~prev =
    let node_id = Unique_id.create () in
    let node = {
      Node. id = node_id;
      kind; sequence; next; prev;} in
    (node)

  let add_or_update_node t node =
    Cache.store t.nodes ~at:node.Node.id ~value:node

  let load_reference: t -> path:string -> (unit, _) Deferred_result.t =
    fun t ~path ->
      (* let next_id = ref (Unique_id.create ()) in *)
      (* let current_root = ref None in *)
      let node_of_line ~line_number ~parent line =
        begin match Sequence.of_string_exn line with
        | s -> return s
        | exception e -> fail (`Graph (`Load_fasta (path, line_number, e)))
        end
        >>= fun sequence ->
        store_new_sequence t ~sequence
        >>= fun sequence_pointer ->
        let prev = [| parent |] in
        return (new_node t ~kind:`Reference
                  ~sequence:sequence_pointer ~next:[| |] ~prev)
      in
      let root_key_of_line line =
        let with_out_carret =
          String.(sub_exn line ~index:1 ~length:(length line - 1)) in
        match
          with_out_carret |> String.split ~on:(`Character ' ')
          |> List.filter_map
            ~f:(fun s -> match String.strip s with "" -> None | st -> Some st)
        with
        | chromosome :: more :: than_one ->
          {chromosome; comment = String.concat ~sep:" " (more :: than_one)}
        | other -> failwithf "Cannot find chromosome name: %S" line
      in
      let add_root line =
        store_new_sequence t Sequence.empty
        >>= fun sequence ->
        let node =
          new_node t ~kind:`Reference ~sequence ~next:[| |] ~prev:[| |] in
        t.roots  <- t.roots @ [(root_key_of_line line, Node.pointer node)];
        return (`Node node)
      in
      let add_last_node node =
        store_new_sequence t Sequence.empty
        >>= fun sequence ->
        let last_node =
          new_node t ~kind:`Reference ~sequence ~next:[| |]
            ~prev:[| Node.pointer node |]
        in
        add_or_update_node t last_node
        >>= fun () ->
        babble "Last node: %s" (Node.to_string last_node);
        add_or_update_node t {node with Node.next = [| Node.pointer last_node |]}
      in
      fold_lines path ~init:(`None) ~f:(fun ~line_number prev line ->
          match prev, line with
          | `None, line when String.get line 0 = Some '>' ->
            add_root line
          | `None, l ->
            fail (`Graph (`Wrong_fasta (`First_line (path, line_number, l))))
          | `Node node, line when String.get line 0 = Some '>' ->
            add_last_node node
            >>= fun () ->
            add_root line
          | `Node node, line ->
            node_of_line ~line_number ~parent:(Node.pointer node) line
            >>= fun (new_node) ->
            add_or_update_node t
              {node with Node.next = [| Node.pointer new_node |] }
            >>= fun () ->
            return (`Node new_node)
        )
        ~on_exn:(fun ~line_number e ->
            `Graph (`Load_fasta (path, line_number, e)))
      >>= function
      | `Node node -> add_last_node node
      | `None -> (* empty file ⇒ no roots *) return ()

  let find_reference_node t ~from =
    let fail e = fail (`Graph e) in
    begin match from with
    | `Global_position (chromosome, position) ->
      (* very stupid implementation,
         and later we'll optimize with the right "addressing" *)
      begin match List.find t.roots (fun (r, _) -> r.chromosome = chromosome) with
      | None -> fail (`Node_not_found from)
      | Some (_, node_p) ->
        return (node_p, position)
      end
    end
    >>= fun (starting_node, position) ->
    begin
      let stream = stream_of_node_pointer t starting_node in
      let rec find_stupidly current_position =
        stream ()
        >>= begin function
        | None ->
          dbg "can't find_stupidly: at %d" current_position;
          return None
        | Some node when node.Node.kind <> `Reference ->
          (* if not in the reference “path” we don't adavance *)
          find_stupidly current_position
        | Some node when Array.length node.Node.next = 0 ->
          (* last node *)
          return (Some (node, 1, Sequence.empty))
        | Some node ->
          Cache.get t.sequences (Pointer.id node.Node.sequence)
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
    | None -> fail (`Node_not_found from)

  let get_parents t ~node how =
    Array.fold_left ~init:(return []) node.Node.prev ~f:(fun l_m pointer ->
        l_m >>= fun l ->
        Cache.get t.nodes (Pointer.id pointer)
        >>= fun parent ->
        begin match parent.Node.kind with
        | `Db_snp _
        | `Cosmic _ ->
          return (match how with `Only_reference -> l | `All -> parent :: l)
        | `Reference ->
          return (parent :: l)
        end)
      (*
    >>= function
    | l -> return l
    | [] -> return None
    | s :: [] -> return (Some s)
    | more -> failwithf "Node %s has too many reference-parents: [%s]!"
                (Node.to_string node)
                (List.map more ~f:Node.to_string |> String.concat ~sep:", ")
         *)

  let insert_node_before_reference t ~reference_node ~sequence ~variant =
    (* insert from there:
       - find reference-parent of reference_node
       - parents.next += new_node
       - new_node.parent = parents
       - reference_node.parents += new_node
       - new_node.next = reference_node
    *)
    let open Node in
    get_parents t ~node:reference_node `Only_reference
    >>= fun parents ->
    store_new_sequence t ~sequence
    >>= fun sequence ->
    let new_node =
      let next = [| Node.pointer reference_node |] in
      let prev =
        List.map parents ~f:pointer |> Array.of_list in
      new_node t ~kind:(`Db_snp variant) ~sequence ~next ~prev
    in
    babble "New node: %s\n   goes before %s"
      (Node.to_string new_node) (Node.to_string reference_node);
    add_or_update_node t new_node
    >>= fun () ->
    List.fold parents ~init:(return ()) ~f:(fun prev parent ->
        prev >>= fun () ->
        let new_parent =
          {parent with
           next = Array.concat [parent.next; [| pointer new_node |] ]}
        in
        babble "Updated parent: %s" (Node.to_string new_parent);
        add_or_update_node t new_parent)
    >>= fun () ->
    add_or_update_node t 
      {reference_node with
       prev = Array.concat [reference_node.prev; [| pointer new_node |] ]}

  let split_reference_node t ~reference_node ~reference_sequence ~index =
        (*
           - split the reference_node in two
               - new_node: seq: after, prev: ref_node, child: = refnode.children
               - reference_node.seq = prefix
               - replace prev in child of reference_node: (prev 
        *)
    let before, after = Sequence.split_exn reference_sequence ~before:index in
    store_new_sequence t ~sequence:before
    >>= fun before_sequence ->
    store_new_sequence t ~sequence:after
    >>= fun after_sequence ->
    let new_ref_node =
      let next = Array.copy reference_node.Node.next in
      let prev = [| Node.pointer reference_node |] (* + ins_node *) in
      new_node t ~kind:`Reference ~sequence:after_sequence ~next ~prev
    in
    let open Node in
    babble "split_reference_node: %s (%s / %s) into\n    %s"
      (Node.to_string reference_node)
      (Sequence.to_string before) (Sequence.to_string after)
      (Node.to_string new_ref_node);
    add_or_update_node t new_ref_node
    >>= fun () ->
    (* Delete old sequence? *)
    add_or_update_node t 
      {reference_node with
       sequence = before_sequence;
       next = [| Node.pointer new_ref_node |]}
    >>= fun () ->
    Array.fold_left reference_node.next ~init:(return ())
      ~f:(fun unitm child ->
          unitm >>= fun () ->
          Cache.get t.nodes (Pointer.id child)
          >>= fun child_node ->
          let prev =
            Array.map child_node.prev ~f:(fun pointer ->
                if Pointer.id pointer = reference_node.id
                then Pointer.create new_ref_node.id
                else pointer) in
          add_or_update_node t {child_node with prev})
    >>= fun () ->
    return new_ref_node

  let add_path_to_reference t
      ~chromosome ~fork_position ~join_position ~sequence ~kind =
    begin (* find/create "forking" node *)
      find_reference_node t ~from:(`Global_position (chromosome, fork_position))
      >>= fun  (reference_node, index, reference_sequence) ->
      babble "Shortcut starts at in %s (+%d %S)"
        (Node.to_string reference_node) index (Sequence.to_string reference_sequence);
      begin match index with
      | 1 ->
        return reference_node
      | _ ->
        split_reference_node t ~reference_node ~index ~reference_sequence
      end
      >>= fun new_left_ref_node ->
      get_parents t ~node:new_left_ref_node `All
      (* >>| Option.value_exn ~msg:(sprintf "new_left_ref_node has no parent?") *)
    end
    >>= fun forking_nodes ->
    begin (* find/create "joining" node *)
      find_reference_node t ~from:(`Global_position (chromosome, join_position))
      >>= fun  (reference_node, index, reference_sequence) ->
      babble "Shortcut joins at in %s (+%d %S)"
        (Node.to_string reference_node) index (Sequence.to_string reference_sequence);
      begin match index with
      | 1 -> return reference_node
      | _ ->
        split_reference_node t ~reference_node ~index ~reference_sequence
        >>= fun new_right_ref_node ->
        return new_right_ref_node
      end
    end
    >>= fun joining_node ->
    store_new_sequence t ~sequence
    >>= fun sequence ->
    let open Node in
    let new_variant_node =
      let next = [| pointer joining_node |] in
      let prev = List.map forking_nodes ~f:pointer |> Array.of_list in
      new_node t ~kind ~sequence ~next ~prev
    in
    add_or_update_node t new_variant_node
    >>= fun () ->
    babble "New variant node: %s" (Node.to_string new_variant_node);
      (*
         - forking_node.next += new_node
         - joining_node.prev += new_node
      *)
    List.fold forking_nodes ~init:(return ()) ~f:(fun prev forking_node ->
        prev >>= fun () ->
        add_or_update_node t {
          forking_node with
          next = Array.append forking_node.next [| pointer new_variant_node |]})
    >>= fun () ->
    add_or_update_node t {
      joining_node with
      prev = Array.append joining_node.prev [| pointer new_variant_node |]}

  let integrate_variant t  ~variant =
    let chromosome, position = variant.Variant.position in
    begin match variant.Variant.action with
    | `Insert ins ->
      (* find node, split there if needed, insert new path *)
      find_reference_node t ~from:(`Global_position (chromosome, position))
      >>= fun  (reference_node, index, seq) ->
      babble "Integrating in %s (+%d %S)"
        (Node.to_string reference_node) index (Sequence.to_string seq);
      begin match index with
      | 1 ->
        insert_node_before_reference t ~reference_node ~sequence:ins ~variant
      | _ ->
        split_reference_node t ~reference_node ~index
          ~reference_sequence:seq
        >>= fun new_ref_node ->
        insert_node_before_reference
          t ~reference_node:new_ref_node ~sequence:ins ~variant
        >>= fun () ->
        return ()
      end
    | `Delete nb_of_bps ->
      add_path_to_reference t ~chromosome
        ~fork_position:position ~join_position:(position + nb_of_bps)
        ~sequence:Sequence.empty ~kind:(`Db_snp variant)
    | `Replace (to_replace, sequence) ->
      let join_position = position + Sequence.length to_replace in
      add_path_to_reference t ~chromosome
        ~fork_position:position ~join_position
        ~sequence ~kind:(`Db_snp variant)
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
              if line_number mod 1_000 = 0 then
                dbg "[%s] %s:%d Variant: %s"
                  Time.(now () |> to_filename) path line_number
                  (Variant.to_string variant);
              integrate_variant t ~variant)
          >>= fun ((_ : unit list), errors) ->
          begin match errors with
          | [] -> return ()
          | e ->
            failwithf "Errors!:\n    - %s"
              (List.map e ~f:Error.to_string |> String.concat ~sep:"\n    -")
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
