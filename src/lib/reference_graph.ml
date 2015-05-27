open Internal_pervasives
    



module Node = struct
  type t = {
    id: Pointer.id;
    kind: [ `Reference | `Db_snp of Variant.t | `Cosmic of string ];
    sequence: Sequence.t;
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
    (* sequences: Sequence.t Cache.t; *)
    nodes: Node.t Cache.t;
    mutable roots: (root_key * Node.t Pointer.t) list;
    (* empty_sequence_id: Sequence.t Pointer.t; *)
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
    return {nodes; roots = [];}

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
    return (node.id, node.kind, node.sequence)


  let new_node t ~kind  ~sequence ~next ~prev =
    let node_id = Unique_id.create () in
    let node = {
      Node. id = node_id;
      kind; sequence; next; prev;} in
    (node)

  let add_or_update_node t node =
    Cache.store t.nodes ~at:node.Node.id ~value:node


  let load_reference:
    ?buffer_size: int ->
    ?packetization_threshold:int ->
    ?region:Linear_genome.Region.t ->
    t ->
    path:string -> (unit, _) Deferred_result.t =
    fun ?buffer_size ?(packetization_threshold = 200) ?(region = `Everything) t ~path ->
      let state =
        object (self)
          val mutable current_node = None
          val mutable current_sequence = []
          method flush_sequence =
            begin match current_node with
            | None -> return ()
            | Some node ->
              let sequence = (String.concat ~sep:"" current_sequence) in
              current_sequence <- [];
              babble "finalize: updating node : %s with seq: %s"
                (Node.to_string node) (sequence);
              let updated =
                {node with Node.sequence = Sequence.of_string_exn sequence} in
              current_node <- Some updated;
              add_or_update_node t updated
            end
          method finalize =
            self#flush_sequence
            >>= fun () ->
            let node =
              new_node t ~kind:`Reference
                ~sequence:Sequence.empty ~next:[| |] ~prev:[| |] in
            begin match current_node with
            | None -> return [| |]
            | Some current ->
              add_or_update_node t
                {current with Node.next = [|Node.pointer node|]}
              >>= fun () ->
              return [| Node.pointer current |]
            end
            >>= fun new_prev ->
            add_or_update_node t { node with Node.prev = new_prev }
          method new_root chromosome comment =
            babble "new_root %s %s" chromosome comment;
            self#finalize
            >>= fun () ->
            let sequence = Sequence.empty in
            let first_node =
              new_node t ~kind:`Reference ~sequence ~next:[| |] ~prev:[| |] in
            let second_node =
              new_node t ~kind:`Reference ~sequence ~next:[| |]
                ~prev:[| Node.pointer first_node |] in
            t.roots  <- t.roots @ [{chromosome; comment},
                                   Node.pointer first_node];
            add_or_update_node t {first_node with
                                  Node.next = [| Node.pointer second_node |] }
            >>= fun () ->
            current_node <- Some second_node;
            return ()
          method add_dna dna =
            current_sequence <- dna :: current_sequence;
            let count =
              List.fold current_sequence ~init:0
                ~f:(fun x s -> x + String.length s) in
            babble "Adding %s count: %d" dna count;
            if count >= packetization_threshold then (
              self#flush_sequence
              >>= fun () ->
              let current = Option.value_exn current_node ~msg:"current_node" in
              let node =
                new_node t ~kind:`Reference ~sequence:Sequence.empty
                  ~next:[| |]
                  ~prev:[| Node.pointer current |] in
              babble "add_dna: updating node : %s with next %s"
                (Node.to_string current) (Node.to_string node);
              add_or_update_node t {current with Node.next = [|Node.pointer node|]}
              >>= fun () ->
              current_node <- Some node;
              return ()
            ) else (
              return ()
            )
        end
      in
      let open Linear_genome in
      let init = Position.create "" 0 in
      fold_lines ?buffer_size path ~init ~f:(fun ~line_number position line ->
          let fasta_event = FASTA.event_of_line line in
          let newpos = FASTA.update_position fasta_event ~position in
          match fasta_event with
          | `Chromosome_line (chr, comment)
            when Region.interesting_chromosome region chr ->
            state#new_root chr comment
            >>= fun () ->
            return newpos
          | `Chromosome_line (chr, comment) -> return newpos
          | `Piece_of_DNA line ->
            begin match Region.sub_string region position line with
            | Some str -> state#add_dna str
            | None -> return ()
            end
            >>= fun () ->
            return newpos
        )
        ~on_exn:(fun ~line_number e ->
            `Graph (`Load_fasta (path, line_number, e)))
      >>= fun last_pos ->
      state#finalize

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
          let seq = node.Node.sequence in
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
    let new_ref_node =
      let next = Array.copy reference_node.Node.next in
      let prev = [| Node.pointer reference_node |] (* + ins_node *) in
      new_node t ~kind:`Reference ~sequence:after ~next ~prev
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
       sequence = before;
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


  let add_vcf:
    ?buffer_size: int ->
    ?region:Linear_genome.Region.t -> t -> path:string ->
    (unit, _) Deferred_result.t =
    fun ?buffer_size ?(region=`Everything) t ~path ->
      let temp = ref [] in
      fold_lines path ?buffer_size ~init:() ~f:begin fun ~line_number () line ->
        match line with
        | comment when String.get comment ~index:0 = Some '#' ->
          babble "Comment: %s" comment;
          return ()
        | row ->
          let variants = Variant.of_vcf_row_exn row in
          Deferred_list.for_concurrent variants ~f:(fun variant ->
              if line_number mod 100_000 = 0 then
                dbg "[%s] %s:%d Variant: %s (temp: %d)"
                  Time.(now () |> to_filename) path line_number
                  (Variant.to_string variant)
                  (List.length !temp) ;
              match Variant.intersect_region variant region with
              | Some variant ->
                babble "Variant: %s" (Variant.to_string variant);
                temp := variant :: !temp;
                integrate_variant t ~variant
              | None -> return ()
            )
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
        List.fold !temp ~init:(0,0,0,0) ~f:(fun (snv, ins, del, repl) v ->
            match v.Variant.action with
            | `Replace (a, b)
              when Sequence.length a = 1 && Sequence.length b = 1 ->
              (snv + 1, ins, del, repl)
            | `Replace _ -> (snv, ins, del, repl + 1)
            | `Insert _ -> (snv, ins + 1, del, repl)
            | `Delete _ -> (snv, ins, del + 1, repl))
      in
      dbg "Variants: %d “lines”, %d snvs, %d ins, %d dels, %d repls"
        (List.length !temp) snv ins del repl;
      return ()

end
