open Nonstd
module String = Sosa.Native_string
open Pvem_lwt_unix
open Deferred_result

let failwithf fmt = ksprintf failwith fmt

let verbose = Plawireg.is_babbling

module Error = struct
  let to_string = function
  | `Graph ce -> Plawireg.Graph.Error.to_string ce
  | `Cache ce -> Plawireg.Cache.Error.to_string ce
  | `IO _ as e -> IO.error_to_string e
end

let write_lines ~path l =
  let content =
    String.concat ~sep:"" (List.map ~f:(sprintf "%s\n") l) in
  IO.write_file path ~content

let generate_test_fasta ~path =
  let chr name comment lines = sprintf ">%s %s" name comment :: lines in
  let chr1 = [
    "NNNNN";
    "NNNNN";
    "NNNNN";
    "ACGTC";
    "CAGCN"; (* 21 -- 25 *)
    "CCACG";
    "CATTT"; (* 31 -- 32 *)
    "CCAGT";
  ] in
  write_lines ~path (List.concat [
      chr "1" "chr with a bunch of Ns" chr1;
      chr "2" "like 1 but testing insertions" chr1;
      chr "3" "like 1 but testing deletions" chr1;
      chr "4" "like 1 but testing substitutions" chr1;
      [
        ">5 more tests";
        "ACGTC";
        "NTNTC";
        "CAGCN";
        "TNNTC";
        "CCACG";
        "CCTCC";
        "NNNNN";
        "NNNNN";
      ]
    ])

let generate_test_dbnsp ~path =
  let tabify l = String.concat ~sep:"\t" l in
  let variant ?info chr pos ?name refer alt =
    tabify [
      Int.to_string chr;
      Int.to_string pos;
      Option.value name ~default:(sprintf "rs%d-%d" chr pos);
      refer;
      alt;
      ".";
      ".";
      Option.value info ~default:"SOME=RANDOM;GARBAGE=42";
    ] in
  write_lines ~path [
    "# comment stuf";
    "# comment stuf";
    "# comment stuf";
    tabify ["#CHROM"; "POS"; "ID"; "REF"; "ALT"; "QUAL"; "FILTER"; "INFO"];
    (* variant 2 1 "" "CGT" ~name:"insert-CGT"; *)
    variant 2 4 "N" "NCGT" ~name:"insert-CGT";
    variant 2 9 "N" "NAC,NGTTTT" ~name:"insert-AC-and-GTTTT";
    variant 3 17 "CGT" "C" ~name:"del-GT-inside-ACGTC";
    variant 3 23 "GCNCC" "G" ~name:"del-CNCC-accros-CAGCN-CCACG";
    variant 3 30 "GCATTT" "G" ~name:"del-CATTT-bypass-existing";
    variant 4 1 "N" "Y" ~name:"subs-N-Y-at-begining";
    variant 4 6 "N" "Y" ~name:"subs-N-Y-at-begining-of-seq";
    variant 4 12 "NN" "NACGT" ~name:"subs-N-ACGT-at-inside-seq";
    variant 4 39 "GT" "HELLO" ~name:"subs-GT-HELLO-at-the-end";
    variant 5 4 "T" "A"
      ~name:"SNV-T-A"
      ~info:"CAF=[0.9,.01]";
    variant 5 5 "C" "A,CTGTG"
      ~name:"subs+ins-after-SNV-T-A"
      ~info:"CAF=[0.8,0.15,0.05]";
    variant 5 6 "NTN" "N,NG"
      ~name:"del-TN-or-subs-G" ;
  ]

let test_load ~memory_stats ~fasta ~dbsnp =
  let print_stats msg =
    if memory_stats then
      begin
        printf "\n=== %s %s ===\nGC stats:\n" msg Plawireg.Time.(now () |> to_filename);
        Gc.print_stat stdout;
        IO.read_file "/proc/self/status"
        >>= fun status ->
        printf "\n/proc/self/status:\n%s\n%!" status;
        return ()
      end
    else
      begin
        printf "\n=== %s %s ===\n" msg Plawireg.Time.(now () |> to_filename);
        return ()
      end
  in
  Plawireg.Graph.create ()
  >>= fun graph ->
  print_stats "before load_reference" >>= fun () ->
  Plawireg.Graph.load_reference graph ~path:fasta
  >>= fun () ->
  print_stats "before add_vcf" >>= fun () ->
  Plawireg.Graph.add_vcf graph ~path:dbsnp
  >>= fun () ->
  print_stats "before count_nodes" >>= fun () ->
  Plawireg.Graph.count_nodes graph
  >>= fun counts ->
  let () =
    printf "\nCounts:\n";
    List.iter  counts (fun (name, count) ->
        printf "* %s: %d nodes\n" name.Plawireg.Graph.chromosome count
      ) in
  begin
    if verbose then
      print_stats "before outputting .dot file" >>= fun () ->
      let o = open_out "out.dot" in
      fprintf o "digraph refgraph {\n";
      Plawireg.Graph.fold graph ~init:false ~f:(fun in_subgraph -> function
        | `Name { Plawireg.Graph. chromosome; comment } ->
          (if in_subgraph then fprintf o "}\n");
          fprintf o "subgraph cluster%s {" chromosome;
          fprintf o "  label=\"%s (%s)\"" chromosome comment;
          return true
        | `Node node ->
          let open Plawireg.Node in
          let id = Plawireg.Unique_id.to_string in
          let seq = Plawireg.Sequence.to_string in
          Plawireg.Graph.expand_node graph node
          >>= fun (_, _, sequence) ->
          let shape, comment =
            match node.kind with
            | `Db_snp comment ->
              ("box", Plawireg.Variant.to_string comment)
            | `Reference -> "doubleoctagon", ""
            | `Cosmic cm -> "diamond", cm
          in
          let color =
            match Array.length node.next, Array.length node.prev with
            | 0, _ -> "red"
            | _, 0 -> "green"
            | _, _ -> "black" in
          let label =
            sprintf "%s %s" (seq sequence) comment |> String.strip in
          fprintf o "N_%s [label=%S,shape=%s,color=%s,\
                     fontname=\"monospace\",fontsize=18];\n"
            (id node.id) label shape color;
          Array.iter node.next ~f:(fun {Plawireg.Pointer. id = child} ->
              fprintf o "N_%s -> N_%s[color=red];\n" (id node.id) (id child);
            );
          Array.iter node.prev ~f:(fun {Plawireg.Pointer. id = parent} ->
              fprintf o "N_%s -> N_%s[color=blue];\n" (id node.id) (id parent);
            );
          return in_subgraph
        )
      >>= fun in_subgraph ->
      (if in_subgraph then fprintf o "}\n");
      fprintf o "}\n";
      close_out o;
      print_stats "before second fold"
    else
      print_stats "before first fold"
  end
  >>= fun () ->
  Plawireg.Graph.fold graph ~init:([]) ~f:(fun prev -> function
    | `Name n -> return (n.Plawireg.Graph.chromosome :: prev)
    | `Node node ->
      Plawireg.Graph.expand_node graph node
      >>= fun (id, kind, seq) ->
      return (sprintf "%s → %s"
                (Plawireg.Unique_id.to_string id)
                (Plawireg.Sequence.to_string seq)
              :: prev)
      (* outline "  %s → %s" *)
      (*   (Plawireg.Unique_id.to_string id)  *)
    )
  >>= fun list_of_strings ->
  let () =
    printf "Names and Seqs:\n";
    begin try
      List.iteri (List.rev list_of_strings) ~f:(fun i x ->
          if i > 100 then raise Not_found
          else
            printf "  %s\n" x)
    with _ -> (printf "  ...\n")
    end
  in
  let base_counts = Hashtbl.create 42 in
  Plawireg.Graph.fold graph ~init:"" ~f:(fun name -> function
    | `Name n ->
      Hashtbl.add base_counts n.Plawireg.Graph.chromosome (Hashtbl.create 42);
      return n.Plawireg.Graph.chromosome
    | `Node node ->
      Plawireg.Graph.expand_node graph node
      >>= fun (id, kind, seq) ->
      let () =
        String.iter (Plawireg.Sequence.to_string seq) ~f:(fun c ->
            let ht = Hashtbl.find base_counts name in
            match Hashtbl.find ht c with
            | v -> incr v
            | exception e -> Hashtbl.add ht c (ref 1))
      in
      return name)
  >>= fun (_ : string) ->
  printf "\nSome Stats:\n%!";
  let disp chr ht =
    printf "- %s:\n" chr;
    Hashtbl.iter (fun c cr -> printf "  %c\t%d\n" c !cr) ht
  in
  Hashtbl.iter disp base_counts;
  print_stats "last one" >>= fun () ->
  return ()

let () =
  let to_do =
    match Sys.argv |> Array.to_list |> List.tl_exn with
    | "generate" :: "test" :: file_kind :: path :: [] ->
      begin match file_kind with
      | "fasta" -> generate_test_fasta ~path
      | "dbsnp" -> generate_test_dbnsp ~path
      | other -> failwithf "Unknown file-kind to generate"
      end
    | "test-load" :: fasta :: dbsnp :: more ->
      let memory_stats = List.mem ~set:more "memory-stats" in
      test_load ~memory_stats ~fasta ~dbsnp
    | "test-uid" :: _ ->
      printf "Test-UID:\n%!";
      printf "- time: %f s\n%!" (Plawireg.Unique_id.test ());
      return ()
    | other ->
      failwithf "Cannot understand: [%s]"
        (List.map other ~f:(sprintf "%S") |> String.concat ~sep:"; ")
  in
  match (Lwt_main.run to_do) with
  | `Error e ->
    failwithf "Error: %s" (Error.to_string e)
  | `Ok () -> ()
