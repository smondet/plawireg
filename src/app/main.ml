open Nonstd
module String = Sosa.Native_string
open Pvem_lwt_unix
open Deferred_result

open Plawireg
    
let failwithf fmt = ksprintf failwith fmt

let verbose = Internal_pervasives.is_babbling

module Error = struct
  let to_string = function
  | `Graph ce -> Reference_graph.Graph.Error.to_string ce
  | `Cache ce -> Cache.Error.to_string ce
  | `IO _ as e -> IO.error_to_string e
  | `Failure s -> sprintf "Failure: %s" s
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
    "CATTT"; (* 31 -- 35 *)
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

let test_load ~region_string ~memory_stats ~fasta ~dbsnp
    ~packetization_threshold =
  let print_stats msg =
    let open Internal_pervasives in
    if memory_stats then
      begin
        printf "\n=== %s %s ===\nGC stats:\n" msg Time.(now () |> to_filename);
        Gc.print_stat stdout;
        IO.read_file "/proc/self/status"
        >>= fun status ->
        printf "\n/proc/self/status:\n%s\n%!" status;
        return ()
      end
    else
      begin
        printf "\n=== %s %s ===\n" msg Time.(now () |> to_filename);
        return ()
      end
  in
  let open Reference_graph in
  Graph.create ()
  >>= fun graph ->
  print_stats "before load_reference" >>= fun () ->
  let region = Linear_genome.Region.of_string_exn region_string in
  Graph.load_reference graph ~path:fasta ~region
    ~packetization_threshold
  >>= fun () ->
  (* exit 0 |> ignore; *)
  print_stats "before add_vcf" >>= fun () ->
  Graph.add_vcf graph ~path:dbsnp ~region
  >>= fun () ->
  print_stats "before count_nodes" >>= fun () ->
  Graph.count_nodes graph
  >>= fun counts ->
  let () =
    printf "\nCounts:\n";
    List.iter  counts (fun (name, count) ->
        printf "* %s: %d nodes\n" name.Graph.chromosome count
      ) in
  begin
    if verbose then
      let open Internal_pervasives in
      print_stats "before outputting .dot file" >>= fun () ->
      let o = open_out "out.dot" in
      fprintf o "digraph refgraph {\n";
      Graph.fold graph ~init:false ~f:(fun in_subgraph -> function
        | `Name { Graph. chromosome; comment } ->
          (if in_subgraph then fprintf o "}\n");
          fprintf o "subgraph cluster%s {" chromosome;
          fprintf o "  label=\"%s (%s)\"" chromosome comment;
          return true
        | `Node node ->
          let open Node in
          let id = Unique_id.to_string in
          let seq = Sequence.to_string in
          Graph.expand_node graph node
          >>= fun (_, _, sequence) ->
          let shape, comment =
            match node.kind with
            | `Db_snp comment -> ("box", Variant.to_string comment)
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
          Array.iter node.next ~f:(fun child ->
              let cid = Pointer.id  child in
              fprintf o "N_%s -> N_%s[color=red];\n" (id node.id) (id cid);
            );
          Array.iter node.prev ~f:(fun parent ->
              let cid = Pointer.id parent in
              fprintf o "N_%s -> N_%s[color=blue];\n" (id node.id) (id cid);
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
  Graph.fold graph ~init:([]) ~f:(fun prev -> function
    | `Name n -> return (n.Graph.chromosome :: prev)
    | `Node node ->
      Graph.expand_node graph node
      >>= fun (id, kind, seq) ->
      return (sprintf "%s → %s"
                (Internal_pervasives.Unique_id.to_string id)
                (Internal_pervasives.Sequence.to_string seq)
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
  Graph.fold graph ~init:"" ~f:(fun name -> function
    | `Name n ->
      Hashtbl.add base_counts n.Graph.chromosome (Hashtbl.create 42);
      return n.Graph.chromosome
    | `Node node ->
      Graph.expand_node graph node
      >>= fun (id, kind, seq) ->
      let () =
        String.iter (Internal_pervasives.Sequence.to_string seq) ~f:(fun c ->
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

open Internal_pervasives
type bench_result = {
  packetization: int;
  buffer_size: int;
  region: Linear_genome.Region.t;
  fasta: string;
  vcf: string;
  start_time: Time.t;
  ref_loaded: Time.t;
  vcf_loaded: Time.t;
  nodes_counted: Time.t;
  wc_l_executed: Time.t;
  node_count: int option;
  wc_l_vcf: int;
  wc_l_char_stream_vcf: int;
  wc_l_char_stream_executed: Time.t;
} [@@deriving show, yojson]
let benchmark ~fasta ~dbsnp ~region_string ~packetization ~buffer_size ~output =
  let open Reference_graph in
  let wc_l path =
    fold_lines ~buffer_size path ~init:0
      ~f:(fun ~line_number prev line -> return (prev + 1))
      ~on_exn:(fun ~line_number e -> 
          failwithf "exn wc_l: line %d: %s" line_number Exn.(to_string e))
  in
  let wc_l_char_stream path =
    Lwt.(
      Lwt_io.open_file ~mode:Lwt_io.input ~buffer_size path
      >>= fun chan ->
      let stream = Lwt_io.read_chars chan in
      let count = ref 0 in
      Lwt_stream.iter
        (function '\n' -> incr count | other -> ())
        stream
      >>= fun () ->
      return (`Ok !count)
      ) in
  let region = Linear_genome.Region.of_string_exn region_string in
  Graph.create ()
  >>= fun graph ->
  let start = Time.now () in
  Graph.load_reference graph ~buffer_size ~path:fasta ~region
    ~packetization_threshold:packetization
  >>= fun () ->
  let after_load_ref = Time.now () in
  Graph.add_vcf graph ~path:dbsnp ~region ~buffer_size
  >>= fun () ->
  let after_load_vcf = Time.now () in
  Graph.count_nodes graph
  >>= fun node_number ->
  let after_counts = Time.now () in
  wc_l dbsnp
  >>= fun wc_l_vcf ->
  let after_wc_l = Time.now () in
  wc_l_char_stream dbsnp
  >>= fun wc_l_char_stream_vcf ->
  let wc_l_char_stream_executed = Time.now () in
  let bench = {
    packetization;
    buffer_size;
    region; fasta; vcf = dbsnp;
    start_time = start;
    ref_loaded = after_load_ref;
    vcf_loaded = after_load_vcf;
    nodes_counted = after_counts;
    wc_l_executed = after_wc_l;
    node_count = (List.nth node_number 0 |> Option.map ~f:snd);
    wc_l_vcf;
    wc_l_char_stream_executed; wc_l_char_stream_vcf;
  } in
  printf "bench: %s" (show_bench_result bench);
  IO.write_file output ~content:(bench_result_to_yojson bench
                                 |> Yojson.Safe.pretty_to_string ~std:true)
let benchmarks_summary files =
  Deferred_list.while_sequential files ~f:(fun path ->
      IO.read_file path
      >>= fun content ->
      begin
        begin try
          of_result (Yojson.Safe.from_string content |> bench_result_of_yojson)
        with e ->
          fail (sprintf "Warning cannot parse %s: %s" path Exn.(to_string e))
        end
        >>< function
        | `Ok o -> return o
        | `Error s -> fail (`Failure s)
      end
    )
  >>| List.sort  ~cmp:(fun a b ->
      match compare a.region b.region with
      | 0 -> compare a.packetization b.packetization
      | different_regions -> different_regions
    )
  >>= fun benches ->
  let cell ~title f = title, f in
  let cells = [
    cell ~title:"Region" (fun s -> Linear_genome.Region.show s.region);
    cell ~title:"Packetization" (fun s -> sprintf "%d" s.packetization);
    cell ~title:"Input-Buffer-size" (fun s -> sprintf "%d" s.buffer_size);
    cell ~title:"FASTA Loading" (fun bench ->
        sprintf "%s %f"
          Filename.(basename bench.fasta) (bench.ref_loaded -. bench.start_time));
    cell ~title:"VCF Loading" (fun bench ->
        sprintf "%s %f"
          Filename.(basename bench.vcf) (bench.vcf_loaded -. bench.ref_loaded));
    cell ~title:"Node Counting" (fun bench ->
        sprintf "%f"
          (bench.nodes_counted -. bench.vcf_loaded));
    cell ~title:"Total Plawireg" (fun bench ->
        sprintf "%f"
          (bench.nodes_counted -. bench.start_time));
    cell ~title:"Wc -l (fold_lines)" (fun bench ->
        sprintf "%f (%d)"
          (bench.wc_l_executed -. bench.nodes_counted) bench.wc_l_vcf);
    cell ~title:"Wc -l (char stream)" (fun bench ->
        sprintf "%f (%d)"
          (bench.wc_l_char_stream_executed -. bench.wc_l_executed)
          bench.wc_l_char_stream_vcf);
  ] in
  let rows =
    List.map benches ~f:(fun bench ->
        List.map cells ~f:(fun (title, f) -> (title, f bench)))
  in
  printf "| %s |\n" (String.concat ~sep:" | " (List.map cells ~f:fst));
  printf "|-%s-|\n" (String.concat ~sep:"-|-" (List.map cells ~f:(fun (title, _) -> String.map title ~f:(fun _ -> '-'))));
  List.iter rows ~f:(fun row ->
      printf "| %s |\n" (List.map ~f:snd row |> String.concat ~sep:" | ")
    );
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
    | "test-load" :: region_string :: packetize :: fasta :: dbsnp :: more ->
      let memory_stats = List.mem ~set:more "memory-stats" in
      let packetization_threshold =
        Int.of_string packetize |> Option.value_exn ~msg:"packetize argument" in
      test_load ~region_string ~memory_stats ~fasta ~dbsnp
        ~packetization_threshold
    | "bench" :: fasta :: vcf :: region_string :: packet :: buf_size :: json :: [] ->
      let packetization =
        Int.of_string packet |> Option.value_exn ~msg:"Packetization to `int`" in
      let buffer_size =
        Int.of_string buf_size |> Option.value_exn ~msg:"Buffer-size to `int`" in
      benchmark ~fasta ~dbsnp:vcf ~region_string ~packetization ~output:json
        ~buffer_size
    | "bench-report" :: files -> benchmarks_summary files
    | "test-uid" :: _ ->
      printf "Test-UID:\n%!";
      printf "- time: %f s\n%!" (Unique_id.test ());
      return ()
    | other ->
      failwithf "Cannot understand: [%s]"
        (List.map other ~f:(sprintf "%S") |> String.concat ~sep:"; ")
  in
  match (Lwt_main.run to_do) with
  | `Error e ->
    failwithf "Error: %s" (Error.to_string e)
  | `Ok () -> ()
