open Nonstd
module String = Sosa.Native_string
open Pvem_lwt_unix
open Deferred_result

let failwithf fmt = ksprintf failwith fmt


module Error = struct
  let to_string = function
  | `Graph ce -> Plawireg.Graph.Error.to_string ce
  | `Cache ce -> Plawireg.Cache.Error.to_string ce
  | `IO _ as e -> IO.error_to_string e
end

let generate_test_fasta ~path =
  let content =
    String.concat ~sep:"" (List.map ~f:(sprintf "%s\n") [
        ">1 bla bla";
        "NNNNNNNNNN";
        "NNNNNNNNNN";
        "NNNNNNNNNN";
        "ACGTCNTNTC";
        "CAGCNTNNTC";
        "CCACGCCTCC";
        "CCACGCCTCC";
        "CCAGCCCCTC";
        ">2 bla bla";
        ">3 bla bla";
        "ACGTCNTNTC";
        "CAGCNTNNTC";
        "CCACGCCTCC";
        "NNNNNNNNNN";
      ]) in
  IO.write_file path ~content

let test_load files =
  let outline fmt = ksprintf (fun s -> printf "%s\n" s; return ()) fmt in
  Deferred_list.while_sequential files ~f:(fun f ->
      Plawireg.Graph.create ()
      >>= fun graph ->
      Plawireg.Graph.load_reference graph f
      >>= fun () ->
      Plawireg.Graph.fold graph ~init:(()) ~f:(fun () -> function
        | `Name n -> outline "» %s" n
        | `Node (id, kind, seq) ->
          outline "  %s → %s"
            (Plawireg.Unique_id.to_string id) (Plawireg.Sequence.to_string seq)
        )
      >>= fun () ->
      return ())
  >>= fun (_ : unit list) ->
  return ()
  
let () =
  let to_do =
    match Sys.argv |> Array.to_list |> List.tl_exn with
    | "generate" :: "test" :: "fasta" :: path :: [] ->
      generate_test_fasta ~path
    | "test-load" :: files ->
      test_load files
    | other ->
      failwithf "Cannot understand: [%s]"
        (List.map other ~f:(sprintf "%S") |> String.concat ~sep:"; ")
  in
  match (Lwt_main.run to_do) with
  | `Error e ->
    failwithf "Error: %s" (Error.to_string e)
  | `Ok () -> ()
