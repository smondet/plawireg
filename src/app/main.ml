open Nonstd
module String = Sosa.Native_string
let failwithf fmt = ksprintf failwith fmt

let () =
  match Sys.argv |> Array.to_list |> List.tl_exn with
  | other ->
    failwithf "Cannot understand: [%s]"
      (List.map other ~f:(sprintf "%S") |> String.concat ~sep:"; ")
