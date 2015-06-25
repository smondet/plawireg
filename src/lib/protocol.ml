

open Internal_pervasives

module Up_message = struct
  type t = [
    | `List_roots
    | `Get_nodes of Pointer.id list
  ] [@@deriving yojson]

  let of_string s =
    let open Result in
    try
      (Yojson.Safe.from_string s |> of_yojson)
      >>< function
      | `Ok o -> return o
      | `Error e -> fail (`Protocol (`Parsing (s, e)))
    with e -> fail (`Protocol (`Parsing (s, Exn.to_string e)))
end

module Down_message = struct
  type t = [
    | `Nodes of Node.t list
  ]  [@@deriving yojson] 
end
