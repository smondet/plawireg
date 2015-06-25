open Internal_pervasives

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
