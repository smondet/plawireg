
open Internal_pervasives

type t = {
  name: string;
  position: string * int;
  action: [
    | `Replace of Sequence.t * Sequence.t
    | `Delete of int
    | `Insert of Sequence.t
  ];
} [@@deriving show, yojson]
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

let intersect_region t reg =
  match reg with
  | `Everything -> Some t
  | `Range (chr, b, e) when chr <> fst t.position -> None
  | `Range (chr, b, e) ->
    let loc = snd t.position in
    if b <= loc && loc <= e
    then Some {t with position = (chr, loc - b + 1)}
    else None
(* TODO:
   be more clever and cut the variants that are at the border to make them fit
   begin match t.action with
   | `Insert seq when loc
   | `Delete _
   | `Replace _
   end
*)

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
