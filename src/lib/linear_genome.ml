open Internal_pervasives

module Position = struct
  type t = {chromosome: string; position: int} [@@deriving show, yojson]
  let create chromosome position = {chromosome; position}
  let add pos loci = {pos with position = pos.position + loci}
  let in_range p chr p1 p2 =
    p.chromosome = chr && p1 <= p.position && p.position <= p2
end

module Region = struct
  type t = [
    | `Everything
    | `Range of string * int * int
  ] [@@deriving show, yojson]
  let range pos1 pos2 =
    let open Position in
    if pos1.chromosome = pos2.chromosome
    then Some (`Range (pos1.chromosome, pos1.position, pos2.position))
    else None

  let of_string_exn s =
    match String.split s ~on:(`Character ':') with
    | chr :: reg :: [] ->
      begin match String.split reg ~on:(`Character '-') with
      | b :: e :: [] ->
        let parse_int i =
          Int.of_string i
          |> Option.value_exn ~msg:"Region.of_string: Not an integer"
        in
        `Range (chr, parse_int b, parse_int e)
      | other ->
        failwithf "Region.of_string_exn: can't parse: %S" reg
      end
    | ["all"] -> `Everything
    | other ->
      failwithf "Region.of_string_exn: can't parse: %S" s

  let interesting_chromosome (reg : t) chr =
    match reg with
    | `Everything -> true
    | `Range (c, _, _) -> c = chr

  let sub_string reg {Position. chromosome; position} str =
    match reg with
    | `Everything -> Some str
    | `Range (chr, b, e) when chr = chromosome ->
      let index =
        if b <= position then 0 else b - position in
      let length =
        let sl = String.length str in
        if position + sl <= e
        then sl - index
        else 1 + sl - index - (position + sl - e)
      in
      if length > 0
      then Some (String.sub_exn str ~index ~length)
      else None
    | _ -> None
          (*
             let test rb re pos str =
                sub_string (`Range ("1", rb, re)) {chromosome = "1"; position = pos} str
             test 1 100 1 "bouh"
             test 1 100 4 "bouh"
             test 6 100 4 "bouh"
             test 5 6 4 "bouh"
             test 1 100 100 "bouh"
             test 1 100 101 "bouh"
             *)
end

module FASTA = struct
  type event = [
    | `Chromosome_line of string * string
    | `Piece_of_DNA of string
  ]
  let event_of_line line =
    if String.get line 0 = Some '>'
    then begin
      let with_out_carret =
        String.(sub_exn line ~index:1 ~length:(length line - 1)) in
      match
        with_out_carret |> String.split ~on:(`Character ' ')
        |> List.filter_map
          ~f:(fun s -> match String.strip s with "" -> None | st -> Some st)
      with
      | chromosome :: more :: than_one ->
        `Chromosome_line (chromosome,
                          String.concat ~sep:" " (more :: than_one))
      | other -> failwithf "Cannot find chromosome name: %S" line
    end else `Piece_of_DNA line

  let update_position (event : event) ~position =
    match event with
    | `Chromosome_line (chr, _) -> Position.create chr 1
    | `Piece_of_DNA dna -> Position.add position String.(length dna)
end
