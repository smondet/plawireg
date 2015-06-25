
include Nonstd
module String = Sosa.Native_string
let failwithf fmt = ksprintf failwith fmt
include Pvem
include Pvem_lwt_unix
include Deferred_result
module Exn = Plawireg.Internal_pervasives.Exn

let dbg fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
let is_babbling =
  try Sys.getenv "VERBOSE" = "true" with _ -> false
let babble fmt =
  ksprintf (fun s ->
      if is_babbling then dbg "%s" s else ()
    ) fmt

module Time = struct
  include Plawireg.Internal_pervasives.Time
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
  type t = Plawireg.Internal_pervasives.Unique_id.t
  [@@deriving show, yojson]
  val create: unit -> t
  val to_string: t -> string
  val test: unit -> float
end = struct
  include Plawireg.Internal_pervasives.Unique_id

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

module Pointer = Plawireg.Internal_pervasives.Pointer
module Sequence = Plawireg.Internal_pervasives.Sequence
                   

let fold_lines ?buffer_size path ~on_exn ~init ~f : (_, _) Deferred_result.t  =
  wrap_deferred ~on_exn:(on_exn ~line_number:0)
    (fun () -> Lwt_io.open_file ~mode:Lwt_io.input ?buffer_size path)
  >>= fun chan ->
  let stream = Lwt_io.read_lines chan in
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
