open Internal_pervasives

type 'a t = (Pointer.id, 'a) Hashtbl.t

let create () = Hashtbl.create 42

let store: 'a t -> at:Pointer.id -> value:'a -> (unit, _) Deferred_result.t =
  fun cache ~at ~value ->
    Hashtbl.replace cache at value;
    return ()

let get: 'a t -> Pointer.id -> ('a, _) Deferred_result.t =
  fun cache id ->
    match Hashtbl.find  cache id with
    | some -> return some
    | exception Not_found -> fail (`Cache (`Not_found id))

module Error = struct
  let to_string = function
  | `Not_found id ->
    sprintf "Data-not-found at: %S" (Unique_id.to_string id)
end
