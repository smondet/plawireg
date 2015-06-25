
open Internal_pervasives

module Protocol = Plawireg.Protocol

module Error = struct

  let fail e =  fail (`Server e)
  let wrong_method mth = fail (`Wrong_method mth)
  let wrong_path mth = fail (`Wrong_path mth)
  let not_implemented mth = fail (`Not_implemented mth)

  let to_string = function
  | `Start_error string -> sprintf "starting server: %s" string
  | `Wrong_method m ->
    sprintf "Wrong HTTP method: %S" (Cohttp.Code.string_of_method m)
  | `Wrong_path m -> sprintf "Wrong HTTP path: %S" m
  | `Not_implemented s -> sprintf "Not implemented: %S" s

end

module State = struct
  type t = {
    graph: Reference_graph.Graph.t;
  }
  let create ~graph () = { graph }
  let graph {graph} = graph
end

let respond_to_api_call ~state msg =
  match msg with
  | `Get_nodes ids ->
    let rg = State.graph state in
    Deferred_list.while_sequential ids ~f:(fun id ->
        Reference_graph.Graph.get_node rg (Pointer.create id))
    >>= fun nodes ->
    return (`Nodes nodes)
  | `List_roots ->
    let rg = State.graph state in
    let roots = Reference_graph.Graph.roots rg in
    Deferred_list.while_sequential roots ~f:(fun (_, np) ->
        Reference_graph.Graph.get_node rg np
      )
    >>= fun nodes ->
    return (`Nodes nodes)


let dispatch ~state request body =
  match Uri.path (Cohttp.Request.uri request) with
  | "/hello" -> return `Unit
  | "/api" ->
    begin match Cohttp.Request.meth request with
    | `POST ->
      wrap_deferred ~on_exn:(fun e -> `IO (`Exn e))
        (fun () -> Cohttp_lwt_body.to_string  body)
    | other -> Error.wrong_method other
    end
    >>= fun body_str ->
    of_result (Protocol.Up_message.of_string body_str)
    >>= fun msg ->
    respond_to_api_call ~state msg
    >>= fun msg_down ->
    let ret =
      Protocol.Down_message.to_yojson msg_down |> Yojson.Safe.to_string ~std:true
    in
    return (`String ret)
  | other -> Error.wrong_path other

let start ~state how =
  let mode : Conduit_lwt_unix.server =
    match how with
    | `Tls (certfile, keyfile, port) ->
      `OpenSSL (
        `Crt_file_path certfile,
        `Key_file_path keyfile,
        `No_password, `Port port)
    | `TCP port -> `TCP (`Port port)
(*
    [ `OpenSSL of Conduit_lwt_unix.server_tls_config
    | `TCP of [ `Port of int ]
    | `TLS of Conduit_lwt_unix.server_tls_config
    | `TLS_native of Conduit_lwt_unix.server_tls_config
    | `Unix_domain_socket of [ `File of string ]
    | `Vchan_direct of int * string
    | `Vchan_domain_socket of string * string ]
*)
  in
  let server_state = state in
  Deferred_result.wrap_deferred
    ~on_exn:(function
      | e -> `Server (`Start_error (Printexc.to_string e)))
    Lwt.(fun () ->
        (* let sockaddr = Lwt_unix.(ADDR_INET (Unix.inet_addr_any, port)) in *)
        let request_callback _ request body =
          dispatch ~state:server_state request body
          >>= fun high_level_answer ->
          begin match high_level_answer with
          | `Ok `Unit ->
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body:"" ()
          | `Ok (`String body) ->
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body ()
          | `Error e ->
            let body =
              match e with
                `IO _ as e -> IO.error_to_string e
              | `Server s -> Error.to_string s
              | `Protocol (`Parsing (m ,e)) ->
                sprintf "Failed parsing %S: %s" m e
              | `Cache e ->
                sprintf "Cache error: %s" (Cache.Error.to_string e)
            in
            Cohttp_lwt_unix.Server.respond_string
              ~status:`Not_found ~body ()
          end
        in
        let conn_closed (_, conn_id) =
          dbg "conn %S closed" (Cohttp.Connection.to_string conn_id) 
        in
        Cohttp_lwt_unix.Server.(
          create ~mode (make ~callback:request_callback ~conn_closed ()))
      )

