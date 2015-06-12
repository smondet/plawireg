
open Internal_pervasives

module Error = struct

  let to_string = function
  | `Start_error string -> sprintf "starting server: %s" string

end

let start how =
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
  Deferred_result.wrap_deferred
    ~on_exn:(function
      | e -> `Server (`Start_error (Printexc.to_string e)))
    Lwt.(fun () ->
        (* let sockaddr = Lwt_unix.(ADDR_INET (Unix.inet_addr_any, port)) in *)
        let request_callback _ request body =
          let high_level_answer = `Error "not implemented" in
          begin match high_level_answer with
          | `Ok `Unit ->
            Cohttp_lwt_unix.Server.respond_string ~status:`OK  ~body:"" ()
          | `Error e ->
            Cohttp_lwt_unix.Server.respond_string
              ~status:`Not_found ~body:e ()
          end
        in
        let conn_closed (_, conn_id) =
          dbg "conn %S closed" (Cohttp.Connection.to_string conn_id) 
        in
        Cohttp_lwt_unix.Server.(
          create ~mode (make ~callback:request_callback ~conn_closed ()))
      )

