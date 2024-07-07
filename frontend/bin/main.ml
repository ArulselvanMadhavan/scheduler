open Cohttp_lwt_unix

let default_port = 8421

let html =
  {|
   <!DOCTYPE html>
   <html lang="en">
   <head>
   <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
   <script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
   <script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
   <script type="text/javascript" src="main.js"></script>
   <style>
   .magizhchi-container {
   display: flex;
   }
  .button-row {
      display: flex;
      flex-direction: row;
      align-items: center;
      justify-content: center;
      border: solid;
  }
  .button-row > button {
     margin:auto;
   }
   table {
    border: solid;
    display: flex;
    flex-direction: row;
    margin: auto;
    align-items: center;
    justify-content: center;
   }
    .day-row {
        display: flex;
        flex-direction: row;
        align-items: flex-start;
        justify-content: center;
        margin: auto;
        border: solid;
    }
    .day-col {
      margin: 0 auto;
    }
    .day-header {
        display: flex;
        align-items: center;
        justify-content: center;
        border: solid;
    }
    .task-pref-row {
        display: flex;
        justify-content: space-between;
    }
   </style>
   </head>
   <body>
   <h1>Select your Schedule</h1>
   <div class="magizhchi
   -container">
   <div id="app"></div>
   </div>
   </body>
   </html>
   |}
;;

let respond_string ~content_type ~status ~body ?headers =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Server.respond_string ~headers ~status ~body
;;

let respond_file ~content_type ?headers s =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Server.respond_file ~headers ~fname:s ()
;;

let json_pattern = Base.String.Search_pattern.create ~case_sensitive:false "json"
let csv_pattern = Base.String.Search_pattern.create ~case_sensitive:false "csv"

let server ~port =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    let has_body =
      match Request.has_body req with
      | `No -> false
      | _ -> true
    in
    match uri with
    | "" | "/" | "/index.html" ->
      respond_string ~content_type:"text/html" ~status:`OK ~body:html ()
    | "/main.js" ->
      respond_string
        ~content_type:"application/javascript"
        ~status:`OK
        ~body:Embedded_files.main_dot_bc_dot_js
        ()
    | uri when Base.String.Search_pattern.matches json_pattern uri ->
      respond_file ~content_type:"application/json" (Base.String.drop_prefix uri 1)
    | uri when Base.String.Search_pattern.matches csv_pattern uri && has_body ->
      let save_body body =
        let _ =
          Magizhchi.Csv_utils.write_csv ~fpath:(Base.String.drop_prefix uri 1) body
        in
        respond_string ~content_type:"application/csv" ~status:`OK ~body ()
      in
      Lwt.(Cohttp_lwt.(Body.to_string body) >>= save_body)
    | uri when Base.String.Search_pattern.matches csv_pattern uri ->
      respond_file ~content_type:"application/csv" (Base.String.drop_prefix uri 1)
    | _ -> respond_string ~content_type:"text/html" ~status:`Not_found ~body:"" ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
;;

let main ~port =
  Stdio.printf "\nRunning at: http://localhost:%d/index.html\n" port;
  Stdio.Out_channel.flush Stdio.stdout;
  Lwt_main.run @@ server ~port
;;

let () = main ~port:default_port
