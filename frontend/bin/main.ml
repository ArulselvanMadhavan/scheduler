open Cohttp_lwt_unix
open Magizhchi
open Base

let default_port = 8421

let html =
  {|
   <!DOCTYPE html>
   <html lang="en">
   <head>
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
    .time-row {
      display: flex;
      justify-content: center;
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
    .main-header {
        display: flex;
        justify-content: center;
    }
    .misc-container > .task-pref-row {
        display: flex;
        border: solid;
        width: 30em;
        margin: 0 auto;
    }
   </style>
   </head>
   <body>
   <h1 class="main-header">Fort Awesome Task Scheduler</h1>
   <div class="magizhchi
   -container">
   <div id="app"></div>
   <div id="viz"></div>
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
let xlsx_pattern = Base.String.Search_pattern.create ~case_sensitive:false "xlsx"

let server ~scheduler ~port =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    let has_body =
      match Request.has_body req with
      | `No -> false
      | _ -> true
    in
    let guests_with_prefs = "/" ^ Constants.guests_with_preferences in
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
      respond_file ~content_type:"application/json" (String.drop_prefix uri 1)
    | uri when Base.String.Search_pattern.matches csv_pattern uri && has_body ->
      let save_body body =
        let _ = Csv_utils.write_csv ~fpath:(String.drop_prefix uri 1) body in
        respond_string ~content_type:"application/csv" ~status:`OK ~body ()
      in
      Lwt.(Cohttp_lwt.(Body.to_string body) >>= save_body)
    | uri when Base.String.Search_pattern.matches xlsx_pattern uri ->
      respond_file ~content_type:"application/csv" (String.drop_prefix uri 1)
    | uri when Base.String.is_substring uri ~substring:guests_with_prefs ->
      let body =
        Stdlib.Sys.readdir Constants.preferences_dir
        |> Base.Array.filter
             ~f:(Fn.compose (String.equal ".csv") Stdlib.Filename.extension)
        |> Base.Array.map ~f:(Fn.flip Stdlib.Filename.chop_suffix ".csv")
        |> Array.to_list
        |> String.concat ~sep:","
      in
      respond_string ~content_type:"text/html" ~status:`OK ~body ()
    | uri when has_body && String.is_substring uri ~substring:Constants.delete_guests ->
      let handle_body b =
        String.split ~on:',' b |> List.iter ~f:Stdlib.Sys.remove;
        respond_string ~content_type:"text/html" ~status:`OK ~body:"" ()
      in
      Lwt.(Cohttp_lwt.(Body.to_string body) >>= handle_body)
    | uri when Base.String.Search_pattern.matches csv_pattern uri ->
      respond_file ~content_type:"application/csv" (Base.String.drop_prefix uri 1)
    | uri when String.is_substring uri ~substring:Constants.scheduler ->
      let open Pyops in
      let body = scheduler.&("solve") [||] in
      let body = Py.Bool.is_true body |> Bool.to_string in
      respond_string ~content_type:"text/html" ~status:`OK ~body ()
    | _ -> respond_string ~content_type:"text/html" ~status:`Not_found ~body:"" ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
;;

let main scheduler ~port =
  Stdio.printf "\nRunning at: http://localhost:%d/index.html\n" port;
  Stdio.Out_channel.flush Stdio.stdout;
  Lwt_main.run @@ server ~scheduler ~port
;;

let () =
  Py.initialize ();
  let scheduler = Py.import "scheduler" in
  main scheduler ~port:default_port
;;
