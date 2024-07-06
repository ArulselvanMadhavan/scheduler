open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom

let pref_node _k state set_state _graph : Node.t * int =
  let view =
    let is_disabled id = if Int.equal id state then Some Attr.disabled else None in
    Node.div
      [ Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_state 0); Attr.of_opt (is_disabled 0) ]
          [ Node.text "No" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_state 1); Attr.of_opt (is_disabled 1) ]
          [ Node.text "Maybe" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_state 2); Attr.of_opt (is_disabled 2) ]
          [ Node.text "Yes" ]
      ]
  in
  view, state
;;

let pref_for_users_assoc (graph : Bonsai.graph) : Node.t Bonsai.t =
  let tasks = [ "Task1", 1; "Task2", 0; "Task3", 2 ] in
  let tasks =
    List.map
      ~f:(fun (t, i) ->
        let create_state g =
          let s, set_state = Bonsai.state i g in
          Bonsai.both s set_state
        in
        t, create_state)
      tasks
    |> Map.of_alist_exn (module String)
  in
  let tasks = Bonsai.all_map tasks graph in
  let counters =
    Bonsai.assoc
      (module String)
      tasks
      ~f:(fun k v graph ->
        let%map state_out = v
        and k = k in
        let init, set_state = state_out in
        pref_node k init set_state graph)
      graph
  in
  let%map counters = counters in
  let task_state = Map.to_alist counters |> List.map ~f:(fun (s, (_n, c)) -> s, c) in
  Node.div
    [ Node.table
        (counters
         |> Map.to_alist
         |> List.map ~f:(fun (key, (vdom, _)) ->
           let open Node in
           let name = td [ text key ] in
           let counter = td [ vdom ] in
           tr [ name; counter ]))
    ; Node.sexp_for_debugging ([%sexp_of: (string * int) list] task_state)
    ]
;;

let build_guests guest_prefs =
  let make_cols = function
    | c0 :: _ -> Some c0
    | _ -> None
  in
  List.map guest_prefs ~f:(String.split ~on:',') |> List.filter_map ~f:make_cols
;;

let populate_guests (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let guests, set_guests = Bonsai.state [] graph in
  let%map form =
    Form.Elements.Dropdown.list
      ~init:`First_item
      (module String)
      ~equal:[%equal: String.t]
      guests
      graph
  and guests = guests
  and set_guests = set_guests in
  let is_empty_guests = Int.equal (List.length guests) 0 in
  let button_text =
    if is_empty_guests then "Load Guest Preferences" else "Save Guest Preferences"
  in
  let load_guests path =
    let open Async_kernel.Deferred.Let_syntax in
    let%map response = Async_js.Http.get path in
    match Core.Or_error.ok response with
    | Some l -> String.split_lines l |> build_guests
    | None ->
      let _ =
        Core.Or_error.iter_error response ~f:(fun e ->
          Brr.Console.(log [ str (Error.to_string_hum e) ]))
      in
      []
  in
  let update_guests =
    let open Bonsai_web.Effect.Let_syntax in
    let%bind guests =
      Bonsai_web.Effect.of_deferred_fun load_guests Magizhchi.Constants.guests_csv
    in
    set_guests guests
  in
  let on_click =
    Attr.on_click (fun _ -> if is_empty_guests then update_guests else Effect.Ignore)
  in
  Vdom.Node.div
    [ Node.button ~attrs:[ on_click ] [ Node.text button_text ]; Form.view_as_vdom form ]
;;

let view_for_form (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let%map guests = populate_guests graph
  and preferences = pref_for_users_assoc graph in
  (* let guests_form = guests_form in *)
  (* let value = Form.value guests_form in *)
  Vdom.Node.div
    [ guests
    ; preferences
      (* ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value) *)
    ]
;;

let _ =
  let open! Magizhchi in
  Start.start view_for_form
;;
