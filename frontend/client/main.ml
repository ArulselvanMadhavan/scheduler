open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom

let _pref_node _k state set_state _graph : Node.t * int =
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

let pref_node prefs set_state idx (k, v) =
  let view =
    let is_disabled id = if Int.equal id v then Some Attr.disabled else None in
    let upd_state v =
      let new_arr = Array.copy prefs in
      Array.set new_arr idx (k, v);
      set_state new_arr
    in
    Node.div
      [ Node.button
          ~attrs:[ Attr.on_click (fun _ -> upd_state 0); Attr.of_opt (is_disabled 0) ]
          [ Node.text "No" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> upd_state 1); Attr.of_opt (is_disabled 1) ]
          [ Node.text "Maybe" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> upd_state 2); Attr.of_opt (is_disabled 2) ]
          [ Node.text "Yes" ]
      ]
  in
  view
;;

let handle_pref prefs =
  let split_line line =
    match String.split line ~on:',' with
    | c0 :: c1 :: _ -> Some (c0, Int.of_string c1)
    | _ -> None
  in
  match Core.Or_error.ok prefs with
  | Some prefs ->
    String.split_lines prefs
    |> Fn.flip List.drop 1
    |> List.filter_map ~f:split_line
    |> Array.of_list
  | None -> [||]
;;

let fetch_tasks set_prefs guest =
  let open Bonsai_web.Effect.Let_syntax in
  match Core.Or_error.ok guest with
  | Some guest ->
    let get_pref = Fn.compose Async_js.Http.get (Printf.sprintf "preferences/%s.csv") in
    let%bind response = Bonsai_web.Effect.of_deferred_fun get_pref guest in
    set_prefs (handle_pref response)
  | None ->
    let _ =
      Core.Or_error.iter_error guest ~f:(fun err -> Brr.Console.(log [ str err ]))
    in
    Effect.Ignore
;;

let _pref_for_users_assoc tasks (graph : Bonsai.graph) : Node.t Bonsai.t =
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
  let pref_map =
    Bonsai.assoc
      (module String)
      tasks
      ~f:(fun k v graph ->
        let%map state_out = v
        and k = k in
        let init, set_state = state_out in
        _pref_node k init set_state graph)
      graph
  in
  let%map pref_map = pref_map in
  let task_state = Map.to_alist pref_map |> List.map ~f:(fun (s, (_n, c)) -> s, c) in
  Node.div
    [ Node.table
        (pref_map
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

let load_guests path =
  let open Async_kernel.Deferred.Let_syntax in
  let%map response = Async_js.Http.get path in
  match Core.Or_error.ok response with
  | Some l -> String.split_lines l |> Fn.flip List.drop 1 |> build_guests |> Array.of_list
  | None ->
    let _ =
      Core.Or_error.iter_error response ~f:(fun e ->
        Brr.Console.(log [ str (Error.to_string_hum e) ]))
    in
    [||]
;;

let build_dd_on_change guests set_prefs =
  let%map set_prefs = set_prefs
  and guests = guests in
  let on_change =
    Attr.on_change (fun _e idx ->
      fetch_tasks set_prefs (Core.Or_error.return guests.(Int.of_string idx - 1)))
  in
  [ on_change ]
;;

let view (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let guests, set_guests = Bonsai.state [||] graph in
  let prefs, set_prefs = Bonsai.state [||] graph in
  let arr_to_list g =
    let%map g = g in
    Array.to_list g
  in
  let%map form =
    Form.Elements.Dropdown.list
      ~init:`Empty
      ~extra_attrs:(build_dd_on_change guests set_prefs)
      (module String)
      ~equal:[%equal: String.t]
      (arr_to_list guests)
      graph
  and guests = guests
  and set_guests = set_guests
  and prefs = prefs
  and set_prefs = set_prefs in
  let is_empty_guests = Int.equal (Array.length guests) 0 in
  let selected_guest = Form.value form in
  (* let _ = *)
  (*   Core.Or_error.map selected_guest ~f:(fun g -> *)
  (*     Brr.Console.(log [ str "success"; str g ])) *)
  (* in *)
  (* let _ = *)
  (*   Core.Or_error.iter_error selected_guest ~f:(fun e -> *)
  (*     Brr.Console.(log [ str "err"; str e ])) *)
  (* in *)
  let update_guests () =
    let open Bonsai_web.Effect.Let_syntax in
    let%bind guests =
      Bonsai_web.Effect.of_deferred_fun load_guests Magizhchi.Constants.guests_csv
    in
    Bonsai_web.Effect.all [ set_guests guests; fetch_tasks set_prefs selected_guest ]
    |> Bonsai_web.Effect.ignore_m
  in
  let on_click =
    Attr.on_click (fun _ -> if is_empty_guests then update_guests () else Effect.Ignore)
  in
  let guest_nodes =
    Node.div
      [ Node.button ~attrs:[ on_click ] [ Node.text "Load Guests" ]
      ; Form.view_as_vdom form
      ]
  in
  let nodes = Array.mapi ~f:(pref_node prefs set_prefs) prefs |> Array.to_list in
  let pref_nodes = Node.div nodes in
  Vdom.Node.div
    [ guest_nodes
    ; pref_nodes
    ; Node.sexp_for_debugging ([%sexp_of: (string * int) array] prefs)
    ]
;;

let _ =
  let open! Magizhchi in
  Start.start view
;;
