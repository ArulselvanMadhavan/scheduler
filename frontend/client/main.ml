open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module E = Form.Elements
module Eff = Bonsai_web.Effect

let _pref_node k state set_state _graph : Node.t * int =
  let view =
    let is_disabled id = if Int.equal id state then Some Attr.disabled else None in
    Node.div
      [ Node.text k
      ; Node.button
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

let pref_node prefs set_state idx display_k (k, v) =
  let view =
    let is_disabled id = if Int.equal id v then Some Attr.disabled else None in
    let upd_state v =
      let new_arr = Array.copy prefs in
      Array.set new_arr idx (k, v);
      set_state new_arr
    in
    Node.div
      ~attrs:[ Attr.class_ "task-pref-row" ]
      [ Node.label [ Node.text display_k ]
      ; Node.div
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
  let open Eff.Let_syntax in
  let open Magizhchi in
  match Core.Or_error.ok guest with
  | Some guest ->
    let get_pref = Fn.compose Async_js.Http.get Constants.guest_pref_dir in
    let%bind response = Eff.of_deferred_fun get_pref guest in
    set_prefs (handle_pref response)
  | None ->
    Brr.Console.(log [ str guest; str "failure_tasks" ]);
    let _ =
      Core.Or_error.iter_error guest ~f:(fun err -> Brr.Console.(log [ str err ]))
    in
    Eff.Ignore
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

let load_chores path =
  let build_chore_spec line_id line =
    let xs = String.split ~on:',' line |> List.map ~f:Base.String.strip in
    match xs with
    | [ ch; h; p ] -> Some (ch, Float.of_string h, Int.of_string p)
    | _ ->
      Brr.Console.(log [ str (Int.to_string line_id); str "Parse failed"; str line ]);
      None
  in
  let open Async_kernel.Deferred.Let_syntax in
  let%map response = Async_js.Http.get path in
  match Core.Or_error.ok response with
  | Some l ->
    String.split_lines l
    |> Fn.flip List.drop 1
    |> List.filter_mapi ~f:build_chore_spec
    |> Array.of_list
  | None ->
    Utils.log_response response;
    [||]
;;

let load_guests path =
  let open Async_kernel.Deferred.Let_syntax in
  let%map response = Async_js.Http.get path in
  match Core.Or_error.ok response with
  | Some l -> String.split_lines l |> Fn.flip List.drop 1 |> build_guests |> Array.of_list
  | None ->
    Utils.log_response response;
    [||]
;;

let build_dd_on_change guests set_prefs =
  let%map set_prefs = set_prefs
  and guests = guests in
  let on_change =
    Attr.on_change (fun _e idx ->
      fetch_tasks set_prefs (Core.Or_error.return guests.(Int.of_string idx)))
  in
  [ on_change; Attr.class_ "guest-select" ]
;;

type view =
  | Guests
  | Preferences
  | Chores

let is_chores_view = function
  | Chores -> true
  | _ -> false

let view (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let guests, set_guests = Bonsai.state [||] graph in
  let prefs, set_prefs = Bonsai.state [||] graph in
  let cur_view, set_cur_view = Bonsai.state Guests graph in
  let chores, set_chores = Bonsai.state [||] graph in
  let arr_to_list g =
    let%map g = g in
    Array.to_list g
  in
  let%map form =
    Form.Elements.Dropdown.list
      ~init:`First_item
      ~extra_attrs:(build_dd_on_change guests set_prefs)
      (module String)
      ~equal:[%equal: String.t]
      (arr_to_list guests)
      graph
  and guests = guests
  and set_guests = set_guests
  and prefs = prefs
  and set_prefs = set_prefs
  and cur_view = cur_view
  and set_cur_view = set_cur_view
  and chores = chores
  and set_chores = set_chores in
  let is_empty_guests = Int.equal (Array.length guests) 0 in
  let update_guests () =
    let open Eff.Let_syntax in
    let%bind guests = Eff.of_deferred_fun load_guests Magizhchi.Constants.guests_csv in
    Eff.all [ set_guests guests; fetch_tasks set_prefs (Core.Or_error.return guests.(0)) ]
    |> Eff.ignore_m
  in
  let save_prefs prefs guest _ =
    let open Eff.Let_syntax in
    let open Magizhchi in
    match Core.Or_error.ok guest with
    | Some guest ->
      let lines =
        Array.map prefs ~f:(fun (t, p) -> Printf.sprintf "%s,%d" t p) |> Array.to_list
      in
      let lines =
        String.concat ~sep:"," Constants.preferences_header :: lines
        |> String.concat ~sep:"\n"
      in
      let body = Async_js.Http.Post_body.String lines in
      let post_call q = Async_js.Http.post ~body (Constants.guest_pref_dir q) in
      let%map _ = Eff.of_deferred_fun post_call guest in
      ()
    | _ -> Eff.Ignore
  in
  let load_guests =
    Attr.on_click (fun _ -> if is_empty_guests then update_guests () else Eff.Ignore)
  in
  let group_tasks prefs =
    let days_count = Array.length Magizhchi.Constants.days in
    let mat = Array.create ~len:days_count [] in
    Array.iteri prefs ~f:(fun i (t, p) ->
      let t_parts = String.split ~on:'_' t in
      let day = List.hd_exn t_parts in
      Option.iter (Map.find Magizhchi.Constants.days_dict day) ~f:(fun day_idx ->
        let xs = mat.(day_idx) in
        mat.(day_idx) <- (i, t, String.concat ~sep:"_" (List.tl_exn t_parts), p) :: xs));
    mat
  in
  let build_pref_nodes prefs_by_day =
    Array.foldi prefs_by_day ~init:[] ~f:(fun i acc day_tasks ->
      let day_tasks = List.rev day_tasks in
      let xs =
        List.map day_tasks ~f:(fun (idx, k, display_k, v) ->
          pref_node prefs set_prefs idx display_k (k, v))
      in
      let day = Magizhchi.Constants.days.(i) in
      let day = Node.div ~attrs:[ Attr.class_ "day-header" ] [ Node.text day ] in
      let tasks = Node.div ~attrs:[ Attr.class_ "tasks-col" ] xs in
      let result = Node.div ~attrs:[ Attr.class_ "day-col" ] [ day; tasks ] in
      result :: acc)
  in
  let nodes = build_pref_nodes (group_tasks prefs) |> List.rev in
  let pref_nodes = Node.div ~attrs:[ Attr.class_ "day-row" ] nodes in
  let chores_click cur_view _e =
    let open Eff.Let_syntax in
    match cur_view with
    | Chores ->
      let%bind _ = Chores.save_chores chores in
      set_cur_view Preferences
| _ ->
      let%bind ch = Eff.of_deferred_fun load_chores Magizhchi.Constants.misc_chores_csv in
      Eff.Many [ set_chores ch; set_cur_view Chores ]
  in
  let main_nodes = match cur_view with
    | Chores -> Chores.view set_chores chores
    | Preferences -> pref_nodes
    | Guests -> Guests.view set_guests guests
  in
  Vdom.Node.div
    [ Node.div
        ~attrs:[ Attr.class_ "button-row" ]
        [ Node.button ~attrs:[ load_guests ] [ Node.text "Load Guests" ]
        ; Node.button
            ~attrs:
              [ Attr.on_click (save_prefs prefs (Form.value form))
              ; (if Array.is_empty guests then Attr.disabled else Attr.empty)
              ]
            [ Node.text "Save Preferences" ]
        ; Node.button
            ~attrs:[ Attr.on_click (chores_click cur_view)]
            [ (if is_chores_view cur_view then Node.text "Save chores" else Node.text "Edit chores") ]
        ]
    ; Form.view_as_vdom form
    ; main_nodes
    ]
;;

let _ =
  let open! Magizhchi in
  Start.start view
;;
