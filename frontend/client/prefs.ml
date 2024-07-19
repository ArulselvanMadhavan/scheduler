open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module E = Form.Elements
module F = Bonsai_web.Effect

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
  let open F.Let_syntax in
  let open Magizhchi in
  match Core.Or_error.ok guest with
  | Some guest ->
    let get_pref = Fn.compose Async_js.Http.get Constants.guest_pref_dir in
    let%bind response = F.of_deferred_fun get_pref guest in
    set_prefs (handle_pref response)
  | None ->
    Brr.Console.(log [ str guest; str "failure_tasks" ]);
    let _ =
      Core.Or_error.iter_error guest ~f:(fun err -> Brr.Console.(log [ str err ]))
    in
    F.Ignore
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
    Utils.log_response response;
    [||]
;;

let load_guests_failed = Sexp.of_string "No_Guests_loaded"

let update_guests graph guests set_guests set_prefs =
  let is_inprog, set_inprog = Bonsai.state false graph in
  let%map set_guests = set_guests
  and set_prefs = set_prefs
  and guests = guests
  and is_inprog = is_inprog
  and set_inprog = set_inprog in
  let open F.Let_syntax in
  if Array.is_empty guests
  then
    if is_inprog
    then F.return ()
    else (
      let%bind _ = set_inprog true in
      let%bind guests = F.of_deferred_fun load_guests Magizhchi.Constants.guests_csv in
      let len = Array.length guests in
      Brr.Console.(log [ str (Base.Random.int len) ]);
      let set_pref_eff =
        if Array.is_empty guests
        then F.print_s load_guests_failed
        else fetch_tasks set_prefs (Core.Or_error.return guests.(0))
      in
      F.all [ set_guests guests; set_pref_eff; set_inprog false ] |> F.ignore_m)
  else F.return ()
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

let arr_to_list g =
  let%map g = g in
  Array.to_list g
;;

let save_prefs prefs guest =
  let open F.Let_syntax in
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
    let%map _ = F.of_deferred_fun post_call guest in
    ()
  | _ -> F.Ignore
;;

let prefs_btn cur_view dd_form prefs =
  let%map prefs = prefs
  and cur_view = cur_view
  and dd_form = dd_form in
  let on_save _ =
    if Array.is_empty prefs then F.Ignore else save_prefs prefs (Form.value dd_form)
  in
  let btn_text =
    match cur_view with
    | Utils.Preferences -> "Save"
    | _ -> "Edit"
  in
  let btn_text = btn_text ^ " Preferences" in
  Node.button ~attrs:[ Attr.on_click on_save ] [ Node.text btn_text ]
;;

let view cur_view graph =
  let prefs, set_prefs = Bonsai.state [||] graph in
  let guests, set_guests = Bonsai.state [||] graph in
  let unit_form = Bonsai.return (Form.return ()) in
  let dd_form =
    Form.Elements.Dropdown.list
      ~init:`First_item
      ~extra_attrs:(build_dd_on_change guests set_prefs)
      (module String)
      ~equal:[%equal: String.t]
      (arr_to_list guests)
      graph
  in
  let prefs_btn = prefs_btn cur_view dd_form prefs in
  let%map prefs = prefs
  and set_prefs = set_prefs
  and dd_form = dd_form
  and prefs_btn = prefs_btn
  and _ =
    Form.Dynamic.with_default_from_effect
      (update_guests graph guests set_guests set_prefs)
      unit_form
      graph
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
  let guest_pref_nodes = Node.div ~attrs:[ Attr.class_ "day-row" ] nodes in
  prefs_btn, Node.div [ Form.view_as_vdom dd_form; guest_pref_nodes ]
;;
