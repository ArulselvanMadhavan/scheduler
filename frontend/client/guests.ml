open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module F = Bonsai_web.Effect
open Magizhchi

let guests_and_hours lines =
  let make_elems line =
    match String.split line ~on:',' with
    | [ g; h ] -> Option.map (Float.of_string_opt h) ~f:(fun h -> g, h)
    | _ -> None
  in
  List.filter_map lines ~f:make_elems
;;

let load_guests path =
  let open Async_kernel.Deferred.Let_syntax in
  let%map response = Async_js.Http.get path in
  match Core.Or_error.ok response with
  | Some l ->
    String.split_lines l |> Fn.flip List.drop 1 |> guests_and_hours |> Array.of_list
  | None ->
    Utils.log_response response;
    [||]
;;

let guests_hdr = String.concat ~sep:"," Constants.guests_header

let save_guests guests =
  let open F.Let_syntax in
  let to_row (g, h) = Printf.sprintf "%s,%.2f" g h in
  let lines =
    Array.map ~f:to_row guests
    |> Array.to_list
    |> List.cons guests_hdr
    |> String.concat ~sep:"\n"
  in
  let body = Async_js.Http.Post_body.String lines in
  let post_call q = Async_js.Http.post ~body q in
  let%map post_resp = F.of_deferred_fun post_call Constants.guests_csv in
  Utils.log_response post_resp;
  ()
;;

let check_prefs guests =
  let new_gs = Array.map ~f:(fun (g, _) -> g) guests |> Array.to_list in
  let new_gs_hs = Hash_set.create ~size:(List.length new_gs) (module String) in
  List.iter new_gs ~f:(Hash_set.add new_gs_hs);
  let open F.Let_syntax in
  let%bind response =
    F.of_deferred_fun Async_js.Http.get Constants.guests_with_preferences
  in
  let old_gs =
    match Core.Or_error.ok response with
    | Some l -> String.split l ~on:','
    | None ->
      Utils.log_response response;
      []
  in
  let old_gs_hs = Hash_set.create ~size:(List.length old_gs) (module String) in
  List.iter old_gs ~f:(Hash_set.add old_gs_hs);
  let ins_gs = List.filter new_gs ~f:(fun g -> not (Hash_set.mem old_gs_hs g)) in
  let del_gs = List.filter old_gs ~f:(fun g -> not (Hash_set.mem new_gs_hs g)) in
  let%bind response = F.of_deferred_fun Async_js.Http.get Constants.misc_chores_csv in
  let prefs =
    match Core.Or_error.ok response with
    | Some l ->
      String.split_lines l
      |> List.map ~f:(String.split ~on:',')
      |> List.filter_map ~f:List.hd
      |> List.map ~f:(fun c -> c, 0)
      |> Array.of_list
    | _ -> [||]
  in
  let body =
    List.map ~f:Constants.guest_pref_dir del_gs
    |> String.concat ~sep:","
    |> Async_js.Http.Post_body.String
  in
  let del_gs = F.of_deferred_fun (Async_js.Http.post ~body) Constants.delete_guests in
  let del_gs = F.map del_gs ~f:(Fn.const ()) in
  Brr.Console.(log [ str body ]);
  let ins_gs =
    List.map ins_gs ~f:(fun g -> Prefs.save_prefs prefs (Core.Or_error.return g))
  in
  F.all_unit (del_gs :: ins_gs)
;;

let guests_btn guests set_guests set_cur_view graph =
  let is_edit, set_edit = Bonsai.state true graph in
  let%map guests = guests
  and set_guests = set_guests
  and set_cur_view = set_cur_view
  and is_edit = is_edit
  and set_edit = set_edit in
  let open F.Let_syntax in
  let on_edit () =
    let%bind gs = F.of_deferred_fun load_guests Magizhchi.Constants.guests_csv in
    F.all_unit [ set_guests gs; set_cur_view Utils.Guests ]
  in
  let on_save () =
    F.all_unit [ save_guests guests; check_prefs guests; set_cur_view Utils.Preferences ]
  in
  Utils.make_btn ~text:"Guests" ~state:(is_edit, set_edit) ~on_click:(on_edit, on_save)
;;

let view set_cur_view graph =
  let guests, set_guests = Bonsai.state [||] graph in
  let unit_form = Bonsai.return (Form.return ()) in
  let load_guests =
    let is_inprog, set_inprog = Bonsai.state false graph in
    let%map set_guests = set_guests
    and is_inprog = is_inprog
    and set_inprog = set_inprog in
    let open F.Let_syntax in
    if is_inprog
    then F.return ()
    else (
      let%bind _ = set_inprog true in
      let%bind g_eff = F.of_deferred_fun load_guests Magizhchi.Constants.guests_csv in
      F.all_unit [ set_guests g_eff; set_inprog false ])
  in
  let%map guests = guests
  and set_guests = set_guests
  and btn = guests_btn guests set_guests set_cur_view graph
  and _ = Form.Dynamic.with_default_from_effect load_guests unit_form graph in
  let on_del i _e =
    let before = Array.slice guests 0 i in
    let after = Array.slice guests (i + 1) (Array.length guests) in
    let arr = Array.append before after in
    set_guests arr
  in
  let row i (g, h) =
    let on_change (type_ : [ `Guest | `Hour ]) _e s =
      let guests = Array.copy guests in
      let g, h = guests.(i) in
      (match type_ with
       | `Guest -> guests.(i) <- s, h
       | `Hour -> guests.(i) <- g, Float.of_string s);
      set_guests guests
    in
    let h = Printf.sprintf "%.2f" h in
    let g_attrs = Chores.(tbox_attrs `Text g) @ [ Attr.on_change (on_change `Guest) ] in
    let h_attrs =
      Chores.(tbox_attrs `Number h @ num_attrs ~step:0.5 ~min:0. ~max:4.0)
      @ [ Attr.on_change (on_change `Hour) ]
    in
    Node.div
      [ Node.input ~attrs:g_attrs ()
      ; Node.input ~attrs:h_attrs ()
      ; Node.button ~attrs:[ Attr.on_click (on_del i) ] [ Node.text "remove" ]
      ]
  in
  let guest_nodes = Array.mapi ~f:row guests |> Array.to_list in
  let on_new _e =
    let arr = Array.create ~len:(Array.length guests + 1) ("", 4.) in
    Array.iteri guests ~f:(fun i g -> arr.(i) <- g);
    set_guests arr
  in
  let add_new = Node.button ~attrs:[ Attr.on_click on_new ] [ Node.text "Add new" ] in
  let out = List.append guest_nodes [ add_new ] in
  btn, Node.div out
;;
