open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module F = Bonsai_web.Effect

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

let view graph =
  let guests, set_guests = Bonsai.state [||] graph in
  let unit_form = Bonsai.return (Form.return ()) in
  let load_guests =
    let%map set_guests = set_guests in
    let open F.Let_syntax in
    let%bind g_eff = F.of_deferred_fun load_guests Magizhchi.Constants.guests_csv in
    set_guests g_eff
  in
  let%map guests = guests
  and set_guests = set_guests
  and _ = Form.Dynamic.with_default_from_effect load_guests unit_form graph in
  let on_del i _e =
    let before = Array.slice guests 0 i in
    let after = Array.slice guests (i + 1) (Array.length guests) in
    let arr = Array.append before after in
    set_guests arr
  in
  let row i (g, h) =
    let h = Printf.sprintf "%.2f" h in
    let attrs = Chores.(tbox_attrs `Number h @ num_attrs ~step:0.5 ~min:0. ~max:4.0) in
    Node.div
      [ Node.text g
      ; Node.input ~attrs ()
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
  Node.div out
;;
