open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module F = Bonsai_web.Effect

let view graph =
  let guests, set_guests = Bonsai.state [||] graph in
  let unit_form = Bonsai.return (Form.return ()) in
  let load_guests =
    let%map set_guests = set_guests in
    let open F.Let_syntax in
    let%bind g_eff = F.of_deferred_fun Prefs.load_guests Magizhchi.Constants.guests_csv in
    set_guests g_eff
  in
  let%map guests = guests
  and _ = Form.Dynamic.with_default_from_effect load_guests unit_form graph in
  Node.div (Array.map ~f:Node.text guests |> Array.to_list)
;;
