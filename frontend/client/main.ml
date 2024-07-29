open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module E = Form.Elements
module F = Bonsai_web.Effect

let view (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let clock = Bonsai.Time_source.create ~start:(Time_ns.now ()) in
  let upd_time, set_upd_time = Bonsai.state (Bonsai.Time_source.now clock) graph in
  let cur_view, set_cur_view = Bonsai.state Utils.Preferences graph in
  let chores_val = Chores.view set_cur_view graph in
  let prefs_val = Prefs.view cur_view set_cur_view set_upd_time graph in
  let guests_val = Guests.view set_cur_view graph in
  let sch_val = Gen_schedule.view set_cur_view graph in
  let%map cur_view = cur_view
  and chores_btn, chores_view = chores_val
  and prefs_btn, prefs_view = prefs_val
  and guests_btn, guests_view = guests_val
  and sch_btn, sch_view = sch_val
  and upd_time = upd_time in
  let main_nodes =
    match cur_view with
    | Chores -> chores_view
    | Preferences -> prefs_view
    | Guests -> guests_view
    | GenSchedule -> sch_view
  in
  let zone = Time_float.Zone.of_utc_offset ~hours:(-7) in
  let time_str = Time_ns.to_string_abs_trimmed ~zone upd_time in
  Vdom.Node.div
    [ Node.div
        ~attrs:[ Attr.class_ "time-row" ]
        [ Node.text ("Last Updated: " ^ time_str) ]
    ; Node.div
        ~attrs:[ Attr.class_ "button-row" ]
        [ prefs_btn; chores_btn; guests_btn; sch_btn ]
    ; main_nodes
    ]
;;

let _ =
  let open! Magizhchi in
  Start.start view
;;
