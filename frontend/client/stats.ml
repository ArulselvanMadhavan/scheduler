open! Base
open! Bonsai_web.Cont
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module F = Bonsai_web.Effect

let stats_btn set_cur_view =
  let view_stats _e = set_cur_view Utils.Stats in
  Node.button ~attrs:[ Attr.on_click view_stats ] [ Node.text "View Stats" ]
;;

let view set_cur_view _graph =
  let%map set_cur_view = set_cur_view in
  let btn = stats_btn set_cur_view in
  btn, Node.text "top_tasks"
;;
