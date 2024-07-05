open! Base
open! Bonsai
open! Bonsai_web
open! Async_kernel
open! Async_js
open! Js_of_ocaml
open! Core
module Form = Bonsai_web_ui_form
open Vdom

let task_click task_id _e = Ui_effect.print_s (Sexp.of_string task_id)
let header_div a = Node.text a |> List.singleton |> Node.h3

let text_div a =
  Node.text a |> List.singleton |> Node.div ~attrs:[ Attr.on_click (task_click a) ]
;;

let view_of_form : Vdom.Node.t Computation.t =
  let open! Bonsai.Let_syntax in
  let day_tasks = List.map ~f:text_div [ "task1"; "task2" ] in
  let day_header = header_div "day_of_the_week" in
  let day_column = day_header :: day_tasks in
  let day_div = Node.div ~attrs:[ Attr.id "day"; Attr.class_ "day-column" ] day_column in
  let cal_div = Node.div ~attrs:[ Attr.id "calendar" ] [ day_div ] in
  Bonsai.Computation.return cal_div
;;

let _ = Start.start view_of_form
