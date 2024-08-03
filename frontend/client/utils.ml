open! Base
open! Bonsai_web.Cont
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module F = Bonsai_web.Effect

type view =
  | Guests
  | Preferences
  | Chores
  | Stats
  | GenSchedule

let is_chores_view = function
  | Chores -> true
  | _ -> false
;;

let make_btn ~text ~state ~on_click =
  let is_edit, set_edit = state in
  let on_edit, on_save = on_click in
  let save_btn _ =
    if is_edit
    then F.all_unit [ on_edit (); set_edit false ]
    else F.all_unit [ on_save (); set_edit true ]
  in
  let text = if is_edit then "Edit " ^ text else "Save " ^ text in
  Node.button ~attrs:[ Attr.on_click save_btn ] [ Node.text text ]
;;

let log_response response =
  Core.Or_error.iter_error response ~f:(fun e ->
    Brr.Console.(log [ str (Error.to_string_hum e) ]))
;;
