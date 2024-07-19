open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module E = Form.Elements
module F = Bonsai_web.Effect

let view (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let cur_view, set_cur_view = Bonsai.state Utils.Preferences graph in
  let chores_val = Chores.view set_cur_view graph in
  let prefs_val = Prefs.view cur_view graph in
  let%map cur_view = cur_view
  and set_cur_view = set_cur_view
  and chores_btn, chores_view = chores_val
  and prefs_btn, prefs_view = prefs_val in
  let main_nodes =
    match cur_view with
    | Chores -> chores_view
    | Preferences -> prefs_view
    | Guests -> Guests.view () ()
  in
  Vdom.Node.div
    [ Node.div
        ~attrs:[ Attr.class_ "button-row" ]
        [ prefs_btn
        ; chores_btn
        ; Node.button
            ~attrs:[ Attr.on_click (fun _ -> set_cur_view Guests) ]
            [ Node.text "Guests" ]
        ]
    ; main_nodes
    ]
;;

let _ =
  let open! Magizhchi in
  Start.start view
;;
