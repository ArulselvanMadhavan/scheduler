open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open! Vdom
module E = Form.Elements

let tbox_attrs (type_ : [ `Text | `Number ]) v =
  let to_string = function
    | `Text -> "text"
    | `Number -> "number"
  in
  [ Attr.type_ (to_string type_); Attr.value v ]
;;

let num_attrs ~step ~min ~max =
  let step_attr = Attr.create "step" step in
  let min_attr = Attr.min min in
  let max_attr = Attr.max max in
  [ step_attr; min_attr; max_attr ]
;;

let view ch =
  let chore_edit _i (c, h, p) =
    let h = Float.to_string_hum ~decimals:2 h in
    let p = Int.to_string p in
    let h_attrs =
      tbox_attrs `Number h @ num_attrs ~step:(Float.to_string 0.5) ~min:0.0 ~max:4.0
    in
    let p_attrs =
      tbox_attrs `Number p @ num_attrs ~step:(Float.to_string 1.) ~min:0.0 ~max:2.0
    in
    let c_attrs = tbox_attrs `Text c in
    Node.div
      (List.map ~f:(fun attrs -> Node.input ~attrs ()) [ c_attrs; h_attrs; p_attrs ])
  in
  Node.div (List.mapi ch ~f:chore_edit)
;;
