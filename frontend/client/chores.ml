open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open! Vdom
module E = Form.Elements
module F = Bonsai_web.Effect
open! Magizhchi

let chores_hdr = String.concat ~sep:"," Constants.chores_header

let save_chores chores =
  let open F.Let_syntax in
  let to_row (c, h, p) = Printf.sprintf "%s,%.2f,%d" c h p in
  let lines =
    Array.map ~f:to_row chores
    |> Array.to_list
    |> List.cons chores_hdr
    |> String.concat ~sep:"\n"
  in
  let body = Async_js.Http.Post_body.String lines in
  let post_call q = Async_js.Http.post ~body q in
  let%map post_resp = F.of_deferred_fun post_call Constants.misc_chores_csv in
  Utils.log_response post_resp;
  ()
;;

let tbox_attrs (type_ : [ `Text | `Number ]) v =
  let to_string = function
    | `Text -> "text"
    | `Number -> "number"
  in
  [ Attr.type_ (to_string type_); Attr.value v ]
;;

let num_attrs ~step ~min ~max =
  let step_attr = Attr.create "step" (Float.to_string step) in
  let min_attr = Attr.min min in
  let max_attr = Attr.max max in
  [ step_attr; min_attr; max_attr ]
;;

let view set_chores chores =
  let chore_edit i (c, h, p) =
    let h = Float.to_string_hum ~decimals:2 h in
    let p = Int.to_string p in
    let on_change (type_ : [ `Chore | `Hour | `Prio ]) _e s =
      let chores = Array.copy chores in
      let c, h, p = chores.(i) in
      (match type_ with
       | `Chore -> chores.(i) <- s, h, p
       | `Hour -> chores.(i) <- c, Float.of_string s, p
       | `Prio -> chores.(i) <- c, h, Int.of_string s);
      set_chores chores
    in
    let funs =
      Array.map ~f:(Base.Fn.compose Attr.on_change on_change) [| `Chore; `Hour; `Prio |]
    in
    let h_attrs = tbox_attrs `Number h @ num_attrs ~step:0.5 ~min:0.0 ~max:4.0 in
    let p_attrs = tbox_attrs `Number p @ num_attrs ~step:1. ~min:0.0 ~max:2.0 in
    let c_attrs = tbox_attrs `Text c in
    let attrs = [ c_attrs; h_attrs; p_attrs ] in
    let attrs = List.mapi ~f:(fun i xs -> List.cons funs.(i) xs) attrs in
    let inputs = List.map ~f:(fun attrs -> Node.input ~attrs ()) attrs in
    let on_del _e =
      let before = Array.slice chores 0 i in
      let after = Array.slice chores (i + 1) (Array.length chores) in
      let arr = Array.append before after in
      set_chores arr
    in
    let del = Node.button ~attrs:[ Attr.on_click on_del ] [ Node.text "remove" ] in
    Node.div (List.append inputs [ del ])
  in
  let on_click _e =
    let arr = Array.create ~len:(Array.length chores + 1) ("", 0., 0) in
    Array.iteri chores ~f:(fun i a -> arr.(i) <- a);
    set_chores arr
  in
  let add_new = Node.button ~attrs:[ Attr.on_click on_click ] [ Node.text "Add new" ] in
  let out = Array.mapi chores ~f:chore_edit |> Array.to_list in
  let out = List.append out [ add_new ] in
  Node.div out
;;
