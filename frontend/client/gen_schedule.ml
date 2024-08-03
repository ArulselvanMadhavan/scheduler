open! Base
open! Bonsai_web.Cont
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open! Vdom
module F = Bonsai_web.Effect
open! Magizhchi

let view (set_cur_view, _, set_vega) graph =
  let is_inprog, set_inprog = Bonsai.state false graph in
  let is_done, set_done = Bonsai.state false graph in
  let%map is_inprog = is_inprog
  and set_inprog = set_inprog
  and is_done = is_done
  and set_done = set_done
  and set_cur_view = set_cur_view
  and set_vega = set_vega in
  let disabled_attr = if is_inprog then Attr.disabled else Attr.empty in
  let on_click _e =
    let open F.Let_syntax in
    let%bind _ = set_inprog true in
    let%bind resp = F.of_deferred_fun Async_js.Http.get Constants.scheduler in
    match Core.Or_error.ok resp with
    | Some resp ->
      let resp = Bool.of_string resp in
      let view_eff = if resp then set_cur_view Utils.GenSchedule else F.Ignore in
      F.Many [ set_done resp; set_inprog false; view_eff; set_vega false ]
    | None -> F.Ignore
  in
  let on_click = Attr.on_click on_click in
  let attrs = [ on_click; disabled_attr ] in
  let btn = Node.button ~attrs [ Node.text "Generate Schedule" ] in
  let view =
    if is_done
    then
      Node.div
        ~attrs:[ Attr.class_ "download-link" ]
        [ Node.a ~attrs:[ Attr.href "chores.xlsx" ] [ Node.text "Download" ] ]
    else Node.None
  in
  btn, view
;;
