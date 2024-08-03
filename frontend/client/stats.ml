open! Base
open! Bonsai_web.Cont
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module F = Bonsai_web.Effect
open! Js_of_ocaml

let vega_div_id = "viz"
let vega_div = "#" ^ vega_div_id
let vega_obj_key = "VEGA_DEBUG"

let set_visibility ~visible =
  let div_obj =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "document.getElementById")
      [| Js.Unsafe.inject (Js.string vega_div_id) |]
  in
  let v = if visible then Jv.of_string "visible" else Jv.of_string "hidden" in
  let div_obj = Jv.get div_obj "style" in
  Jv.set div_obj "visibility" v
;;

let handle_spec_change s =
  let json_spec =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "JSON.parse")
      [| Js.Unsafe.inject (Js.string s) |]
  in
  let vega_embed_opts =
    Jv.obj [| "renderer", Jv.of_string "svg"; "actions", Jv.of_bool false |]
  in
  let vpromise =
    Jv.call Jv.global "vegaEmbed" [| Jv.of_string vega_div; json_spec; vega_embed_opts |]
  in
  let attach_to_global view = Jv.set Jv.global vega_obj_key view in
  let fut_or_err =
    Fut.of_promise'
      ~ok:(fun a ->
        Brr.Console.(log [ str "vega_loaded"; a ]);
        attach_to_global a;
        a)
      ~error:(fun e -> Brr.Console.(log [ e ]))
      vpromise
  in
  Fut.await fut_or_err Fn.ignore
;;

module Form = Bonsai_web_ui_form.With_automatic_view

let stats_btn _cur_view (set_cur_view, set_upd_time, set_vega) graph =
  let is_run, set_run = Bonsai.state false graph in
  let%map is_run = is_run
  and set_run = set_run
  and set_cur_view = set_cur_view
  and set_upd_time = set_upd_time
  and set_vega = set_vega in
  let open F.Let_syntax in
  let task_run task_name =
    let%map result = F.of_deferred_fun Async_js.Http.post task_name in
    match Core.Or_error.ok result with
    | Some _ -> ()
    | None -> Utils.log_response result
  in
  let recipe_call task_name =
    let%bind recipe =
      F.of_deferred_fun Async_js.Http.get ("/recipes/" ^ task_name ^ ".vg.json")
    in
    match Core.Or_error.ok recipe with
    | Some recipe -> handle_spec_change recipe |> Core.Time_ns.now |> set_upd_time
    | None -> Utils.log_response recipe |> F.return
  in
  let view_stats _e =
    if is_run
    then F.Ignore
    else (
      let task_name = "top_tasks" in
      F.all_unit
        [ set_run true
        ; task_run task_name
        ; recipe_call task_name
        ; set_vega true
        ; set_cur_view Utils.Stats
        ; set_run false
        ])
  in
  Node.button ~attrs:[ Attr.on_click view_stats ] [ Node.text "View Stats" ]
;;

let view cur_view setters graph =
  let%map btn = stats_btn cur_view setters graph in
  btn, Node.None
;;
