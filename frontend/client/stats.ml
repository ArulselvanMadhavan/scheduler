open! Base
open! Bonsai_web.Cont
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom
module F = Bonsai_web.Effect
open! Js_of_ocaml

let vega_div = "#viz"
let vega_obj_key = "VEGA_DEBUG"

let handle_spec_change s =
  let json_spec =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "JSON.parse")
      [| Js.Unsafe.inject (Js.string s) |]
  in
  let vega_embed_opts = Jv.obj [| "renderer", Jv.of_string "svg" |] in
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
  (* Fut.await fut_or_err (fun a -> Brr.Console.(log [ str "await complete"; a ])) *)
  Fut.await fut_or_err Fn.ignore

let stats_btn set_cur_view set_upd_time =
  let open F.Let_syntax in
  let task_run task_name =
    let%map result = F.of_deferred_fun (Async_js.Http.post) task_name in
    match Core.Or_error.ok result with
    | Some _ -> ()
    | None -> Utils.log_response result
  in
  let recipe_call task_name =
  let%bind recipe = F.of_deferred_fun (Async_js.Http.get) ("/recipes/" ^ task_name ^".vg.json") in
  (match (Core.Or_error.ok recipe) with
   | Some recipe ->  handle_spec_change recipe |> Core.Time_ns.now |> set_upd_time
   | None -> Utils.log_response recipe |> F.return)
  in
  let view_stats _e =
    (* Make a request to run on server *)
    (* Get 200 OK *)
    (* Fetch recipe *)
    (* Load recipe *)
    let task_name = "top_tasks" in
    F.all_unit [task_run task_name; recipe_call task_name; set_cur_view Utils.Stats]
  in
  Node.button ~attrs:[ Attr.on_click view_stats ] [ Node.text "View Stats" ]
;;

let view set_cur_view set_upd_time _graph =
  let%map set_cur_view = set_cur_view
  and set_upd_time = set_upd_time in
  let btn = stats_btn set_cur_view set_upd_time in
  btn, Node.None
;;
