open! Base
open! Bonsai_web.Cont
module Form = Bonsai_web_ui_form.With_automatic_view
open! Core
open! Bonsai_web.Cont.Bonsai.Let_syntax
open Vdom

module Variant = struct
  type t =
    | A
    | B of int
    | C of string
  [@@deriving typed_variants, sexp_of]

  let form : Bonsai.graph -> t Form.t Bonsai.t =
    Form.Typed.Variant.make
      (module struct
        (* reimport the module that typed_fields just derived *)
        module Typed_variant = Typed_variant

        let label_for_variant = `Inferred
        let initial_choice = `First_constructor

        (* provide a form computation for constructor in the variant *)
        let form_for_variant
          : type a. a Typed_variant.t -> Bonsai.graph -> a Form.t Bonsai.t
          =
          fun typed_field graph ->
          match typed_field with
          | A -> Form.return () |> Bonsai.return
          | B -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph
          | C -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
        ;;
      end)
  ;;
end

let pref_node _k state set_state _graph : Node.t * int =
  let view =
    let is_disabled id = if Int.equal id state then Some Attr.disabled else None in
    Node.div
      [ Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_state 0); Attr.of_opt (is_disabled 0) ]
          [ Node.text "No" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_state 1); Attr.of_opt (is_disabled 1) ]
          [ Node.text "Maybe" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_state 2); Attr.of_opt (is_disabled 2) ]
          [ Node.text "Yes" ]
      ]
  in
  view, state
;;

let pref_for_users_assoc (graph : Bonsai.graph) : Node.t Bonsai.t =
  let tasks = [ "Task1", 1; "Task2", 0; "Task3", 2 ] in
  let tasks =
    List.map
      ~f:(fun (t, i) ->
        let create_state g =
          let s, set_state = Bonsai.state i g in
          Bonsai.both s set_state
        in
        t, create_state)
      tasks
    |> Map.of_alist_exn (module String)
  in
  let tasks = Bonsai.all_map tasks graph in
  let counters =
    Bonsai.assoc
      (module String)
      tasks
      ~f:(fun k v graph ->
        let%map state_out = v
        and k = k in
        let init, set_state = state_out in
        pref_node k init set_state graph)
      graph
  in
  let%map counters = counters in
  let task_state = Map.to_alist counters |> List.map ~f:(fun (s, (_n, c)) -> s, c) in
  Node.div
    [ Node.table
        (counters
         |> Map.to_alist
         |> List.map ~f:(fun (key, (vdom, _)) ->
           let open Node in
           let name = td [ text key ] in
           let counter = td [ vdom ] in
           tr [ name; counter ]))
    ; Node.sexp_for_debugging ([%sexp_of: (string * int) list] task_state)
    ]
;;

let view_for_form (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let%map form =
    Form.Elements.Dropdown.list
      ~init:`First_item
      (module String)
      ~equal:[%equal: String.t]
      (Bonsai.return (List.map guest_prefs ~f:(fun (g, _h) -> g)))
      graph
  and counter = pref_for_users_assoc graph in
  let form = form in
  let value = Form.value form in
  Vdom.Node.div
    [ Form.view_as_vdom form (* ; Form.view_as_vdom dd_form *)
    ; counter
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value)
    ]
;;

let build_guests guest_prefs =
  let make_cols = function
    | c0 :: c1 :: _ -> Some (c0, Int.of_string c1)
    | _ -> None
  in
  List.map guest_prefs ~f:(String.split ~on:',') |> List.filter_map ~f:make_cols
;;

let _ =
  let open Magizhchi in
  Start.start view_for_form
;;
