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

let counter graph : Node.t Bonsai.t * int Bonsai.t =
  let count, set_count = Bonsai.state 1 graph in
  let view =
    let%map count = count
    and set_count = set_count in
    let is_disabled id = if Int.equal id count then Some Attr.disabled else None in
    Node.div
      [ Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_count 0); Attr.of_opt (is_disabled 0) ]
          [ Node.text "No" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_count 1); Attr.of_opt (is_disabled 1) ]
          [ Node.text "Maybe" ]
      ; Node.button
          ~attrs:[ Attr.on_click (fun _ -> set_count 2); Attr.of_opt (is_disabled 2) ]
          [ Node.text "Yes" ]
      ]
  in
  view, count
;;

let counters_for_users_assoc (graph : Bonsai.graph) : Node.t Bonsai.t =
  let tasks =
    [ "Task1", (); "Task2", (); "Task3", () ] |> String.Map.of_alist_exn |> Bonsai.return
  in
  let counters =
    Bonsai.assoc
      (module String)
      tasks
      ~f:(fun _k _v graph ->
        let view, _ = counter graph in
        view)
      graph
  in
  let%map counters = counters in
  Node.table
    (counters
     |> Map.to_alist
     |> List.map ~f:(fun (key, vdom) ->
       let open Node in
       let name = td [ text key ] in
       let counter = td [ vdom ] in
       tr [ name; counter ]))
;;

let view_for_form (graph : Bonsai.graph) : Vdom.Node.t Bonsai.t =
  let%map variant_form = Variant.form graph
  and counter = counters_for_users_assoc graph in
  (* let form = Form.both record_form variant_form in *)
  let form = variant_form in
  let value = Form.value form in
  Vdom.Node.div
    [ Form.view_as_vdom form
    ; counter
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: Variant.t Or_error.t] value)
    ]
;;

let _ = Start.start view_for_form
