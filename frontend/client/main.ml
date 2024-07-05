open! Base
open! Bonsai
open! Bonsai_web
open! Async_kernel
open! Async_js
open! Js_of_ocaml
open! Core
module Form = Bonsai_web_ui_form

let view_of_form : Vdom.Node.t Computation.t =
  let open Bonsai_web in
  Bonsai.Computation.return (Vdom.Node.Text "Test")

let _ =
  Start.start view_of_form
;;
