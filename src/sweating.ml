open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

module Levels = struct
  type t =
    | Low
    | Medium
    | High
  [@@deriving sexp, equal, enumerate]

  let to_string = function
  | Low -> "None"
  | Medium -> "Some"
  | High -> "Profuse"
end

let storage_key = "sweating"

module Radios = Radios.Make (struct
  include Levels

  let is_bold _ = false
end)

let component =
  let default_model = Local_storage.parse_item storage_key [%of_sexp: Levels.t option] |> Option.join in
  let%sub component = Bonsai.state_opt [%here] ?default_model (module Levels) in
  return
  @@ let%map state, update = component in
     let node =
       []
       |> add_if (Option.is_none state)
            (Node.span []
               [
                 Icon.svg X ~width:2.0 ~height:2.0 ~container:Span Attr.[ class_ "text-danger" ];
                 Node.text "Pick one option";
               ])
       |> List.cons @@ Radios.create ~storage_key ~update state ~columns:3
       |> Node.div []
     in
     state, node
