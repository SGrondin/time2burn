open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

module Levels = struct
  type t =
    | SPF_0
    | SPF_15
    | SPF_30
    | SPF_50
    | SPF_60
    | SPF_80
    | SPF_100
  [@@deriving sexp, equal, enumerate]

  let is_zero = function
  | SPF_0 -> true
  | _ -> false

  let to_string = function
  | SPF_0 -> "None"
  | SPF_15 -> "SPF 15"
  | SPF_30 -> "SPF 30"
  | SPF_50 -> "SPF 50"
  | SPF_60 -> "SPF 60"
  | SPF_80 -> "SPF 80"
  | SPF_100 -> "SPF 100"

  let to_coeff = function
  | SPF_0 -> 1.0
  | SPF_15 -> 15.0
  | SPF_30 -> 30.0
  | SPF_50 -> 50.0
  | SPF_60 -> 60.0
  | SPF_80 -> 80.0
  | SPF_100 -> 100.0
end

let storage_key = "spf"

module Radios = Radios.Make (struct
  include Levels

  let is_bold = is_zero
end)

let component =
  let default_model = Local_storage.parse_item storage_key [%of_sexp: Levels.t option] |> Option.join in
  let%sub component = Bonsai.state_opt [%here] ?default_model (module Levels) in
  let%sub sweating = Sweating.component in
  return
  @@ let%map state, update = component
     and sweating_data, sweating_node = sweating in
     let node =
       []
       |> add_if
            (Option.value_map state ~default:false ~f:(fun x -> not (Levels.is_zero x)))
            (Node.div []
               [ Node.h5 Attr.[ classes [ "mt-3"; "ms-2" ] ] [ Node.text "Sweating" ]; sweating_node ])
       |> add_if (Option.is_none state)
            (Node.span []
               [
                 Icon.svg X ~width:2.0 ~height:2.0 ~container:Span Attr.[ class_ "text-danger" ];
                 Node.text "Pick one option";
               ])
       |> List.cons @@ Radios.create ~storage_key ~update state ~columns:3
       |> Node.div []
     in
     state, sweating_data, node
