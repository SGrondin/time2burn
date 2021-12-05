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

let radios ~update state =
  let handler _evt s =
    let data = Sexp.of_string_conv_exn s [%of_sexp: Levels.t option] in
    Local_storage.set_item ~key:storage_key ~data:s
    |> Result.iter_error ~f:(fun err ->
           print_endline
             (sprintf "Could not store to local storage key '%s': '%s'. Error: '%s'" storage_key s err));
    update data
  in
  let make spf =
    let id_ = sprintf !"radio-%{Levels}" spf in
    let data = Some spf in
    let input_attrs =
      let is_selected = [%equal: Levels.t option] state data in
      Attr.
        [
          class_ "form-check-input";
          type_ "radio";
          name "spf-input";
          id id_;
          value (sprintf !"%{sexp: Levels.t option}" data);
        ]
      |> add_if is_selected Attr.checked
    in
    let label_attrs =
      let extras =
        match state, spf with
        | None, Levels.SPF_0 -> Attr.[ class_ "text-primary" ]
        | Some x, y when [%equal: Levels.t] x y -> Attr.[ class_ "fw-bold" ]
        | _ -> []
      in
      merge_attrs [ Attr.[ classes [ "form-check-label"; "text-nowrap" ]; for_ id_ ]; extras ]
    in
    Node.div
      Attr.[ classes [ "form-check" ] ]
      [ Node.input input_attrs []; Node.label label_attrs [ Node.textf !"%{Levels}" spf ] ]
  in
  Node.div
    Attr.[ classes [ "row"; "row-cols-3"; "g-1" ]; style Css_gen.(max_width (`Em 36)); on_change handler ]
    (List.map Levels.all ~f:make)

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
       |> List.cons @@ radios ~update state
       |> Node.div []
     in
     state, node
