open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

module Fitzpatrick = struct
  type t =
    | I
    | II
    | III
    | IV
    | V
    | VI
  [@@deriving sexp, equal, enumerate, variants]

  let to_string = sprintf !"Type %{Variants.to_name}"

  let tone = function
  | I -> "#F5D0B3"
  | II -> "#E7B592"
  | III -> "#D19F7F"
  | IV -> "#BB7955"
  | V -> "#A55E31"
  | VI -> "#3A1F1C"

  let subtitle = function
  | I -> "Light, Pale White"
  | II -> "White, Fair"
  | III -> "Medium, White to Olive"
  | IV -> "Olive, Brown"
  | V -> "Brown, Dark Brown"
  | VI -> "Very Dark, Brown to Black"

  let text = function
  | I -> "Always burns, never tans"
  | II -> "Usually burns, tans with difficulty"
  | III -> "Sometimes mild burns, gradually tans to olive"
  | IV -> "Rarely burns, tans with ease to a moderate brown"
  | V -> "Very rarely burns, tans very easily"
  | VI -> "Never burns, tans very easily, deeply pigmented"

  let coeff = function
  | I -> 5.0
  | II -> 10.0
  | III -> 15.0
  | IV -> 25.0
  | V -> 45.0
  | VI -> 90.0
end

let storage_key = "skin-type"

let render_card ~update state fitz =
  let data = Some fitz in
  let handler _evt =
    let s = sprintf !"%{sexp: Fitzpatrick.t option}" data in
    Local_storage.set_item ~key:storage_key ~data:s
    |> Result.iter_error ~f:(fun err ->
           print_endline
             (sprintf "Could not store to local storage key '%s': '%s'. Error: '%s'" storage_key s err));
    update data
  in
  let is_selected = [%equal: Fitzpatrick.t option] state data in
  let card_attrs =
    match is_selected with
    | true ->
      Attr.
        [
          classes [ "card"; "flex-fill" ];
          style
            (Css_gen.create ~field:"box-shadow"
               ~value:"inset 0rem 0rem 1rem #706666, inset 0rem 0rem 8rem #e7e7e7");
        ]
    | false -> Attr.[ classes [ "card"; "flex-fill"; "shadow-sm" ] ]
  in
  let content =
    Node.div
      Attr.[ classes [ "card-body"; "px-1"; "py-2"; "d-flex"; "flex-column"; "justify-content-around" ] ]
      [
        Node.div Attr.[ classes [ "card-text"; "small" ] ] [ Node.text (Fitzpatrick.subtitle fitz) ];
        Node.div Attr.[ classes [ "card-text"; "small" ] ] [ Node.text (Fitzpatrick.text fitz) ];
      ]
  in
  let badge =
    let nodes =
      []
      |> add_if is_selected
           (Icon.svg Check_lg ~container:Span Attr.[ classes [ "ms-1"; "text-success" ] ])
      |> List.cons @@ Node.text (Fitzpatrick.to_string fitz)
    in
    Node.span
      Attr.[ classes [ "px-1"; "ms-1"; "bg-light"; "rounded"; "rounded-sm" ] ]
      [
        Node.div
          Attr.
            [
              classes [ "d-inline-flex"; "justify-content-center" ];
              style Css_gen.(width (`Em_float (if is_selected then 4.8 else 3.4)));
            ]
          nodes;
      ]
  in
  Node.div
    Attr.[ classes [ "col"; "d-flex"; "flex-column"; "flex-fill" ]; on_click handler; style pointer ]
    [
      Node.div card_attrs
        [
          Node.div
            Attr.
              [
                style
                  Css_gen.(
                    background_color (`Hex (Fitzpatrick.tone fitz))
                    @> border_radius (`Raw "0.2rem 0.2rem 0 0"));
              ]
            [ badge ];
          content;
        ];
    ]

let component =
  let default_model =
    Local_storage.parse_item storage_key [%of_sexp: Fitzpatrick.t option] |> Option.join
  in
  let%sub component = Bonsai.state_opt [%here] ?default_model (module Fitzpatrick) in
  return
  @@ let%map state, update = component in
     let node =
       []
       |> add_if (Option.is_none state)
            (Node.span []
               [
                 Icon.svg X ~width:2.0 ~height:2.0 ~container:Span Attr.[ class_ "text-danger" ];
                 Node.text "Pick one skin type";
               ])
       |> List.cons
          @@ Node.div
               Attr.[ classes [ "row"; "row-cols-3"; "g-1" ]; style Css_gen.(max_width (`Em 36)) ]
               (List.map Fitzpatrick.all ~f:(render_card ~update state))
       |> Node.div []
     in
     state, node
