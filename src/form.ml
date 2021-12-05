open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

type data = {
  weather: Weather.t;
  skin_type: Skin_type.Fitzpatrick.t;
  spf: Spf.Levels.t;
}
[@@deriving sexp]

type subforms = {
  geo_node: Node.t;
  skin_type_node: Node.t;
  spf_node: Node.t;
  data: data option;
}

let subforms =
  let%sub geo = Geo.component in
  let%sub skin_type = Skin_type.component in
  let%sub spf = Spf.component in
  return
  @@ let%map geo_data, geo_node = geo
     and skin_type_data, skin_type_node = skin_type
     and spf_data, spf_node = spf in
     let data =
       match geo_data, skin_type_data, spf_data with
       | Some weather, Some skin_type, Some spf -> Some { weather; skin_type; spf }
       | _ -> None
     in
     { geo_node; skin_type_node; spf_node; data }

type slice = {
  dt: Weather.DT.t;
  uvi: float;
}
[@@deriving sexp, equal]

type computed = {
  slice: slice;
  cost: float;
  total_at_start: float;
}
[@@deriving sexp, equal]

let how_long_rev ~num_slices { weather; skin_type; spf } =
  let slice_minutes = 60 // num_slices in
  let ( +* ) time = function
    | 0 -> time
    | n -> Time.add time (Time.Span.of_min Float.(of_int n * slice_minutes))
  in
  let skin_type = Skin_type.Fitzpatrick.coeff skin_type in
  let spf = Spf.Levels.to_coeff spf in
  let slices =
    let rec loop acc : Weather.hourly list -> slice list = function
      | []
       |[ _ ] ->
        acc
      | x :: (y :: _ as rest) ->
        let open Float in
        let ll =
          List.init num_slices ~f:(fun i ->
              let gradient = of_int i * (1 // 12) in
              { dt = x.dt +* i; uvi = (x.uvi * (1.0 - gradient)) + (y.uvi * gradient) })
          |> List.rev_filter ~f:(fun { dt; _ } -> Time.(dt >= weather.current.dt))
        in
        loop (ll @ acc) rest
    in
    loop [] (List.take weather.hourly 13) |> List.rev
  in
  let going_outside = List.hd slices |> Option.map ~f:(fun { dt; _ } -> dt) in
  let num_points, points =
    List.fold_until slices ~init:(0.0, 0, [])
      ~finish:(fun (_, n, ll) -> n, ll)
      ~f:(fun (total, n, ll) slice ->
        let open Float in
        let would_be_100 = skin_type * 8.0 / max 1.0 slice.uvi * spf in
        let cost = slice_minutes * 100.0 / would_be_100 in
        let next = { slice; cost; total_at_start = total } in
        if total >= 100.0 then Stop (succ n, next :: ll) else Continue (total + cost, succ n, next :: ll))
  in
  going_outside, num_points, points

let best_fit_how_long_rev data =
  let rec loop = function
    | [ num_slices ] ->
      let going_outside, _, points = how_long_rev ~num_slices data in
      going_outside, points
    | num_slices :: rest ->
      let going_outside, num_points, points = how_long_rev ~num_slices data in
      if num_points <= 26 then going_outside, points else loop rest
    | [] -> failwith "Impossible case, there should always be hourly values"
  in
  loop [ 30; 12; 6; 4 ]

let component =
  let%sub subforms = subforms in
  let module Component = struct
    module Input = struct
      type t = subforms
    end

    module Model = struct
      type t = unit [@@deriving sexp, equal]
    end

    module Action = Unit

    let name = Source_code_position.to_string [%here]

    let apply_action ~inject:_ ~schedule_event:_ (_subforms : Input.t) (prev : Model.t) _action = prev

    let get_results data =
      let format_info = function
        | [] -> None
        | ll -> List.map ll ~f:(fun s -> Node.li [] [ Node.text s ]) |> Node.ul [] |> Option.return
      in
      Option.map data ~f:(fun subform_data ->
          let going_outside, points = best_fit_how_long_rev subform_data in
          let ttb =
            match going_outside, points with
            | Some _, { total_at_start; _ } :: _ when Float.( < ) total_at_start 95.0 ->
              let alert =
                Node.div
                  Attr.[ classes [ "alert"; "alert-success" ]; style Css_gen.(max_width (`Px 650)) ]
                  [ Node.text "You should be fine!" ]
              in
              let info =
                match subform_data.spf with
                | Spf.Levels.SPF_0 -> []
                | _ ->
                  [
                    "Make sure to reapply sunscreen every 2 hours";
                    "Reapply sunscreen after swimming or excessive sweating";
                    "With these precautions you can spend the rest of the day out in the sun, enjoy! \
                     ☀️";
                  ]
              in
              [ Some alert; format_info info ] |> List.filter_opt |> Node.div []
            | Some start, _ :: { slice = { dt = burn; _ }; _ } :: _ ->
              let alert =
                Node.div
                  Attr.[ classes [ "alert"; "alert-warning" ]; style Css_gen.(max_width (`Px 650)) ]
                  [
                    Node.textf
                      !"If you go in the sun at %{Weather.DT}, you will have a sunburn at around "
                      start;
                    Node.span Attr.[ class_ "fw-bold" ] [ Node.textf !"%{Weather.DT}" burn ];
                  ]
              in
              let info =
                match subform_data.spf with
                | Spf.Levels.SPF_0 -> [ "You should try again with sunscreen" ]
                | Spf.Levels.SPF_100 -> [ "Limit your time in the sun today" ]
                | _ -> [ "Try using a stronger sunscreen or limit your time in the sun today" ]
              in
              [ Some alert; format_info info ] |> List.filter_opt |> Node.div []
            | _ -> Node.none
          in
          let labels, data =
            List.fold points ~init:([], []) ~f:(fun (labels, pct) computed ->
                Weather.DT.to_string computed.slice.dt :: labels, computed.total_at_start :: pct)
          in
          Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
              let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.1 in
              let open Js_of_ocaml in
              (Dom_html.getElementById_opt "results" |> function
               | None -> print_endline "Error, could not find 'results'"
               | Some el -> (Js.Unsafe.coerce el)##scrollIntoView);
              Lwt.return_unit);
          [ ttb; Chart.render ~labels ~data ~key:(sprintf !"%{sexp: data}" subform_data) ] |> Node.div [])

    let compute ~inject:_ ({ geo_node; skin_type_node; spf_node; data } : Input.t) (_model : Model.t) =
      let make_section ?id:id_ ~title ?subtitle nodes =
        let attrs = Attr.[ class_ "pb-1" ] |> add_opt id_ ~f:Attr.id in
        nodes
        |> add_opt subtitle ~f:(fun s -> Node.h6 Attr.[ class_ "pb-1" ] [ Node.text s ])
        |> List.cons @@ Node.h5 attrs [ Node.text title ]
        |> Node.div Attr.[ classes [ "my-4" ] ]
      in
      []
      |> add_opt (get_results data) ~f:(fun node -> make_section ~id:"results" ~title:"Result" [ node ])
      |> List.cons
         @@ make_section ~title:"3. Location" ~subtitle:"Used for cloud coverage and the angle of the sun"
              [ geo_node ]
      |> List.cons @@ make_section ~title:"2. Sunscreen" [ spf_node ]
      |> List.cons
         @@ make_section ~title:"1. Fitzpatrick skin scale"
              ~subtitle:"Your skin's sensitivity to UV. Click on one." [ skin_type_node ]
      |> Node.div []

    module Result = Node
  end in
  Bonsai.of_module1 (module Component) ~default_model:() subforms
