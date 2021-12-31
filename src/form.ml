open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

type time_t = Time.t

let sexp_of_time_t x = Sexp.Atom (Time.to_string x)

let time_t_of_sexp x = [%of_sexp: string] x |> Time.of_string

type data = {
  weather: Weather.t;
  place_name: string;
  now: time_t;
  skin_type: Skin_type.Fitzpatrick.t;
  spf: Spf.Levels.t;
  sweating: Sweating.Levels.t;
}
[@@deriving sexp]

type subforms = {
  geo_node: Node.t;
  skin_type_node: Node.t;
  spf_node: Node.t;
  sweating_node: Node.t;
  data: data option;
}

let subforms =
  let%sub geo = Geo.component in
  let%sub skin_type = Skin_type.component in
  let%sub spf = Spf.component in
  let%sub sweating = Sweating.component in
  return
  @@ let%map geo_data, geo_node = geo
     and skin_type_data, skin_type_node = skin_type
     and spf_data, spf_node = spf
     and sweating_data, sweating_node = sweating in
     let data =
       match geo_data, skin_type_data, spf_data, sweating_data with
       | (Some weather, Some place_name), Some skin_type, Some spf, Some sweating ->
         Some { weather; place_name; now = weather.current.dt; skin_type; spf; sweating }
       | _ -> None
     in
     { geo_node; skin_type_node; spf_node; sweating_node; data }

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

let how_long_rev ~num_slices { weather; place_name = _; now; skin_type; spf; sweating } =
  let slice_minutes = 60 // num_slices in
  let ( +* ) time = function
    | 0 -> time
    | n -> Time.add time (Time.Span.of_min Float.(of_int n * slice_minutes))
  in
  let slices =
    let rec loop acc : Weather.hourly list -> slice list = function
      | []
       |[ _ ] ->
        acc
      | x :: (y :: _ as rest) ->
        let open Float in
        let ll =
          List.init num_slices ~f:(fun i ->
              let gradient = of_int i * (1 // num_slices) in
              { dt = x.dt +* i; uvi = (x.uvi * (1.0 - gradient)) + (y.uvi * gradient) })
          |> List.rev_filter ~f:(fun { dt; _ } -> Time.( >= ) dt now)
        in
        loop (ll @ acc) rest
    in
    loop [] (List.take weather.hourly 13) |> List.rev
  in
  let skin_type = Skin_type.Fitzpatrick.to_coeff skin_type in
  let get_spf =
    let open Float in
    let base = Spf.Levels.to_coeff spf in
    let convert ~start ~over =
      let over = Time.Span.of_hr over in
      let over_minutes = Time.Span.to_min over in
      let cutoff_start = Time.add now (Time.Span.of_hr start) in
      let cutoff_finish = Time.add cutoff_start over in
      function
      | dt when Time.(dt <= cutoff_start) -> base
      | dt when Time.(dt >= cutoff_finish) -> Spf.Levels.to_coeff SPF_0
      | dt ->
        let passed = Time.diff dt cutoff_start |> Time.Span.to_min in
        base - (passed * base / over_minutes)
    in
    match sweating, spf with
    | Low, _
     |_, SPF_0 ->
      const base
    | Medium, _ -> convert ~start:1.0 ~over:6.0
    | High, _ -> convert ~start:2.0 ~over:12.0
  in
  let num_points, points =
    List.fold_until slices ~init:(0.0, 0, [])
      ~finish:(fun (_, n, ll) -> n, ll)
      ~f:(fun (total, n, ll) slice ->
        let open Float in
        let would_be_100 = 200.0 * skin_type / max 0.001 (slice.uvi * 3.0) * get_spf slice.dt in
        let cost = slice_minutes * 100.0 / would_be_100 in
        let next = { slice; cost; total_at_start = total } in
        match slice with
        | _ when total >= 100.0 -> Stop (succ n, next :: ll)
        | { dt; _ } when Int.(n > 11 && Weather.DT.to_hour_of_day dt >= 22) -> Stop (succ n, next :: ll)
        | _ -> Continue (total + cost, succ n, next :: ll))
  in
  let going_outside = List.hd slices |> Option.map ~f:(fun { dt; _ } -> dt) in
  going_outside, num_points, points

let best_fit_how_long_rev ?(slices = [ 30; 12; 6; 4 ]) data =
  let rec loop = function
    | [ num_slices ] ->
      let going_outside, _, points = how_long_rev ~num_slices data in
      going_outside, points, num_slices
    | num_slices :: rest ->
      let going_outside, num_points, points = how_long_rev ~num_slices data in
      if num_points <= 26 then going_outside, points, num_slices else loop rest
    | [] -> failwith "Impossible case, there should always be hourly values"
  in
  loop slices

type color =
  | Info
  | Warning
  | Danger

let color_to_class = function
| Info -> "text-info"
| Warning -> "text-warning"
| Danger -> "text-danger"

let get_results data =
  let render_advice = function
    | [] -> None
    | ll -> List.map ll ~f:(fun s -> Node.li [] [ Node.text s ]) |> Node.ul [] |> Option.return
  in
  let render_ttb ~color limit text =
    Node.div []
      [
        Node.h5
          Attr.[ classes [ "fw-bold"; "d-inline"; "me-1"; color_to_class color ] ]
          [ Node.text limit ];
        Node.span [] [ Node.text text ];
      ]
  in
  let get_ttb ~color spf now = function
    | { total_at_start; _ } :: _ when Float.( < ) total_at_start 95.0 -> None
    | _ :: { slice = { dt; _ }; _ } :: _ ->
      let sunscreen =
        if Spf.Levels.is_zero spf then "without sunscreen" else sprintf !"with %{Spf.Levels}" spf
      in
      let burn = Time.diff dt now |> Weather.DT.span_to_string in
      Some (render_ttb ~color burn sunscreen)
    | _ -> None
  in
  let render subform_data =
    let going_outside, points, num_slices = best_fit_how_long_rev subform_data in
    let sub_offset =
      match going_outside with
      | None -> Fn.id
      | Some start when Time.(subform_data.now >= start) -> Fn.id
      | Some start ->
        let offset = Time.diff start subform_data.now in
        (fun x -> Time.sub x offset)
    in

    let labels, data =
      List.fold points ~init:([], []) ~f:(fun (labels, pct) computed ->
          Weather.DT.to_string (sub_offset computed.slice.dt) :: labels, computed.total_at_start :: pct)
    in

    let data_spf_0, ttb_spf_0, data_spf_x, ttb_spf_x =
      match subform_data.spf with
      | SPF_0 as spf -> data, get_ttb ~color:Warning spf subform_data.now points, None, None
      | spf ->
        let points_spf_0 = best_fit_how_long_rev { subform_data with spf = SPF_0 } |> snd3 in
        let data_spf_0 =
          best_fit_how_long_rev ~slices:[ num_slices ] { subform_data with spf = SPF_0 }
          |> snd3
          |> List.fold ~init:[] ~f:(fun pct computed -> computed.total_at_start :: pct)
        in
        ( data_spf_0,
          get_ttb ~color:Danger SPF_0 subform_data.now points_spf_0,
          Some data,
          get_ttb ~color:Warning spf subform_data.now points )
    in

    Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
        let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep 0.1 in
        let open Js_of_ocaml in
        (Dom_html.getElementById_opt "results" |> function
         | None -> print_endline "Error, could not find 'results'"
         | Some el -> (Js.Unsafe.coerce el)##scrollIntoView);
        Lwt.return_unit);

    let advice =
      let is_zero, advice =
        match Spf.Levels.is_zero subform_data.spf with
        | true as x -> x, []
        | false as x -> x, [ "Reapply sunscreen after swimming or excessive sweating" ]
      in
      match points with
      | { total_at_start; _ } :: _ when Float.( < ) total_at_start 95.0 ->
        if is_zero
        then advice
        else
          advice
          @ [ "With these precautions you can spend the rest of the day out in the sun, enjoy! ☀️" ]
      | _ :: { slice = { dt = _burn; _ }; _ } :: _ -> (
        match subform_data.spf with
        | Spf.Levels.SPF_0 -> "You should try again with sunscreen" :: advice
        | Spf.Levels.SPF_100 -> "Limit your time in the sun today" :: advice
        | _ -> "Try using a stronger sunscreen or limit your time in the sun today" :: advice
      )
      | _ -> advice
    in

    let node =
      []
      |> add_opt_id (render_advice advice)
      |> add_opt_id ttb_spf_0
      |> add_opt_id
           (Option.first_some ttb_spf_x
              (Option.some_if
                 (not (Spf.Levels.is_zero subform_data.spf))
                 (render_ttb ~color:Info "No limit" "with sunscreen")))
      |> Node.div []
    in
    [ node; Chart.render ~labels ~data_spf_x ~data_spf_0 ~key:(sprintf !"%{sexp: data}" subform_data) ]
    |> Node.div []
  in
  Option.map data ~f:render

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

    let compute ~inject:_ ({ geo_node; skin_type_node; spf_node; sweating_node; data } : Input.t)
       (_model : Model.t) =
      let make_section ?id:id_ ?title ?subtitle nodes =
        let attrs = Attr.[ class_ "pb-1" ] |> add_opt id_ ~f:Attr.id in
        nodes
        |> add_opt subtitle ~f:(fun s -> Node.h6 Attr.[ class_ "pb-1" ] [ Node.text s ])
        |> add_opt title ~f:(fun s -> Node.h5 attrs [ Node.text s ])
        |> Node.div Attr.[ class_ "my-4" ]
      in
      []
      |> add_opt (get_results data) ~f:(fun node ->
             [ node ]
             |> List.cons
                @@ Node.h4 Attr.[ classes [ "pb-1"; "fw-bold" ]; id "results" ] [ Node.text "Results" ]
             |> Node.div Attr.[ class_ "my-4" ])
      |> List.cons
         @@ make_section ~title:"3. Time & Place"
              ~subtitle:"Used for cloud coverage and the angle of the sun." [ geo_node ]
      |> List.cons @@ make_section ~subtitle:"Sweating" [ sweating_node ]
      |> List.cons @@ make_section ~title:"2. Sunscreen" [ spf_node ]
      |> List.cons
         @@ make_section ~title:"1. Fitzpatrick skin scale"
              ~subtitle:"Your skin's sensitivity to UV. Click on one." [ skin_type_node ]
      |> Node.div []

    module Result = Node
  end in
  Bonsai.of_module1 (module Component) ~default_model:() subforms
