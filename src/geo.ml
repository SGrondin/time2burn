open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic
open Cohttp
open Cohttp_lwt_jsoo
module Body = Cohttp_lwt.Body
open Lwt.Syntax

module Position = struct
  type t = {
    latitude: float;
    longitude: float;
  }
  [@@deriving sexp, equal]
end

module Errors = struct
  type t =
    [ `Disabled
    | `Declined
    | `Failed
    | `Timeout
    | `Unexpected
    | `Safari
    | `Text       of string
    ]
  [@@deriving sexp, equal]

  let to_div = function
  | `Disabled ->
    Node.div []
      [
        Node.div [] [ Node.text "Please enable Location Services on your device and try again." ];
        Node.text "You can also enter an address manually.";
      ]
  | `Declined ->
    Node.div []
      [
        Node.div [] [ Node.text "This site is not allowed to use your location." ];
        Node.text "You can enter an address manually.";
      ]
  | `Failed ->
    Node.div []
      [
        Node.div [] [ Node.text "Your device failed to discover your location." ];
        Node.text "You can also enter an address manually.";
      ]
  | `Timeout ->
    Node.div []
      [
        Node.div [] [ Node.text "Your device could not determine your position in a reasonable time." ];
        Node.text "Please try again or enter your address manually.";
      ]
  | `Unexpected ->
    Node.div []
      [
        Node.div [] [ Node.text "Your device failed to discover your location due to an unknown error." ];
        Node.text "Please try again or enter your address manually.";
      ]
  | `Safari ->
    Node.div []
      [
        Node.div [] [ Node.text "Please enable Location Services on your device and try again." ];
        Node.p [] [ Node.text "Settings -> Privacy -> Location Services" ];
        Node.text "You can also enter an address manually.";
      ]
  | `Text s ->
    Node.div [] [ Node.div [] [ Node.text s ]; Node.text "You can also enter an address manually." ]
end

module Permissions = struct
  type t =
    | Unknown
    | Granted
    | Prompt
    | Denied
  [@@deriving sexp, equal]

  let query kind =
    let open Js_of_ocaml in
    match window##.navigator##.permissions |> Js.Optdef.return |> Js.Optdef.to_option with
    | None -> Lwt.return_ok Unknown
    | Some permissions ->
      let p, w = Lwt.task () in
      let arg =
        object%js
          val name = Js.string kind
        end
      in
      let jsp = permissions##query arg in
      let _then =
        jsp##_then (fun obj ->
            Lwt.wakeup_later w
              (match obj##.state |> Js.to_string with
              | "granted" -> Ok Granted
              | "prompt" -> Ok Prompt
              | "denied" -> Ok Denied
              | s -> Error (`Text (sprintf "Invalid geolocation permission '%s'" s))))
      in
      let _catch =
        jsp##_catch (fun err -> Lwt.wakeup_later w (Error (`Text (err##toString |> Js.to_string))))
      in
      p
end

let get_location () =
  let p, w = Lwt.task () in
  let on_success position =
    (try
       let () = window##.console##log position in
       let latitude = position##.coords##.latitude in
       let longitude = position##.coords##.longitude in
       Ok Position.{ latitude; longitude }
     with
    | Failure msg -> Error (`Text msg)
    | exn -> Error (`Text (Exn.to_string exn)))
    |> Lwt.wakeup_later w
  in
  let on_error err =
    (match err##.code with
    | 1 -> `Disabled
    | 2 -> `Failed
    | 3 -> `Timeout
    | _ -> `Unexpected)
    |> Result.fail
    |> Lwt.wakeup_later w
  in
  let options =
    object%js
      val maximumAge = Time.Span.(of_min 10.0 |> to_ms) |> Float.to_int

      val timeout = Time.Span.(of_sec 10.0 |> to_ms) |> Float.to_int
    end
  in
  let () = window##.navigator##.geolocation##getCurrentPosition on_success on_error options in
  p

module Mapbox = struct
  let api_key =
    "pk.eyJ1Ijoic2dyb25kaW4iLCJhIjoiY2txcW9manJwMTN2bjJ2cDU4dW5yN3Y2MCJ9.iE43zZJTBcKqdSMo__va1g"

  let base_uri = Uri.of_string "https://api.mapbox.com"

  type feature = {
    place_name: string;
    center: float * float;
  }
  [@@deriving sexp, equal, of_yojson { strict = false }]

  type feature_collection = {
    features: feature list;
    attribution: string option; [@default None]
  }
  [@@deriving sexp, equal, of_yojson { strict = false }]

  let parse json =
    let open Result.Monad_infix in
    [%of_yojson: feature_collection] json
    >>= (fun { features; attribution } ->
          List.hd features |> Result.of_option ~error:"No results found" >>| fun x -> x, attribution)
    |> Result.map_error ~f:(fun s -> `Text s)
end

let geocoding query =
  Lwt.catch
    (fun () ->
      let uri =
        Uri.pct_encode query
        |> sprintf "/geocoding/v5/mapbox.places/%s.json"
        |> Uri.with_path Mapbox.base_uri
        |> Fn.flip Uri.with_query'
             [
               "autocomplete", "true";
               "fuzzyMatch", "true";
               "language", "en";
               "limit", "1";
               "routing", "false";
               "types", "district,place,locality,neighborhood,address,poi";
               "worldview", "us";
               "access_token", Mapbox.api_key;
             ]
      in
      let* res, body = Client.get uri in
      let+ raw = Body.to_string body in
      match Response.status res |> Code.code_of_status with
      | 200 -> Yojson.Safe.from_string raw |> Mapbox.parse
      | code ->
        print_endline raw;
        failwithf "Could not get address data. Error %d" code ())
    (function
      | Failure msg -> Lwt.return_error (`Text msg)
      | exn -> Lwt.return_error (`Text (Exn.to_string exn)))

let component =
  let module Component = struct
    module Input = Unit

    type confirm_search = {
      feature: Mapbox.feature;
      attribution: string option;
      query: string;
    }
    [@@deriving sexp, equal]

    module Model = struct
      type status =
        | Blank_geo
        | Blank_search      of string
        | Fetching_geo
        | Fetching_search
        | Confirming_search of confirm_search
        | Fetching_weather
        | Completed         of string
      [@@deriving sexp, equal]

      type t = {
        weather: (Weather.t option, Errors.t) Result.t;
        place_name: string option;
        status: status;
      }
      [@@deriving sexp, equal]
    end

    module Action = struct
      type t =
        | Blank_geo
        | Blank_search      of string
        | Fetching_geo
        | Fetching_search   of string
        | Confirming_search of confirm_search
        | Fetching_weather  of {
            place_name: string;
            position: Position.t;
          }
        | Fetched_weather   of {
            place_name: string;
            weather: Weather.t;
          }
        | Errored           of Errors.t
      [@@deriving sexp_of]
    end

    let name = Source_code_position.to_string [%here]

    let apply_action ~inject ~schedule_event (() : Input.t) (_prev : Model.t) : Action.t -> Model.t =
      function
    | Blank_geo -> { weather = Ok None; place_name = None; status = Blank_geo }
    | Blank_search text -> { weather = Ok None; place_name = None; status = Blank_search text }
    | Fetching_geo ->
      let open Lwt.Infix in
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          Permissions.query "geolocation" >>= function
          | Error (`Text _ as err) ->
            Action.Errored err |> inject |> schedule_event;
            Lwt.return_unit
          | Ok permissions ->
            let+ location = get_location () in
            (match permissions, location with
            | Denied, Error x -> Action.Errored x
            | _, Ok position ->
              Action.Fetching_weather
                {
                  position;
                  place_name = sprintf "Coordinates %.3f,%.3f" position.latitude position.longitude;
                }
            | Unknown, Error `Failed -> Action.Errored `Safari
            | Unknown, Error `Disabled
             |Prompt, Error `Disabled ->
              Action.Errored `Declined
            | _, Error x -> Action.Errored x)
            |> inject
            |> schedule_event);
      { weather = Ok None; place_name = None; status = Fetching_geo }
    | Fetching_search query ->
      let open Lwt.Infix in
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          geocoding query
          >|= (function
                | Ok (feature, attribution) -> Action.Confirming_search { feature; attribution; query }
                | Error err -> Action.Errored err)
          >|= Fn.compose schedule_event inject);
      { weather = Ok None; place_name = None; status = Fetching_search }
    | Confirming_search x -> { weather = Ok None; place_name = None; status = Confirming_search x }
    | Fetching_weather { place_name; position = { longitude; latitude } } ->
      let open Lwt.Infix in
      Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
          Weather.get_weather ~longitude ~latitude
          >|= (function
                | Ok weather -> Action.Fetched_weather { place_name; weather }
                | Error err -> Action.Errored err)
          >|= Fn.compose schedule_event inject);
      { weather = Ok None; place_name = Some place_name; status = Fetching_weather }
    | Fetched_weather { place_name; weather } ->
      { weather = Ok (Some weather); place_name = Some place_name; status = Completed place_name }
    | Errored err -> { weather = Error err; place_name = None; status = Blank_geo }

    let compute ~inject (() : Input.t) (model : Model.t) =
      let status_node =
        let make_attribution attribution =
          let line =
            Node.div
              Attr.[ classes [ "small"; "d-flex"; "align-items-center" ] ]
              [
                Node.text "Address search powered by ";
                Node.create "object"
                  Attr.
                    [
                      create "data" "/mapbox.svg";
                      type_ "image/svg+xml";
                      class_ "ms-1";
                      style Css_gen.(width (`Em 5));
                    ]
                  [];
              ]
          in
          Option.value_map attribution ~default:line ~f:(fun att ->
              Node.div
                Attr.[ class_ "mt-3" ]
                [
                  line;
                  Node.div Attr.[ class_ "small"; style Css_gen.(max_width (`Em 20)) ] [ Node.text att ];
                ])
        in

        match model.status with
        | Blank_geo ->
          let handler_toggle _evt = inject (Action.Blank_search "") in
          let handler_geo _evt = inject Action.Fetching_geo in
          Node.div []
            [
              Node.button
                Attr.[ classes [ "btn"; "btn-primary"; "d-block" ]; on_click handler_geo ]
                [ Node.text "Use my location" ];
              Node.span
                Attr.[ classes [ "link-info"; "mt-1" ]; on_click handler_toggle; style pointer ]
                [ Node.text "Enter address manually" ];
            ]
        | Blank_search text ->
          let open Js_of_ocaml in
          let input_id = "search-box" in
          let handler_toggle _evt = inject Action.Blank_geo in
          let handler_search _evt =
            Dom_html.getElementById_opt input_id
            |> Option.value_map ~default:Event.Ignore ~f:(fun el ->
                   let query = (Js.Unsafe.coerce el)##.value |> Js.to_string |> String.strip in
                   if String.is_empty query then Event.Ignore else inject (Action.Fetching_search query))
          in
          let handler_keydown evt =
            let open Js_of_ocaml in
            match Js.Optdef.case evt##.key (const None) (fun jss -> Some (Js.to_string jss)) with
            | Some "Enter" -> handler_search evt
            | _ -> Event.Ignore
          in
          Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
              Dom_html.getElementById_coerce input_id Dom_html.CoerceTo.input
              |> Option.iter ~f:(fun el -> el##focus);
              Lwt.return_unit);
          Node.div []
            [
              Node.input
                Attr.
                  [
                    type_ "text";
                    id input_id;
                    class_ "form-control";
                    style Css_gen.(max_width (`Em 20));
                    value text;
                    on_keydown handler_keydown;
                  ]
                [];
              make_attribution None;
              Node.div
                Attr.[ class_ "mt-2" ]
                [
                  Node.button
                    Attr.[ classes [ "btn"; "btn-primary"; "me-2" ]; on_click handler_search ]
                    [ Node.text "Search" ];
                  Node.button
                    Attr.[ classes [ "btn"; "btn-secondary" ]; on_click handler_toggle ]
                    [ Node.text "Cancel" ];
                ];
            ]
        | Confirming_search { feature = { place_name; center = longitude, latitude }; attribution; query }
          ->
          let handler_geo _evt = inject Action.Fetching_geo in
          let handler_yes _evt =
            inject (Action.Fetching_weather { place_name; position = { longitude; latitude } })
          in
          let handler_no _evt = inject (Action.Blank_search query) in
          Node.div []
            [
              Node.div [] [ Node.text "Is this correct?" ];
              Node.div Attr.[ class_ "fw-bold" ] [ Node.text place_name ];
              Node.button
                Attr.[ classes [ "btn"; "btn-primary"; "me-2" ]; on_click handler_yes ]
                [ Node.text "Yes" ];
              Node.button
                Attr.[ classes [ "btn"; "btn-secondary" ]; on_click handler_no ]
                [ Node.text "No" ];
              Node.div
                Attr.[ classes [ "link-info"; "mt-1" ]; on_click handler_geo; style pointer ]
                [ Node.text "Use my location instead" ];
              make_attribution attribution;
            ]
        | Fetching_geo
         |Fetching_search ->
          Node.div []
            [
              Node.div
                Attr.[ classes [ "spinner-border"; "text-info" ]; create "role" "status" ]
                [ Node.span Attr.[ class_ "visually-hidden" ] [ Node.text "Loading..." ] ];
            ]
        | Fetching_weather ->
          Node.div
            Attr.[ classes [ "spinner-border"; "text-success" ]; create "role" "status" ]
            [ Node.span Attr.[ class_ "visually-hidden" ] [ Node.text "Loading..." ] ]
        | Completed place_name ->
          let handler _evt = inject Action.Blank_geo in
          Node.div []
            [
              Icon.svg Check_lg ~container:Span Attr.[ class_ "text-success" ];
              Node.button
                Attr.[ classes [ "btn"; "btn-light"; "shadow-sm"; "p-1"; "mx-2" ]; on_click handler ]
                [ Icon.svg ~container:Span Pencil_square []; Node.text "Edit" ];
              Node.span [] [ Node.text place_name ];
            ]
      in
      let node =
        match model.weather with
        | Error err ->
          Node.div []
            [
              Node.div
                Attr.[ classes [ "alert"; "alert-danger" ]; style Css_gen.(max_width (`Em 40)) ]
                [ Errors.to_div err ];
              status_node;
            ]
        | Ok _ -> Node.div [] [ Node.div [] []; status_node ]
      in
      let data_weather =
        match model.weather with
        | Error _
         |Ok None ->
          None
        | Ok (Some x) -> Some x
      in
      (data_weather, model.place_name), node

    module Result = struct
      type t = (Weather.t option * string option) * Node.t
    end
  end in
  Bonsai.of_module0
    (module Component)
    ~default_model:{ weather = Ok None; place_name = None; status = Blank_geo }
