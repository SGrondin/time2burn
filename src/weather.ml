open! Core_kernel
open Lwt.Syntax
open Bootstrap.Basic

module DT = struct
  type t = Time.t [@@deriving equal]

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
  | `Int x -> x |> Int.to_float |> Time.Span.of_sec |> Time.of_span_since_epoch |> Result.return
  | `Float x -> x |> Time.Span.of_sec |> Time.of_span_since_epoch |> Result.return
  | json -> Error (sprintf !"Invalid JSON for DT: '%{Yojson.Safe}" json)

  let sexp_of_t x = Sexp.Atom (Time.to_string_iso8601_basic ~zone:Time.Zone.utc x)

  let t_of_sexp = function
  | Sexp.Atom x -> Time.of_string x
  | sexp -> failwithf !"Invalid sexp for DT: '%{Sexp}'" sexp ()

  let to_string dt =
    let parts = Time.to_ofday ~zone:browser_timezone dt |> Time.Ofday.to_parts in
    let hour, ampm =
      match parts.hr with
      | 0 -> 12, "am"
      | h when h < 12 -> h, "am"
      | 12 -> 12, "pm"
      | h -> h - 12, "pm"
    in
    sprintf "%d:%02d%s" hour parts.min ampm
end

type overview = {
  id: int;
  main: string;
  description: string;
  icon: string;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

type current = {
  dt: DT.t;
  temp: float;
  feels_like: float;
  pressure: int;
  humidity: int;
  uvi: float;
  clouds: int;
  wind_speed: float;
  wind_gust: float option; [@default None]
  weather: overview list;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

type hourly = {
  dt: DT.t;
  temp: float;
  feels_like: float;
  pressure: int;
  humidity: int;
  uvi: float;
  clouds: int;
  wind_speed: float;
  wind_gust: float option; [@default None]
  weather: overview list;
  pop: float;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

type t = {
  current: current;
  hourly: hourly list;
}
[@@deriving sexp, equal, of_yojson { strict = false }]

let openweather_api_key = "7d892e70cd3986f7e4bd3f32c58dc8a6"

let weather_uri = Uri.of_string "https://api.openweathermap.org/data/2.5/onecall"

open Cohttp
open Cohttp_lwt_jsoo
module Body = Cohttp_lwt.Body

let get_weather ~latitude ~longitude =
  Lwt.catch
    (fun () ->
      let uri =
        Uri.with_query weather_uri
          [
            "lat", [ Float.to_string latitude ];
            "lon", [ Float.to_string longitude ];
            "exclude", [ "minutely"; "daily"; "alerts" ];
            "lang", [ "en_us" ];
            "units", [ "imperial" ];
            "appid", [ openweather_api_key ];
          ]
      in
      let* res, body = Client.get uri in
      let+ raw = Body.to_string body in
      match Response.status res |> Code.code_of_status with
      | 200 -> Yojson.Safe.from_string raw |> [%of_yojson: t] |> Result.map_error ~f:Error.of_string
      | code ->
        print_endline raw;
        failwithf "Could not get weather data. Error %d" code ())
    (fun exn -> Or_error.of_exn exn |> Lwt.return)
