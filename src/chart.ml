open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic
open Js_of_ocaml

let id_ = "chart"

type dataset = {
  label: string option; [@default None]
  data: float list;
  border_color: string; [@key "borderColor"]
  background_color: string option; [@default None] [@key "backgroundColor"]
  point_background_color: string; [@key "pointBackgroundColor"]
  fill: bool;
  cubic_interpolation_mode: string option; [@default None] [@key "cubicInterpolationMode"]
  tension: float option; [@default None]
}
[@@deriving to_yojson]

type data = {
  labels: string list;
  datasets: dataset list;
}
[@@deriving to_yojson]

module Options = struct
  type title = {
    display: bool;
    text: string option; [@default None]
  }
  [@@deriving to_yojson]

  type legend = { display: bool } [@@deriving to_yojson] [@@unboxed]

  type plugins = { legend: legend } [@@deriving to_yojson] [@@unboxed]

  type interaction = {
    intersect: bool;
    mode: string;
  }
  [@@deriving to_yojson]

  type jss = Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t

  type scale = {
    max: int;
    title: title;
  }
  [@@deriving to_yojson]

  type scales = { y: scale } [@@deriving to_yojson] [@@unboxed]

  type t = {
    responsive: bool;
    plugins: plugins;
    interaction: interaction;
    scales: scales;
  }
  [@@deriving to_yojson]
end

type config = {
  type_: string; [@key "type"]
  data: data;
  options: Options.t;
}
[@@deriving to_yojson]

let load ~labels ~data_spf_x ~data_spf_0 ~width:_ ~height canvas =
  let canvas = Js.Unsafe.coerce canvas in
  let ctx = canvas##getContext "2d" in
  let constructor = window##._Chart in
  let backgroud_gradient =
    let gradient = ctx##createLinearGradient 0 height 0 0 in
    let () = gradient##addColorStop 0.0 "#FFFFFF" in
    let () = gradient##addColorStop 0.6 "#FFF75D" in
    let () = gradient##addColorStop 0.80 "#FFC11F" in
    let () = gradient##addColorStop 0.85 "#FE650D" in
    let () = gradient##addColorStop 0.90 "#F33C04" in
    let () = gradient##addColorStop 0.95 "#DA1F05" in
    let () = gradient##addColorStop 1.0 "#A10100" in
    gradient
  in
  let datasets =
    []
    |> List.cons
         {
           label = Some "Without sunscreen";
           data = data_spf_0;
           border_color = "#FE650D";
           background_color = Some "#FE650D";
           point_background_color = "#FE650D";
           fill = Option.is_none data_spf_x;
           cubic_interpolation_mode = Some "monotone";
           tension = Some 0.4;
         }
    |> add_opt data_spf_x ~f:(fun data ->
           {
             label = Some "With sunscreen";
             data;
             border_color = "#3ADA05";
             background_color = None;
             point_background_color = "#3ADA05";
             fill = true;
             cubic_interpolation_mode = Some "monotone";
             tension = Some 0.4;
           })
  in
  let options =
    {
      type_ = "line";
      data = { labels; datasets };
      options =
        {
          responsive = false;
          plugins = { legend = { display = true } };
          interaction = { intersect = false; mode = "index" };
          scales = { y = { max = 100; title = { display = true; text = Some "skin damage" } } };
        };
    }
    |> [%to_yojson: config]
    |> Yojson.Safe.pretty_to_string
    |> Js.string
  in
  let parsed = Js._JSON##parse options in
  (* Add background gradient *)
  parsed##.data##.datasets##._0##.backgroundColor := backgroud_gradient;
  (* Add beforeDraw plugin *)
  parsed##.plugins :=
    Js.array
      [|
        object%js
          val beforeDraw =
            fun c ->
              c##.legend##.legendItems
              |> Js.to_array
              |> Array.iter ~f:(fun item -> item##.fillStyle := item##.strokeStyle)
        end;
      |];
  (* Add ticks *)
  let ticks =
    object%js
      val callback =
        fun v _i _vs ->
          (match v with
          | v when Float.(v >= 95.0) -> "SUNBURN"
          | v when Float.(v >= 85.0) -> "Danger"
          | v -> sprintf "%d%%" (Float.to_int v))
          |> Js.string
    end
  in
  parsed##.options##.scales##.y##.ticks := ticks;
  let _chart = new%js constructor ctx parsed in
  ()

let render ~labels ~data_spf_x ~data_spf_0 ~key =
  let width =
    window##.outerWidth
    |> Js.Optdef.return
    |> Js.Optdef.to_option
    |> Option.value_map ~default:650 ~f:(fun x -> min 650 (Float.to_int x - 20))
  in
  let height = width / 2 in
  Js_of_ocaml_lwt.Lwt_js_events.async (fun () ->
      Dom_html.getElementById_opt id_
      |> Option.iter ~f:(load ~labels ~data_spf_x ~data_spf_0 ~width ~height);
      Lwt.return_unit);
  Node.div []
    [
      Node.create "canvas" ~key
        Attr.[ id id_; create "width" (Int.to_string width); create "height" (Int.to_string height) ]
        [];
    ]
