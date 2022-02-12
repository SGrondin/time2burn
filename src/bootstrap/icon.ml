open! Core_kernel
open! Bonsai_web
open! Vdom

type t =
  | Check_lg
  | Pencil_square
  | X
[@@deriving sexp]

let get_fill = function
| Check_lg
 |Pencil_square
 |X ->
  "currentColor"

let get_viewbox = function
| Check_lg
 |Pencil_square
 |X ->
  0, 0, 16, 16

type container =
  | Div
  | Span

let svg ?(width = 1.0) ?(height = 1.0) ?(bold = false) ?fill ?(container = Div) ?(raw_extra_classes = [])
   icon attrs =
  let fill =
    Option.value fill ~default:(get_fill icon) |> fun fill -> Yojson.Basic.to_string (`String fill)
  in
  let v1, v2, v3, v4 = get_viewbox icon in
  let paths =
    match icon with
    | Check_lg ->
      {svg|<path d="M13.485 1.431a1.473 1.473 0 0 1 2.104 2.062l-7.84 9.801a1.473 1.473 0 0 1-2.12.04L.431 8.138a1.473 1.473 0 0 1 2.084-2.083l4.111 4.112 6.82-8.69a.486.486 0 0 1 .04-.045z"/>|svg}
    | Pencil_square ->
      {svg|<path d="M15.502 1.94a.5.5 0 0 1 0 .706L14.459 3.69l-2-2L13.502.646a.5.5 0 0 1 .707 0l1.293 1.293zm-1.75 2.456-2-2L4.939 9.21a.5.5 0 0 0-.121.196l-.805 2.414a.25.25 0 0 0 .316.316l2.414-.805a.5.5 0 0 0 .196-.12l6.813-6.814z"/>
      <path fill-rule="evenodd" d="M1 13.5A1.5 1.5 0 0 0 2.5 15h11a1.5 1.5 0 0 0 1.5-1.5v-6a.5.5 0 0 0-1 0v6a.5.5 0 0 1-.5.5h-11a.5.5 0 0 1-.5-.5v-11a.5.5 0 0 1 .5-.5H9a.5.5 0 0 0 0-1H2.5A1.5 1.5 0 0 0 1 2.5v11z"/>|svg}
    | X ->
      {svg|<path d="M4.646 4.646a.5.5 0 0 1 .708 0L8 7.293l2.646-2.647a.5.5 0 0 1 .708.708L8.707 8l2.647 2.646a.5.5 0 0 1-.708.708L8 8.707l-2.646 2.647a.5.5 0 0 1-.708-.708L7.293 8 4.646 5.354a.5.5 0 0 1 0-.708z"/>|svg}
  in
  let html =
    sprintf
      {svg|<svg width="%srem" height="%srem" viewBox="%d %d %d %d" class="%s" %s fill=%s xmlns="http://www.w3.org/2000/svg">
%s
</svg>
|svg}
      (Float.to_string_hum ~strip_zero:true ~decimals:3 width)
      (Float.to_string_hum ~strip_zero:true ~decimals:3 height)
      v1 v2 v3 v4
      (String.concat ~sep:" " raw_extra_classes)
      (if bold then sprintf {|stroke=%s stroke-width="0.75"|} fill else "")
      fill paths
  in
  match container with
  | Div ->
    Node.inner_html ~tag:"div" ~this_html_is_sanitized_and_is_totally_safe_trust_me:html
      ([ attrs; Attr.[ classes [ "v-center"; "align-items-center" ] ] ]
      |> List.concat_no_order
      |> Attrs.merge_classes_and_styles
      )
  | Span -> Node.inner_html ~tag:"span" ~this_html_is_sanitized_and_is_totally_safe_trust_me:html attrs
