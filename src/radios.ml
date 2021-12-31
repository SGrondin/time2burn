open! Core_kernel
open! Bonsai_web
open! Bonsai.Let_syntax
open! Vdom
open! Bootstrap
open! Bootstrap.Basic

module type S = sig
  type t [@@deriving sexp, equal, enumerate]

  val to_string : t -> string

  val is_bold : t -> bool
end

module Make (M : S) = struct
  let create ~storage_key ~update state ~columns =
    let handler _evt s =
      let data = Sexp.of_string_conv_exn s [%of_sexp: M.t option] in
      Local_storage.set_item ~key:storage_key ~data:s
      |> Result.iter_error ~f:(fun err ->
             print_endline
               (sprintf "Could not store to local storage key '%s': '%s'. Error: '%s'" storage_key s err));
      update data
    in
    let make value =
      let id_ = sprintf !"radio-%s-%{M}" storage_key value in
      let data = Some value in
      let input_attrs =
        let is_selected = [%equal: M.t option] state data in
        Attr.
          [
            class_ "form-check-input";
            type_ "radio";
            name (sprintf "%s-input" storage_key);
            id id_;
            value (sprintf !"%{sexp: M.t option}" data);
          ]
        |> add_if is_selected Attr.checked
      in
      let label_attrs =
        let extras =
          match state with
          | None when M.is_bold value -> Attr.[ class_ "text-primary" ]
          | Some x when [%equal: M.t] x value -> Attr.[ class_ "fw-bold" ]
          | _ -> []
        in
        merge_attrs [ Attr.[ classes [ "form-check-label"; "text-nowrap" ]; for_ id_ ]; extras ]
      in
      Node.div
        Attr.[ classes [ "form-check" ] ]
        [ Node.input input_attrs []; Node.label label_attrs [ Node.textf !"%{M}" value ] ]
    in
    Node.div
      Attr.
        [
          classes [ "row"; sprintf "row-cols-%d" columns; "g-1" ];
          style Css_gen.(max_width (`Em 36));
          on_change handler;
        ]
      (List.map M.all ~f:make)
end
