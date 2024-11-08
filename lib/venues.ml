open! Core
open Jsonaf.Export

module Capacity = struct
  type t = int [@@deriving sexp_of]

  let t_of_jsonaf = function
    | `Number n -> int_of_string n
    | (_ : Jsonaf_kernel.t) -> 0
end

type t = {
  id : int;
  name : string;
  country : Country.t option; [@jsonaf.option]
  city : string;
  capacity : Capacity.t;
  has_flood_lights : bool; [@key "floodlight"]
}
[@@deriving fields ~getters, of_jsonaf, sexp_of] [@@jsonaf.allow_extra_fields]

let print_stdout_table =
  let cols =
    [
      ("Name", name, 17);
      ( "Country",
        (fun t ->
          Option.value_map (country t) ~default:"" ~f:(fun country ->
              Country.Country_name.to_string (Country.name country))),
        12 );
      ("City", city, 17);
      ("Capacity", (fun t -> string_of_int (capacity t)), 10);
      ("Flood Lights", (fun t -> string_of_bool (has_flood_lights t)), 8);
    ]
    |> List.map ~f:(fun (name, f, min_width) ->
           Ascii_table.Column.create name f ~min_width)
  in
  Ascii_table.output cols ~oc:Out_channel.stdout

let update_country t country = { t with country }
