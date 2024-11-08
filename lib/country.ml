open! Core
open Jsonaf.Export

module Country_name = struct
type t =
  | Afghanistan
  | Australia
  | Bangladesh
  | England
  | Germany
  | Hong_kong [@rename "Hong Kong"]
  | India
  | Ireland
  | Jersey
  | Kenya
  | Kuwait
  | Nepal
  | Netherlands
  | New_zealand [@rename "New Zealand"]
  | Papua_new_guinea [@rename "Papua New Guinea"]
  | Pakistan
  | Qatar
  | Saudi_arabia [@rename "Saudi Arabia"]
  | Scotland
  | South_africa [@rename "South Africa"]
  | Sri_lanka [@rename "Sri Lanka"]
  | United_arab_emirates [@rename "UAE"]
  | USA
  | West_indies [@rename "West Indies"]
  | Zimbabwe
  | Undefined of string [@fallback]
[@@deriving compare, equal, sexp_of, string ~case_insensitive]

let t_of_jsonaf jsonaf =
  match jsonaf with
  | `String str -> (
      match str with
      | "Afghanistan" -> Afghanistan
      | "Australia" -> Australia
      | "Bangladesh" -> Bangladesh
      | "England" -> England
      | "Germany" -> Germany
      | "Hong Kong" -> Hong_kong
      | "India" -> India
      | "Republic of Ireland" -> Ireland
      | "Jersey" -> Jersey
      | "Kenya" -> Kenya
      | "Kuwait" -> Kuwait
      | "Nepal" -> Nepal
      | "Netherlands" -> Netherlands
      | "New Zealand" -> New_zealand
      | "Papua New Guinea" -> Papua_new_guinea
      | "Pakistan" -> Pakistan
      | "Qatar" -> Qatar
      | "Saudi Arabia" -> Saudi_arabia
      | "Scotland" -> Scotland
      | "South Africa" -> South_africa
      | "Sri Lanka" -> Sri_lanka
      | "United Arab Emirates" -> United_arab_emirates
      | "USA" -> USA
      | "West Indies" -> West_indies
      | "Zimbabwe" -> Zimbabwe
      | other -> Undefined other)
  | (_ : Jsonaf_kernel.t) -> Undefined "Failed to parse the JSON"

end

type t = {
  id : int;
  name : Country_name.t;
}
[@@deriving fields ~getters, of_jsonaf, sexp_of]
[@@jsonaf.allow_extra_fields]

let print_stdout_table =
  let cols =
    [ ("Country", fun t -> Country_name.to_string (name t)) ]
    |> List.map ~f:(fun (name, f) ->
        Ascii_table.Column.create name f)
  in
  Ascii_table.output cols ~oc:Out_channel.stdout
