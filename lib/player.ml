open! Core
open Jsonaf.Export

module Sportmonk_date = struct
  type t = Date.t [@@deriving sexp_of]

  let t_of_jsonaf date =
    let formatted_date =
      date |> Jsonaf.to_string |> String.split ~on:'"' |> String.concat
    in
    if String.equal formatted_date "0000-00-00" then
      Date.today ~zone:Timezone.utc
    else Date.of_string formatted_date
end

type t = {
  full_name : string; [@key "fullname"]
  date_of_birth : Sportmonk_date.t; [@key "dateofbirth"]
  position : Position.t;
  gender : Gender.t;
  country : Country.t option [@jsonaf.option];
}
[@@deriving fields ~getters, of_jsonaf, sexp_of]
[@@jsonaf.allow_extra_fields]

let print_stdout_table =
  let cols =
    [
      ("Name", full_name);
      ("Date of Birth", fun t -> Date.to_string (date_of_birth t));
      ("Position", fun t -> Position.to_string (position t));
      ("Gender", fun t -> Gender.to_string (gender t));
      ("Country", fun t -> Option.value_map (country t) ~default:"" ~f:(fun country -> Country.Country_name.to_string (Country.name country)));
    ]
    |> List.map ~f:(fun (name, f) ->
        Ascii_table.Column.create name f)
  in
  Ascii_table.output cols ~oc:Out_channel.stdout

let update_country t country = { t with country }
