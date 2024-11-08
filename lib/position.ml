open! Core

type t =
  | Batsman
  | Bowler
  | Wicketkeeper
  | All_rounder [@rename "All Rounder"]
  | Unknown of string [@fallback]
[@@deriving sexp_of, to_string]

let unknown_message = Unknown "Failed to determine the position of the player"

let of_string = function
  | "Bowler" -> Bowler
  | "Batsman" -> Batsman
  | "Wicketkeeper" -> Wicketkeeper
  | "Allrounder" -> All_rounder
  | other -> Unknown other

let t_of_jsonaf = function
  | `Object lst ->
    let position =
      let find_position_from_object = function
        | `String pos -> of_string pos
        | (_ : Jsonaf_kernel.t) -> unknown_message
      in
      List.find_map lst ~f:(fun (key, value) ->
          if String.equal key "name" then
            Some (find_position_from_object value)
          else None)
    in
    Option.value position ~default:unknown_message
  | (_ : Jsonaf_kernel.t) -> unknown_message

let%expect_test "Custom to_string for All_rounder" =
  let all_rounder_str = to_string All_rounder in
  print_s [%message (all_rounder_str : string)];
  [%expect {| (all_rounder_str "All Rounder") |}]
