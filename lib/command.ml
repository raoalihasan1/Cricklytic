open! Core
open! Async

let api_token_flag =
  Command.Param.flag "api-token"
    (Command.Param.required (Command.Arg_type.create Fn.id))
    ~doc:"STRING API token for Sportmonks"

let compute_and_print_sportsmonk_result ?filter_f ~f ~print_f () () =
  let%map.Deferred.Or_error query = f in
  let api_results = Sportmonk.api_result query in
  let filtered_results =
    Option.value_map filter_f ~default:api_results ~f:(fun filter ->
        filter api_results)
  in
  print_f filtered_results

let country =
  Command.async_or_error ~summary:"Query about a cricketing nation"
    [%map_open.Command
      let country_name =
        flag "country-name"
          (optional (Arg_type.create Country.Country_name.of_string))
          ~doc:"STRING The name of the country to search for"
      and api_token = api_token_flag in
      compute_and_print_sportsmonk_result
        ~filter_f:(fun countries ->
          Option.value_map country_name ~default:countries ~f:(fun name ->
              List.filter countries ~f:(fun country ->
                  Country.Country_name.equal name (Country.name country))))
        ~f:(Sportmonk.find_countries ~api_token)
        ~print_f:Country.print_stdout_table ()]

let player =
  Command.async_or_error ~summary:"Query about specific cricket players"
    [%map_open.Command
      let full_name =
        flag "player-name" (required string)
          ~doc:"STRING The full name of the cricketer to search for"
      and api_token = api_token_flag in
      compute_and_print_sportsmonk_result
        ~f:(Sportmonk.find_players ~full_name ~api_token)
        ~print_f:Player.print_stdout_table ()]

let venues =
  Command.async_or_error
    ~summary:"Query about cricketing venues around the world"
    [%map_open.Command
      let country_name =
        flag "country-name"
          (optional (Arg_type.create Country.Country_name.of_string))
          ~doc:"STRING The name of the countries venues to search for"
      and api_token = api_token_flag in
      compute_and_print_sportsmonk_result
        ~filter_f:(fun venues ->
          Option.value_map country_name ~default:venues ~f:(fun name ->
              List.filter venues ~f:(fun venue ->
                  match Venues.country venue with
                  | None -> false
                  | Some country ->
                      Country.Country_name.equal name (Country.name country))))
        ~f:(Sportmonk.find_venues ~api_token)
        ~print_f:Venues.print_stdout_table ()]

let command =
  Command.group ~summary:"Get cricket-related analytics"
    [ ("country", country); ("player", player); ("venues", venues) ]
