open! Core
open! Async

type 'a t = { api_result : 'a; requested_uri : Uri.t; api_token : string }
[@@deriving fields ~getters]

let base_uri = Uri.of_string "https://cricket.sportmonks.com/api/v2.0/"

let query_sportmonk ~uri ~api_token =
  let uri_with_token = Uri.add_query_param' uri ("api_token", api_token) in
  let%bind response, body = Cohttp_async.Client.get uri_with_token in
  let response_status_code =
    response |> Cohttp.Response.status |> Cohttp.Code.code_of_status
  in
  if Cohttp.Code.is_success response_status_code then
    let%map str_body = Cohttp_async.Body.to_string body in
    Ok (str_body |> Jsonaf.of_string |> Jsonaf.member_or_null "data")
  else
    Deferred.Or_error.error_string
      [%string
        "The call to Sportmonk API returned status code \
         %{response_status_code#Int} for endpoint %{uri_with_token#Uri}"]

let find_countries ~api_token =
  let uri = Uri.of_string [%string "%{base_uri#Uri}countries"] in
  let%map.Deferred.Or_error response = query_sportmonk ~uri ~api_token in
  let countries =
    match response with
    | `Array countries ->
        List.map countries ~f:Country.t_of_jsonaf
        |> List.sort ~compare:(fun a b ->
               Country.Country_name.compare (Country.name a) (Country.name b))
    | (_ : Jsonaf_kernel.t) -> []
  in
  { api_result = countries; requested_uri = uri; api_token }

let find_players ~full_name ~api_token =
  let open Deferred.Or_error.Let_syntax in
  let split_full_name = String.split ~on:' ' full_name in
  match List.last split_full_name with
  | Some last_name when not (String.is_empty last_name) ->
      let uri =
        Uri.of_string
          [%string "%{base_uri#Uri}players?filter[lastname]=%{last_name}"]
      in
      let%bind countries_lst = find_countries ~api_token in
      let%map players_lst =
        match%map query_sportmonk ~uri ~api_token with
        | `Array player_objs ->
            List.map player_objs ~f:(fun player_json ->
                let player = Player.t_of_jsonaf player_json in
                match Jsonaf.member_or_null "country_id" player_json with
                | `Number country_id ->
                    let country =
                      List.find countries_lst.api_result ~f:(fun country ->
                          int_of_string country_id = Country.id country)
                    in
                    Player.update_country player country
                | (_ : Jsonaf_kernel.t) -> player)
        | (_ : Jsonaf_kernel.t) -> []
      in
      let filtered_players =
        List.filter players_lst ~f:(fun player ->
            String.Caseless.equal (Player.full_name player) full_name)
      in
      { api_result = filtered_players; requested_uri = uri; api_token }
  | (_ : string option) ->
      Deferred.Or_error.error_string
        [%string
          "Failed to determine the last name of the player '%{full_name}'"]

let find_venues ~api_token =
  let open Deferred.Or_error.Let_syntax in
  let uri = Uri.of_string [%string "%{base_uri#Uri}venues"] in
  let%bind countries = find_countries ~api_token in
  let%map response = query_sportmonk ~uri ~api_token in
  let venues =
    match response with
    | `Array venues ->
        List.map venues ~f:(fun venue_json ->
            let country =
              List.filter (api_result countries) ~f:(fun country ->
                  Int.equal (Country.id country)
                    (venue_json
                    |> Jsonaf.member_or_null "country_id"
                    |> Jsonaf.to_string |> int_of_string))
              |> List.hd
            in
            Venues.update_country (Venues.t_of_jsonaf venue_json) country)
    | (_ : Jsonaf_kernel.t) -> []
  in
  { api_result = venues; requested_uri = uri; api_token }
