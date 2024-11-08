open! Core
open! Async

type 'a t

val api_result : 'a t -> 'a
val api_token : 'a t -> string
val find_countries : api_token:string -> Country.t list t Or_error.t Deferred.t

val find_players :
  full_name:string -> api_token:string -> Player.t list t Or_error.t Deferred.t

val find_venues : api_token:string -> Venues.t list t Or_error.t Deferred.t
val requested_uri : 'a t -> Uri.t
