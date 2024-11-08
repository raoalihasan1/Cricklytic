open! Core

type t [@@deriving of_jsonaf, sexp_of]

val country : t -> Country.t option
val print_stdout_table : t list -> unit
val update_country : t -> Country.t option -> t
