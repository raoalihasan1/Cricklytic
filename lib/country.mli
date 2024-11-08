open! Core

module Country_name : sig
  type t [@@deriving compare, equal, of_jsonaf, sexp_of, string]
end

type t [@@deriving of_jsonaf, sexp_of]

val id : t -> int
val name : t -> Country_name.t
val print_stdout_table : t list -> unit
