open! Core

type t = Male | Female | Other of string [@fallback] [@@deriving sexp_of, to_string]

let t_of_jsonaf gender =
  let gender_formatted = gender |> Jsonaf.to_string |> String.split ~on:'"' |> String.concat in
    match gender_formatted with
    | "m" -> Male
    | "f" -> Female
    | other_gender -> Other other_gender
