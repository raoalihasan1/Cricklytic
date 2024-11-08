(lang dune 3.16)

(name cricklytic)

(generate_opam_files true)

(source
 (github raoalihasan1/cricklytic))

(authors "Ali Hasan")

(maintainers "Ali Hasan")

(package
 (name cricklytic)
 (synopsis "A CLI application to query basic information about cricketers and cricketing venues")
 (description "Get information about cricketers such as the country they represent, date of birth
and the position they play as. Similarly, query about cricketing venues such as their
capacity, location and facilities. All the data is fetched from the Sportmonk API 
hence you will need your own API token to run this application.")
 (depends ocaml dune)
 (tags
  (cricket sportmonk)))