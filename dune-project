(lang dune 3.20)

(name project_t)

(generate_opam_files true)

(source
 (github DCCXXV/project_t))

(authors "Marco Antonio Pérez Neira <marcoantonioperezneira@gmail.com>")

(maintainers "Marco Antonio Pérez Neira <marcoantonioperezneira@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name project_t)
 (synopsis "A simple ascii dungeon crawler")
 (description "Turn based dungeon crawler with deterministic enemy patterns. Use hazards to kill every enemy and complete floor 3 for victory.")
 (depends
    (ocaml (= 5.4.0))
   notty_community)
 (tags
  (game ascii)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
