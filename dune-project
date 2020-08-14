(lang dune 2.2)

(name main)

(package
 (name main)
  (depends
    (ocaml (>= 4.07))
    (dune  (>= 2.2))
     menhir
  )
)

(using menhir 2.1)
