(lang dune 3.14)

(name fathom-experiments)

(source (github yeslogic/fathom-experiments))

; (using mdx 0.4)
(using menhir 3.0)

(implicit_transitive_deps false)
(opam_file_location inside_opam_directory)
(generate_opam_files)

; Supporting packages

(package
  (name byte-set))

(package
  (name case-conv))


; Language experiments

(package
  (name ll1-combinators)
  (depends
    byte-set))

(package
  (name ll1-dsl)
  (depends
    byte-set
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (sedlex (>= "3.2"))))

(package
  (name ll1-dsl2)
  (depends
    byte-set
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (sedlex (>= "3.2"))
    (tsort (>= "2.1"))))

(package
  (name llk-simple)
  (depends
    byte-set))

(package
  (name simple-ddl)
  (depends
    case-conv
    (cmdliner (>= "1.3"))
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (sedlex (>= "3.2"))
    (tsort (>= "2.1"))))
