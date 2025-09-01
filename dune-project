(lang dune 3.17)

(name fathom-experiments)

(license MIT)
(authors "YesLogic Pty. Ltd. <info@yeslogic.com>")
(source (github yeslogic/fathom-experiments))

(using mdx 0.4)
(using menhir 3.0)

(implicit_transitive_deps false)
(opam_file_location inside_opam_directory)
(generate_opam_files)

; Supporting packages

(package
  (name byte-set))

(package
  (name case-conv))

(package
  (name sized-numbers)
  (depends
    (alcotest (and (>= "1.9") :with-test))
    (fmt (and (>= "0.9") :with-test))))


; Language experiments

(package
  (name bidi-combinators)
  (depends
    sized-numbers
    (mdx (and (>= "2.5") :with-test))))

(package
  (name derivatives)
  (depends
    byte-set))

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
    (sedlex (>= "3.6"))))

(package
  (name ll1-dsl2)
  (depends
    byte-set
    (menhir (and (>= "20240715") :build))
    (menhirLib (>= "20240715"))
    (sedlex (>= "3.6"))
    (tsort (>= "2.2"))))

(package
  (name llk-simple)
  (depends
    byte-set))

(package
  (name narcissus)
  (depends
    sized-numbers))

(package
  (name simple-ddl)
  (depends
    ; CLI
    (cmdliner (>= "1.3"))
    (menhirLib (>= "20240715"))

    ; Web interface
    (js_of_ocaml-compiler (>= "6.0.1"))
    (js_of_ocaml-ppx (>= "6.0.1"))
    (js_of_ocaml (>= "6.0.1"))

    ; Core library
    case-conv
    (menhir (and (>= "20240715") :build))
    (sedlex (>= "3.6"))
    sized-numbers
    (tsort (>= "2.2"))))
