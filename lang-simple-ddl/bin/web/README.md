# Web frontend

```text
$ dune build simple-ddl
$ python -m http.server 8000 --bind localhost --directory _build/default/lang-simple-ddl/bin/web
```

Then navigate to <http://127.0.0.1:8000/>.

## Todo list

- [x] Basic build setup
- [ ] Hot reloading
- [ ] Code packaging

## Resources

- [Js_of_ocaml - Reference Manual](https://ocsigen.org/js_of_ocaml/latest/manual/overview)
- [JavaScript Compilation With Js_of_ocaml](https://dune.readthedocs.io/en/stable/jsoo.html)
- [JavaScript Compilation With Melange](https://dune.readthedocs.io/en/stable/melange.html)
