# Vite plugin for OCaml

A plugin for vite that handles `.ml` files compiled with `dune` and `js_of_ocaml`.

> [!WARNING]
> The development server might flash an error temporarily while the `.bc.js`
> file is missing during hot module replacement. This is due to a race with
> Dune that I’ve not yet been able to fix.

## Inspiration

Getting this to work was a challenge, and was helped by looking at the
following projects:

- [github:jihchi/vite-plugin-rescript](https://github.com/jihchi/vite-plugin-rescript)
- [github:Lavinium/vite-plugin-melange](https://github.com/Lavinium/vite-plugin-melange)
- [github:pdelacroix/vite-plugin-melange](https://github.com/pdelacroix/vite-plugin-melange)
- [github:nojaf/vite-plugin-fable](https://github.com/nojaf/vite-plugin-fable)
