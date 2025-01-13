# Fathom related experiments

A repository of Fathom-related binary parsing experiments.

## Language projects

- [**bidi-combinators**](./lang-bidi-combinators/):
  A library of bidirectional binary format combinators.
- [**ll1-combinators**](./lang-ll1-combinators):
  Linear-time parser combinators based on Neel Krishnaswami’s blog post.
- [**ll1-dsl**](./lang-ll1-dsl):
  Linear-time parser combinators implemented as an external DSL.
- [**ll1-dsl2**](./lang-ll1-dsl2):
  Linear-time parser combinators implemented as an external DSL, using a more
  traditional elaborator.
- [**llk-simple**](./lang-llk-simple):
  LL(k) parser DSL.
- [**simple-ddl**](./lang-simple-ddl): A simply typed binary format DSL that
  compiles to a recursive descent parser.

## Development setup

### With Nix

Using [Nix] is not required, but can be useful for setting up a development
shell with the required packages and tools used in this project. If you choose
to use Nix, ensure you have it installed, with the [Nix flakes] feature enabled.

[nix-direnv] can be used to load development tools into your shell
automatically. Once it’s installed, run the following commands to enable it in
the project directory:

```sh
echo "use flake" > .envrc
direnv allow
```

The [`.envrc.sample`](.envrc.sample) file contains some additional commands to
watch opam files and load compiled executables onto the PATH. To use it instead,
run the following commands in your shell:

```sh
echo "source_env .envrc.sample" > .envrc
direnv allow
```

You’ll want to locally exclude the `.envrc`, or add it to your global gitignore.

After that, [dune] can be used to build, test, and run the projects:

```sh
dune build --display=short
dune test --display=short
```

[dune]: https://dune.build
[Nix]: https://nixos.org
[Nix flakes]: https://nixos.wiki/wiki/Flakes
[nix-direnv]: https://github.com/nix-community/nix-direnv

#### VS Code

To enable IDE support with the [OCaml Platform extension], add the following to
your [`./.vscode/settings.json`] file:

```jsonc
{
    // https://github.com/ocamllabs/vscode-ocaml-platform/issues/984
    "ocaml.sandbox": {
        "kind": "custom",
        "template": "nix develop ${firstWorkspaceFolder} --command $prog $args",
    },
}
```

[OCaml Platform extension]: https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform
[`./.vscode/settings.json`]: https://code.visualstudio.com/docs/getstarted/settings#_workspace-settingsjson-location

### With opam

Alternatively, [opam] package definitions are provided in the [`./opam`](./opam)
directory. They drive the Nix flake, so _should_ be up to date. I don’t use opam
however, so I’m not sure what the workflow is.

[opam]: opam.ocaml.org
