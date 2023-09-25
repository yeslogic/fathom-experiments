# Fathom related experiments

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

The `.envrc.example` file contains some additional commands to watch opam files
and load compiled executables onto the PATH. To use it instead, run the
following commands in your shell:

```sh
echo "source_env .envrc.example" > .envrc
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

### With opam

Alternatively, [opam] package definitions are provided in the [`./opam`](./opam)
directory. They drive the Nix flake, so _should_ be up to date. I don’t use opam
however, so I’m not sure what the workflow is.

[opam]: opam.ocaml.org
