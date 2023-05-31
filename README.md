# Haskell Project Template

## Development Quickstart

Enter Nix shell:

```sh
nix-shell
```

Generate `.cabal` file:

```sh
hpack
```

Reformat, lint, build and test codebase, respectively:

```sh
fourmolu -i app/ src/ test/
hlint app/ src/ test/
cabal build -O0
cabal v1-test
```

Generate Haddock documentation:

```sh
cabal haddock -O0
```

Run application:

```sh
cabal run -O0
```
