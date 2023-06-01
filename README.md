# Haskell Project Template

## Building and Installation

### Plain Nix

Build the application:

```sh
nix-build -A application
```

Run the application:

```sh
./result/bin/hebele-hubele-app-cli --help
```

Install the application:

```sh
nix-env -f default.nix -iA application
```

### Nix Flakes

Build the application:

```sh
nix build
```

Run the application:

```sh
./result/bin/hebele-hubele-app-cli --help
```

Install the application:

```sh
nix profile install
```

## Development Quickstart

First, enter Nix shell:

1. Vanilla Nix:

    ```sh
    nix-shell
    ```

2. Nix Flakes:

    ```sh
    nix develop
    ```

Re-generate `.cabal` file if and when required:

```sh
hpack hebele-hubele-core
hpack hebele-hubele-domain
hpack hebele-hubele-app-cli
```

Reformat, lint, build and test codebase, respectively:

```sh
fourmolu -i hebele-hubele-core/**/*.hs hebele-hubele-domain/**/*.hs hebele-hubele-app-cli/**/*.hs
hlint hebele-hubele-core/{src,test} hebele-hubele-domain/{src,test} hebele-hubele-app-cli/{app,src,test}
cabal build -O0 all
cabal test -O0 all ## TODO: Fix doctest failure due to *Paths_* usage.
```

Generate Haddock documentation:

```sh
cabal haddock -O0 all
```

Run application:

```sh
cabal run -O0 hebele-hubele-app-cli -- --help
```
