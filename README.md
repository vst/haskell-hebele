# Haskell Project Template

## Building and Installation

### Plain Nix

Build applications:

```sh
nix-build -A appCli
nix-build -A appServer
```

Run applications:

```sh
./result/bin/hebele-hubele-app-cli --help
./result/bin/hebele-hubele-app-server --help
```

Install applications:

```sh
nix-env -f default.nix -iA appCli
nix-env -f default.nix -iA appServer
```

### Nix Flakes

Build applications:

```sh
nix build .#cli
nix build .#server
```

Run applications:

```sh
./result/bin/hebele-hubele-app-cli --help
./result/bin/hebele-hubele-app-server --help
```

Install applications:

```sh
nix profile install .#cli
nix profile install .#server
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
hpack hebele-hubele-app-server
```

Reformat, lint, build and test codebase, respectively:

```sh
fourmolu -i hebele-hubele-core/**/*.hs hebele-hubele-domain/**/*.hs hebele-hubele-app-cli/**/*.hs hebele-hubele-app-server/**/*.hs
hlint hebele-hubele-core/{src,test} hebele-hubele-domain/{src,test} hebele-hubele-app-cli/{app,src,test} hebele-hubele-app-server/{app,src,test}
cabal build -O0 all
cabal test -O0 all ## TODO: Fix doctest failure due to *Paths_* usage.
```

Generate Haddock documentation:

```sh
cabal haddock -O0 all
```

Run applications:

```sh
cabal run -O0 hebele-hubele-app-cli -- --help
cabal run -O0 hebele-hubele-app-server -- --help
```
