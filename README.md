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
./result/bin/hebele-app-cli --help
./result/bin/hebele-app-server --help
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
./result/bin/hebele-app-cli --help
./result/bin/hebele-app-server --help
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
hpack hebele-core
hpack hebele-domain
hpack hebele-app-cli
hpack hebele-app-server
```

Reformat, lint, build and test codebase, respectively:

```sh
fourmolu -i hebele-core/**/*.hs hebele-domain/**/*.hs hebele-app-cli/**/*.hs hebele-app-server/**/*.hs
hlint hebele-core/{src,test} hebele-domain/{src,test} hebele-app-cli/{app,src,test} hebele-app-server/{app,src,test}
cabal build -O0 all
cabal test -O0 all ## TODO: Fix doctest failure due to *Paths_* usage.
```

Generate Haddock documentation:

```sh
cabal haddock -O0 all
```

Run applications:

```sh
cabal run -O0 hebele-app-cli -- --help
cabal run -O0 hebele-app-server -- --help
```

Also:

- Create a PostgreSQL database `haskell-hebele`. Check
  `./database/atlas.hcl` for assumptions.
- Apply database migrations:

    ```sh
    cd database/
    atlas migrate apply --env dev
    ```
