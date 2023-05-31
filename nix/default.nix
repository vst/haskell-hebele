{ sources ? import ./sources.nix
, haskellCompiler ? "ghc92"
, haskellPackageName ? "hebele-hubele"
, haskellPackagePath ? ../.
, haskellPackageOpts ? ""
, ...
}:

let
  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { };

  ## Get the Haskell package set with overrides:
  haskell = pkgs.haskell.packages.${haskellCompiler}.override { };

  ## Get this Haskell package:
  thisHaskellPackage = haskell.callCabal2nixWithOptions haskellPackageName haskellPackagePath haskellPackageOpts { };

  ## Get dependencies of this Haskell package:
  thisHaskellPackageDeps = pkgs.haskell.lib.compose.getHaskellBuildInputs thisHaskellPackage;

  ## Get development GHC for this Haskell package:
  thisGhcDev = haskell.ghcWithPackages (_: thisHaskellPackageDeps);

  ## Prepare Nix shell:
  thisShell = pkgs.mkShell {
    ## Build inputs for development shell:
    buildInputs = [
      ## Haskell related build inputs:
      thisGhcDev
      haskell.apply-refact
      haskell.cabal-install
      haskell.cabal2nix
      haskell.fourmolu
      haskell.haskell-language-server
      haskell.hlint
      haskell.hpack
    ];

    ## Shell hook for development shell:
    shellHook = ''
      export NIX_GHC=${thisGhcDev}/bin/ghc
      export NIX_GHC_LIBDIR=${thisGhcDev}/lib/${thisGhcDev.meta.name}
    '';
  };
in
{
  shell = thisShell;
  application = thisHaskellPackage;
}
