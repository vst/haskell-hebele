{ sources ? import ./nix/sources.nix
, compiler ? "default"
, system ? builtins.currentSystem
, ...
}:

let
  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { inherit system; };

  ## Get base Haskell package set:
  thisHaskell =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  ## Get Haskell multi-project driver:
  thisProject = (thisHaskell.override {
    overrides = self: super: with pkgs.haskell.lib; {
      hebele-hubele-core = self.callCabal2nix "hebele-hubele-core" ./hebele-hubele-core { };
      hebele-hubele-app-cli = self.callCabal2nix "hebele-hubele-app-cli" ./hebele-hubele-app-cli { };
    };
  }).extend (pkgs.haskell.lib.packageSourceOverrides { });

  ## Prepare Nix shell:
  thisShell = thisProject.shellFor {
    ## Define packages for the shell:
    packages = p: [
      p.hebele-hubele-core
      p.hebele-hubele-app-cli
    ];

    ## Enable Hoogle:
    withHoogle = true;

    ## Build inputs for development shell:
    buildInputs = [
      ## Haskell related build inputs:
      thisProject.apply-refact
      thisProject.cabal-install
      thisProject.cabal2nix
      thisProject.fourmolu
      thisProject.haskell-language-server
      thisProject.hlint
      thisProject.hpack
    ];

    ## Shell hook for development shell:
    shellHook = ''
      echo "Welcome to Development Shell"
    '';
  };

  ## Define a function that makes this application installable in Nix environment:
  makeThisApplicationInstallable = drv: drv.overrideAttrs (oldAttrs:
    let
      ## We need these inputs at buildtime:
      extraNativeBuildInputs = [
        pkgs.git
        pkgs.makeWrapper
      ];

      ## We need these inputs at runtime:
      binPath = pkgs.lib.makeBinPath [ ];

      ## Post-fixup process:
      extraPostFixup = ''
        wrapProgram $out/bin/hebele-hubele-app-cli --prefix PATH : ${binPath}
      '';
    in
    rec {
      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
      postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
    }
  );

  ## Get the installable application (only static executable):
  thisApplication = pkgs.haskell.lib.justStaticExecutables (makeThisApplicationInstallable thisProject.hebele-hubele-app-cli);

  ## Note: You may wish to use following to disable checks instead:
  ##
  ## (pkgs.haskell.lib.dontCheck thisProject.hebele-hubele-app-cli)
in
if pkgs.lib.inNixShell then thisShell else {
  shell = thisShell;
  application = thisApplication;
}
