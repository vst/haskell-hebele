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
      openapi3 = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken super.openapi3);

      hebele-core = self.callCabal2nix "hebele-core" ./hebele-core { };
      hebele-domain = self.callCabal2nix "hebele-domain" ./hebele-domain { };
      hebele-app-cli = self.callCabal2nix "hebele-app-cli" ./hebele-app-cli { };
      hebele-app-server = self.callCabal2nix "hebele-app-server" ./hebele-app-server { };
    };
  }).extend (pkgs.haskell.lib.packageSourceOverrides { });

  ## Prepare Nix shell:
  thisShell = thisProject.shellFor {
    ## Define packages for the shell:
    packages = p: [
      p.hebele-core
      p.hebele-domain
      p.hebele-app-cli
      p.hebele-app-server
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

      ## Database related inputs:
      (import ./nix/ext/atlasgo.nix { pkgs = pkgs; })
    ];

    ## Shell hook for development shell:
    shellHook = ''
      echo "Welcome to Development Shell"
    '';
  };

  ## Define a function that makes this CLI application installable in Nix environment:
  makeThisAppCliInstallable = drv: drv.overrideAttrs (oldAttrs:
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
        wrapProgram $out/bin/hebele-app-cli --prefix PATH : ${binPath}
      '';
    in
    rec {
      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
      postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
    }
  );

  ## Get the installable application (only static executable):
  thisAppCli = pkgs.haskell.lib.justStaticExecutables (makeThisAppCliInstallable thisProject.hebele-app-cli);

  ## Define a function that makes this server application installable in Nix environment:
  makeThisAppServerInstallable = drv: drv.overrideAttrs (oldAttrs:
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
        wrapProgram $out/bin/hebele-app-server --prefix PATH : ${binPath}
      '';
    in
    rec {
      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
      postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
    }
  );

  ## Get the installable application (only static executable):
  thisAppServer = pkgs.haskell.lib.justStaticExecutables (makeThisAppServerInstallable thisProject.hebele-app-server);

  ## Note: You may wish to use following to disable checks instead:
  ##
  ## (pkgs.haskell.lib.dontCheck thisProject.hebele-app-cli)
in
if pkgs.lib.inNixShell then thisShell else {
  shell = thisShell;
  appCli = thisAppCli;
  appServer = thisAppServer;
}
