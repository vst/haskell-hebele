{ pkgs
, ...
}:

pkgs.stdenv.mkDerivation rec {
  name = "alphago";
  version = "0.12.0";

  meta = {
    homepage = "http://pandanet-igs.com/communities/gopanda2";
  };

  src = pkgs.fetchurl {
    url = "https://release.ariga.io/atlas/atlas-linux-amd64-musl-v${version}";
    sha256 = "sha256-HlvQWXzTaD2XB/yMnLp1ELiFQyTdXZXDXPoiHXFb9j0=";
  };

  phases = [ "installPhase" ];

  nativeBuildInputs = [ ];

  buildInputs = [ ];

  libPath = pkgs.lib.makeLibraryPath buildInputs;

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/atlas
    chmod +x $out/bin/atlas
  '';
}
