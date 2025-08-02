{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  pname = "myracursor";
  version = "1.0";
  src = ./.;

  installPhase = ''
    mkdir -p $out/share/icons/myracursor/cursors
    cp -r cursors/* $out/share/icons/myracursor/cursors/
    cp -t $out/share/icons/myracursor/ cursor.theme index.theme 
  '';
}
