final: prev: {
  rodinNTLG = final.callPackage (
    {
      lib,
      stdenvNoCC,
      fetchurl,
    }:
    stdenvNoCC.mkDerivation {
      pname = "FOT-RodinNTLG-Pro";
      version = "1.0.0";

      src = fetchurl {
        url = "https://storage.googleapis.com/darkened-finance-sauna-siamese/public/RodinNTLG_otf.tar.gz";
        hash = "sha256-tueWXbwvSfq2UFx/SWrfEk5j70boct8c6EMXaKdvfqE=";
      };

      # no top level directory
      sourceRoot = ".";

      dontConfigure = true;
      dontBuild = true;

      installPhase = ''
        runHook preInstall
        find . -type f -iname '*.otf' -exec install -Dm644 -t $out/share/fonts/opentype {} +
        runHook postInstall
      '';

      meta = with lib; {
        description = "Font Works ロダンNTLG Pro";
        homepage = "https://fontworks.co.jp/";
        platforms = platforms.all;
      };
    }
  ) { };
}
