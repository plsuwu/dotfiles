{
  lib,
  stdenv,
  fetchurl,
  perl,
  makeWrapper,
  ...
}:
let
  perlEnv = perl.withPackages (
    p: with p; [
      NetServer
      IPCShareable
      IOSocketSSL
      (NetDNS.overrideAttrs (old: rec {
        version = "1.33";
        name = "${old.pname or "Net-DNS"}-${version}";
        src = fetchurl {
          url = "mirror://cpan/authors/id/N/NL/NLNETLABS/Net-DNS-${version}.tar.gz";
          hash = "sha256-WkDnz1JOS9LDPPA7grR9Uwi3Egg6pe4YCwta9Uxx+9I=";
        };
        patches = [ ];
      }))
    ]
  );
in
stdenv.mkDerivation rec {
  # populates a Perl environment wrapper for `inetsim`
  pname = "inetsim";
  version = "1.3.2";
  src = fetchurl {
    url = "https://www.inetsim.org/downloads/inetsim-${version}.tar.gz";
    hash = "sha256-ZeJavJSFGwBUGDZw7jUaehL0hj9CiawKVrr3RX1f20w=";
  };

  nativeBuildInputs = [ makeWrapper ];

  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/inetsim $out/bin
    cp -r . $out/share/inetsim/
    rm -f $out/share/inetsim/setup.sh

    makeWrapper ${perlEnv}/bin/perl $out/bin/inetsim    \
      --add-flags "-T"                                  \
      --add-flags "-I ${perlEnv}/lib/perl5/site_perl"   \
      --add-flags "-I $out/share/inetsim/lib"           \
      --add-flags "$out/share/inetsim/inetsim"

    runHook postInstall
  '';

  meta = {
    description = "Software suite for simulating common internet services in a lab environment";
    homepage = "https://www.inetsim.org";
    license = lib.licenses.gpl3Plus;
    platforms = lib.platforms.linux;
    mainProgram = "inetsim";
  };
}
