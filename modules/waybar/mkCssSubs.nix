{
  pkgs,
  lib,
}:

{
  name,
  source,
  subs,
}:
let
  subArgs = lib.concatStringsSep " " (
    lib.mapAttrsToList (
      key: val: "--replace--quiet '@@${key}@@' '${toString val}'"
    ) subs
  );
in
pkgs.runCommand "${name}.css" { } ''
  cp ${source} $out
  chmod +w $out
  substituteInPlace $out ${subArgs}
''
