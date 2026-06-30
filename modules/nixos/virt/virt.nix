{
  config,
  pkgs,
  ...
}:
let
  qemu-patched = pkgs.qemu.overrideAttrs (prev: {
    patches = (prev.patches or []) ++ [
      (pkgs.fetchpatch {
      })
    ];
  });  
in
{
}
