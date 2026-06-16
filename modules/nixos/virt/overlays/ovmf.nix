{ pkgs, ... }:
let
  ovmfPatched = pkgs.OVMF.overrideAttrs (old: {
    pname = "OVMF_hardened";
  });
in
{
}
