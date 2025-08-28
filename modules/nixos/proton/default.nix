{ pkgs, lib, config, ... }:
let
  cfg = config.modules.steam;
in {
  options.modules.proton = {
    enable = lib.mkEnabledOption "steam";
  };

  config = lib.mkIf cfg.enable {
    programs.steam.enable = true;
  };
}
