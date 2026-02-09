{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.boot;
in
{
  options.modules.boot = {
    enable = lib.mkEnableOption "boot";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      loader = {
        efi.canTouchEfiVariables = true;
        systemd-boot = {
          enable = true;
          configurationLimit = 5;
          consoleMode = "max";
        };
      };

      kernelPackages = pkgs.linuxPackages_latest;
      extraModprobeConfig = ''
        options hid_apple fnmode=2
        options iwlwifi power_save=0
      '';

      plymouth = {
        enable = true;
        theme = "abstract_ring_alt";
        themePackages = [
          (pkgs.adi1090x-plymouth-themes.override {
            selected_themes = [ "abstract_ring_alt" ];
          })
        ];
      };
    };
  };
}
