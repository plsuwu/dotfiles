{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.systemModules.greeter;
in
{
  options.systemModules.greeter = {
    enable = lib.mkEnableOption "greeter";
  };

  config = lib.mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          user = "greeter";
          # inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland
          command = "${pkgs.tuigreet}/bin/tuigreet --asterisks --time --cmd '${pkgs.hyprland}/bin/start-hyprland'";
        };
      };
    };

    programs.hyprland.enable = true;
    systemd.services.greetd.serviceConfig = {
      Type = "idle";
      StandardInput = "tty";
      StandardOutput = "tty";
      StandardError = "journal";

      TTYReset = true;
      TTYVHangup = true;
      TTYVTDisallocate = true;
    };
  };
}
