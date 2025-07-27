{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    greeter = {
      greetd.enable = lib.mkEnableOption "enables the greetd greeter";
      command = lib.mkOption {
        type = lib.types.str;
        description = "command to execute from the greeter on login";
        default = "hyprland";
      };
    };
  };

  config = lib.mkIf config.greeter.greetd.enable {
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd ${config.greeter.command}";
          user = "greeter";
        };
      };
    };

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
