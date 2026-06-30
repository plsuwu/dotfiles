{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.terminal;
  theme = "tokyo_night_enhanced";
  family = "Iosevka Comfy";
in
{
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.iosevka-comfy.comfy
    ];

    programs.alacritty = {
      inherit theme;
      enable = true;
      settings = {
        terminal.shell = {
          args = [
            "--login"
            "-c"
            "tmux"
          ];

          program = "${pkgs.zsh}/bin/zsh";
        };

        font = {
          size = 14;
          normal = {
            inherit family;
            style = "Regular";
          };
          bold = {
            inherit family;
            style = "Bold";
          };
          italic = {
            inherit family;
            style = "Italic";
          };
        };

        scrolling = {
          history = 50000;
          multiplier = 3;
        };

        window = {
          decorations = "full";
          dynamic_padding = false;
          padding = {
            x = 10;
            y = 10;
          };
        };
      };
    };
  };
}
