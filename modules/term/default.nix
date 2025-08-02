{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.term;
in
{
  options.modules.term = {
    enable = lib.mkEnableOption "term";
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      theme = "tokyo_night_enhanced";
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
          size = 11;

          bold.family = "JetBrainsMonoNerdFont";
          bold.style = "Bold";
          italic.family = "JetBrainsMonoNerdFont";
          italic.style = "Italic";
          normal.family = "JetBrainsMonoNerdFont";
          normal.style = "Regular";
        };

        scrolling = {
          history = 50000;
          multiplier = 3;
        };

        window = {
          decorations = "full";
          dynamic_padding = false;
          padding.x = 14;
          padding.y = 10;
        };
      };
    };

    programs.tmux = {
      enable = true;
      clock24 = true;

      prefix = "C-a";
      terminal = "tmux-256color";
      historyLimit = 50000;

      escapeTime = 50;
      focusEvents = true;
      baseIndex = 1;
      disableConfirmationPrompt = true;

      mouse = true;
      extraConfig = ''
        set-option -sa terminal-features ",alacritty:RGB"

        bind '"' split-window -c '#{pane_current_path}'
        bind '%' split-window -h -c '#{pane_current_path}'
        bind c new-window -c '#{pane_current_path}'
      '';
    };
  };
}
