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
          size = 13;

          bold.family = "Iosevka Nerd Font";
          bold.style = "Bold";
          italic.family = "Iosevka Nerd Font";
          italic.style = "Italic";
          normal.family = "Iosevka Nerd Font";
          normal.style = "Medium";
        };

        scrolling = {
          history = 50000;
          multiplier = 3;
        };

        window = {
          decorations = "full";
          dynamic_padding = false;
          padding.x = 10;
          padding.y = 10;
        };
      };
    };

    programs.tmux = {
      enable = true;
      clock24 = true;
      plugins = with pkgs.tmuxPlugins; [
        harpoon
        fzf-tmux-url
        logging

        tmux-which-key
      ];

      prefix = "C-a";
      terminal = "screen-256color";
      historyLimit = 50000;

      escapeTime = 0;
      focusEvents = true;
      baseIndex = 1;
      disableConfirmationPrompt = true;

      mouse = true;
      extraConfig = ''
        set -g display-time 4000
        set-option -sa terminal-features ",alacritty:RGB"

        bind '"' split-window -c '#{pane_current_path}'
        bind '/' split-window -h -c '#{pane_current_path}'
        bind c new-window -c '#{pane_current_path}'
        
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R
        bind -n C-x kill-pane
      '';
    };
  };
}
