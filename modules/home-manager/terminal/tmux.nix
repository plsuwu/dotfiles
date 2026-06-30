{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.terminal;
in
{
  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      clock24 = true;
      plugins = with pkgs.tmuxPlugins; [
        harpoon
        fzf-tmux-url
        logging
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
