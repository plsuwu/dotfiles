{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.shell;
  extraKeybind = builtins.readFile ./keybinds.zsh;
in
{
  config = lib.mkIf cfg.enable {
    xdg.configFile.".zshrc.d" = {
      source = ./.zshrc.d;
      recursive = true;
    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestion.enable = true;
      defaultKeymap = null;

      initContent =
        let
          early = lib.mkOrder 550 ''
            ZNIXDIR="$HOME/.config/.zshrc.d"
            fpath=($ZNIXDIR $fpath)
          '';

          norm = lib.mkOrder 1000 (
            ''
              autoload -Uz promptinit && promptinit && prompt violet
              unsetopt interactive_comments
            ''
            + extraKeybind
          );
        in
        lib.mkMerge [
          early
          norm
        ];

      shellAliases = {
        zi = "cdi";
        la = "eza -lah";
      };

      antidote = {
        enable = true;
        useFriendlyNames = true;
        plugins = [
          "mattmc3/ez-compinit"
          "mattmc3/zephyr path:plugins/editor"
          "mattmc3/zephyr path:plugins/history"
          "mattmc3/zephyr path:plugins/prompt"
          # "mattmc3/zephyr path:plugins/utility"
          # "mattmc3/zephyr path:plugins/compstyle"
          "mattmc3/zephyr path:plugins/completion kind:defer"
          "zsh-users/zsh-completions kind:fpath path:src"
          "zsh-users/zsh-history-substring-search"
          "zsh-users/zsh-autosuggestions"
          "zdharma-continuum/fast-syntax-highlighting kind:defer"
        ];
      };
    };
  };
}
