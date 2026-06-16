{
  pkgs,
  lib,
  config,
  inputs,
  std,
  ...
}:
let
  cfg = config.modules.zsh;
  additionalKeybindConfig = builtins.readFile ./key-bindings.zsh;
in
{
  options.modules.zsh = {
    enable = lib.mkEnableOption "zsh";
  };

  config = lib.mkIf cfg.enable {
    # don't care enough rn to create an enable option or
    # an entire module for eza :3
    programs.eza = {
      enable = true;
      enableZshIntegration = true;
      git = true;
      icons = "auto";

      extraOptions = [
        "--group-directories-first"
      ];
    };

    xdg.configFile."eza/theme.yml" = {
      source = ./eza/theme.yml;
    };

    xdg.configFile.".zshrc.d" = {
      source = ./.zshrc.d;
      recursive = true;
    };

    programs.zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd cd" ];
    };

    programs.zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestion.enable = true;
      defaultKeymap = null;

      initContent =
        let
          zshEarly = lib.mkOrder 550 ''
            ZNIXDIR="$HOME/.config/.zshrc.d"
            fpath=($ZNIXDIR $fpath)
          '';

          zshNormal = lib.mkOrder 1000 (
            ''
              autoload -Uz promptinit && promptinit && prompt violet
            ''
            + additionalKeybindConfig
          );
        in
        lib.mkMerge [
          zshEarly
          zshNormal
        ];

      shellAliases = {

        zi = "cdi";

        # some omz default aliases
        ll = "eza -lh";
        la = "eza -lah";
        "..." = "../..";
        "...." = "../../..";
        "....." = "../../../..";
        "......" = "../../../../..";
      };

      history = {
        size = 10000;
        path = "$HOME/.zsh_history";
        ignorePatterns = [
          "rm *"
          "mv *"
        ];
      };

      # not totally sold on antidote (configuring it feels weird)
      antidote = {
        enable = true;
        useFriendlyNames = true;
        plugins = [
          # "mattmc3/ez-compinit"
          # "zsh-users/zsh-completions kind:fpath path:src"
          #
          # "belak/zsh-utils path:editor"
          # "belak/zsh-utils path:history"
          # "belak/zsh-utils path:utility"
          #
          # "zdharma-continuum/fast-syntax-highlighting"
          # "zsh-users/zsh-autosuggestions"
          # "zsh-users/zsh-history-substring-search"

          "mattmc3/ez-compinit"

          "mattmc3/zephyr path:plugins/editor"
          "mattmc3/zephyr path:plugins/history"
          "mattmc3/zephyr path:plugins/prompt"
          "mattmc3/zephyr path:plugins/utility"

          "mattmc3/zephyr path:plugins/compstyle"
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
