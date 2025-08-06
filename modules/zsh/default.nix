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
in
{
  options.modules.zsh = {
    enable = lib.mkEnableOption "zsh";
  };

  config = lib.mkIf cfg.enable {
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

          zshNormal = lib.mkOrder 1000 ''
            autoload -Uz promptinit && promptinit && prompt violet
          '';
        in
        lib.mkMerge [
          zshEarly
          zshNormal
        ];

      shellAliases = {
        ll = "ls -lh";
        la = "ls -lah";
        zi = "cdi";
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
          "mattmc3/ez-compinit"
          "zsh-users/zsh-completions kind:fpath path:src"

          "belak/zsh-utils path:editor"
          "belak/zsh-utils path:history"
          "belak/zsh-utils path:utility"

          "zdharma-continuum/fast-syntax-highlighting"
          "zsh-users/zsh-autosuggestions"
          "zsh-users/zsh-history-substring-search"
        ];
      };
    };
  };
}
