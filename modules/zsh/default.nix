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

      initContent = ''
        autoload -Uz promptinit && promptinit && prompt redhat
      '';

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
          "getantidote/use-omz"

          "ohmyzsh/ohmyzsh path:lib"
          "ohmyzsh/ohmyzsh path:plugins/colored-man-pages"
          "ohmyzsh/ohmyzsh path:plugins/gh"
          "ohmyzsh/ohmyzsh path:plugins/git"
          "ohmyzsh/ohmyzsh path:plugins/git-prompt"
          "ohmyzsh/ohmyzsh path:plugins/ssh"

          "mattmc3/ez-compinit"

          "zsh-users/zsh-completions kind:fpath path:src"
          "zsh-users/zsh-autosuggestions"
          "zsh-users/zsh-history-substring-search"

          "zdharma-continuum/fast-syntax-highlighting"
        ];
      };
    };
  };
}
