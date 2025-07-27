{
  lib,
  config,
  pkgs,
  ...
}:
{
  home.username = "please";
  home.homeDirectory = "/home/please";

  home.packages = with pkgs; [
    rustup
    zip
    xz
    unzip
    p7zip

    fzf
    ripgrep

    file
    which
    gnused
    gnutar
    gawk
  ];

  programs.vesktop = {
    enable = true;
  };

  programs.git = {
      enable = true;
      userName = "plsuwu";
      userEmail = "124419933+plsuwu@users.noreply.github.com";
  };

  programs.gh = {
      enable = true;
  };


  programs.tofi = {
    enable = true;
    settings =
      let
        # i'm sure this path can somehow be derived from
        # `pkgs.nerd-fonts.jetbrains-mono` ://
        fontPath = "share/fonts/truetype/NerdFonts/JetBrainsMono";
        fontFile = "JetBrainsMonoNerdFont-Medium.ttf";
      in
      {
        font = ''
          "${pkgs.nerd-fonts.jetbrains-mono}/${fontPath}/${fontFile}"
        '';
      };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.firefox.enable = true;

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

  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };
  };

  gtk = {
    enable = true;
    theme.name = "Adwaita-dark";
    theme.package = pkgs.adw-gtk3;
    gtk3.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
    gtk4.extraConfig = {
      Settings = ''
        gtk-application-prefer-dark-theme=1
      '';
    };
  };

  qt = {
    enable = true;
    style.name = "adwaita-dark";
  };

  imports = [
    ./nvim
    ./eww
  ];

  systemd.user.startServices = "sd-switch";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;
}
