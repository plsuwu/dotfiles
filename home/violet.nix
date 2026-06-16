{
  pkgs,
  inputs,
  config,
  ...
}:
{
  imports = [
    ../modules
  ];

  config.modules = {
    waybar.enable = true;
    media-utils.enable = true;
    discord.enable = true;
    myracursor.enable = true;
    term.enable = true;
    pipewire.enable = true;
    zsh.enable = true;
    hypr.enable = true;
    obs.enable = true;
    nh.enable = true;
    browser.enable = true;

    nvim.enable = true;
  };

  config = {

    gtk.gtk4.theme = config.gtk.theme;
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/http" = [ "brave.desktop" ];
        "x-scheme-handler/https" = [ "brave.desktop" ];
        "application/pdf" = [ "brave.desktop" ];
        "text/html" = [ "brave.desktop" ];
        "x-scheme-handler/zoommtg" = [ "Zoom.desktop" ];
        "x-scheme-handler/zoomus" = [ "Zoom.desktop" ];
      };
    };

    xdg.configFile."mimeapps.list".force = true;

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableZshIntegration = true;
      config.global.hide_env_diff = true;
    };

    # TODO: put these somewhere real later
    programs.rbw.enable = true;
    programs.git = {
      enable = true;
      settings.user = {
        name = "plsuwu";
        email = "124419933+plsuwu@users.noreply.github.com";
      };
    };
    programs.gh = {
      enable = true;
      gitCredentialHelper = {
        enable = true;
      };
    };
    programs.wofi = {
      enable = true;
    };

    systemd.user.startServices = "sd-switch";
  };
}
