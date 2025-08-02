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
    discord.enable = true;
    myracursor.enable = true;
    term.enable = true;
    pipewire.enable = true;
    nvim.enable = true;
    zsh.enable = true;
    hypr.enable = true;
  };

  config = {

    # TODO: put these somewhere real later
    programs.firefox.enable = true;
    programs.rbw.enable = true;
    programs.git = {
      enable = true;
      userName = "plsuwu";
      userEmail = "124419933+plsuwu@users.noreply.github.com";
    };
    programs.gh = {
      enable = true;
    };
    programs.wofi = {
      enable = true;
    };
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    programs.spotify-player = {
      enable = true;
    };

    systemd.user.startServices = "sd-switch";
  };
}
