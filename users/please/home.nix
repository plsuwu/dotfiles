{
  username,
  pkgs,
  ...
}:
let
in
{
  imports = [ ../../modules/home-manager ];
  config.systemModules = {
    desktop.enable = true;
    vesktop.enable = true;
    lutris.enable = true;

    shell.enable = true;
    nvim.enable = true;
    terminal.enable = true;
    browsers.enable = true;
    zoom.enable = true;
  };

  config.home = {
    inherit username;

    homeDirectory = "/home/${username}";
    sessionVariables = {
      EDITOR = "nvim";
    };

    stateVersion = "25.11";
  };
}
