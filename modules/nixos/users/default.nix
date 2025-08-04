{
  lib,
  config,
  user,
  pkgs,
  ...
}:
let
  cfg = config.modules.users;
in
{
  options.modules.users = {
    enable = lib.mkEnableOption "users";
  };

  config = lib.mkIf cfg.enable {
    users.defaultUserShell = pkgs.zsh;
    programs.zsh.enable = true;

    users.users.${user.name} = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "dialout"
      ]
      ++ lib.optional config.modules.virt.enable "libvirtd"
      ++ lib.optional config.modules.docker.enable "docker"
      ++ lib.optional config.modules.net.enable "networkmanager";

      createHome = true;
      shell = pkgs.zsh;
    };

    environment.variables = {
      XDG_DATA_HOME = "$HOME/.local/share";
    };
  };
}
