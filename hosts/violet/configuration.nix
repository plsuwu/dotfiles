{
  config,
  pkgs,
  user,
  ...
}:
{
  config = {
    modules = {
      boot.enable = true;
      virt.enable = true;
      docker.enable = true;
      desktop-manager.enable = true;
      sound.enable = true;
      tz.enable = true;
      users.enable = true;
      thunar.enable = true;
      net.enable = true;
      auth.enable = true;
      nix-ld.enable = true;
    };
  };

  config = {
    fonts.enableDefaultPackages = true;

    programs.zsh.enable = true;
    programs.steam.enable = true;
  };

}
