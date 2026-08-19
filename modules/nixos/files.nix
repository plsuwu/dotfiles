{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = {
    environment.systemPackages = with pkgs; [
      file-roller
      ffmpegthumbnailer
      thunar-archive-plugin
      thunar-volman
      thunar-vcs-plugin
      thunar-media-tags-plugin
    ];
    programs.thunar = {
      enable = true;
    };

    services.gvfs.enable = true;
    services.tumbler.enable = true;
  };
}
