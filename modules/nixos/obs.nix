{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.obs;
in
{
  options.systemModules.obs = {
    enable = lib.mkEnableOption "obs studio";
  };

  config = lib.mkIf cfg.enable {
    programs.obs-studio = {
      enable = true;
      package = (
        pkgs.obs-studio.override {
          cudaSupport = true;
        }
      );

      plugins = with pkgs.obs-studio-plugins; [
        obs-vkcapture
        obs-vnc
        obs-pipewire-audio-capture
        obs-advanced-masks
        obs-livesplit-one
      ];
    };
  };
}
