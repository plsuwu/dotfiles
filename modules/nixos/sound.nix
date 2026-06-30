{
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.sound;
in
{
  options.systemModules.sound = {
    enable = lib.mkEnableOption "sound";
  };

  config = lib.mkIf cfg.enable {
    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      pulse.enable = true;
      jack.enable = true;

      alsa = {
        enable = true;
        support32Bit = true;
      };
    };

    programs.noisetorch.enable = true;
    xdg.sounds.enable = true;
  };
}
