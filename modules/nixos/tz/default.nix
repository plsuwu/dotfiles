{ lib, config, ... }:
let
  cfg = config.modules.tz;
in
{
  options.modules.tz = {
    enable = lib.mkEnableOption "tz";
    zone = lib.mkOption {
      default = "Australia/Brisbane";
    };
  };

  config = lib.mkIf cfg.enable {
    time.timeZone = cfg.zone;
  };
}
