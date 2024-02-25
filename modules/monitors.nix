{ lib, config, ... }:

let
  inherit (lib) mkOption types;
  # cfg = config.monitors;
in
{
  options.monitors = mkOption {
    type = types.listOf (types.submodule {
      options = {
        name = mkOption {
          type = types.str;
          example = "DP-0";
        };
        primary = mkOption {
          type = types.bool;
          default = false;
        };
        width = mkOption {
          type = types.int;
          default = 2560;
        };
        height = mkOption {
          type = types.int;
          default = 1440;
        };
        rate = mkOption {
          type = types.int;
          example = 165;
        };
        x = mkOption {
          type = types.int;
          default = 0;
        };
        y = mkOption {
          type = types.int;
          default = 0;
        };
        enabled = mkOption {
          type = types.bool;
          default = true;
        };
      };
    });
    default = [ ];
  };
  config = {
    assertions = [{
      assertion = ((lib.length config.monitors) != 0) ->
        ((lib.length (lib.filter (m: m.primary) config.monitors)) == 1);
      message = "invalid number of primary monitors.";
    }];
  };
}
