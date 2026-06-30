{
  config,
  lib,
  ...
}:
let
  cfg = config.systemModules.git;
in
{
  options.systemModules.git = {
    name = lib.mkOption {
      type = lib.types.str;
      description = "git username";
      default = "plsuwu";
    };

    email = lib.mkOption {
      type = lib.types.str;
      description = "git email";
      default = "124419933+plsuwu@users.noreply.github.com";
    };
  };

  config = {
    programs.git = {
      enable = true;
      settings = {
        user = {
          inherit (cfg) name email;
        };
      };
    };

    programs.gh = {
      enable = true;
      gitCredentialHelper = {
        enable = true;
      };
    };
  };
}
