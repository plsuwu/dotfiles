{ lib, config, pkgs, ... }:

let
  cfg = config.please;
in
{
  options.please = {
    enable = lib.mkEnableOption "enable user module";

    userName = lib.mkOption {
      default = "please";
      description = ''
        username
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.userName} = {
      isNormalUser = true;
      description = "please";
      extraGroups = [ "wheel" "networkmanager" ];
      shell = pkgs.zsh;
    };
  };
}
