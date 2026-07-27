{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.systemModules.desktop;
  wf-toggle = pkgs.writeShellScriptBin "wf-toggle" ''
    if pgrep -x wf-recorder > /dev/null; then
      ${pkgs.procps}/bin/pkill -INT -x wf-recorder
    else
      wf-recorder --audio -g "$(${pkgs.slurp}/bin/slurp)" -f "$HOME/Videos/Recordings/r_$(date +%Y%m%d_%H%M%S).mp4" &
    fi
  '';
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    home.packages = [
      pkgs.grimblast
      pkgs.wf-recorder
      pkgs.slurp

      wf-toggle
    ];
  };
}
