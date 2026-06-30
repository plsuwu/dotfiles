{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.zoom;
in
{
  options.systemModules.zoom = {
    enable = lib.mkEnableOption "zoom";
    createDesktopEntry = lib.mkOption {
      type = lib.types.bool;
      default = pkgs.stdenv.isLinux;
      description = "creates an XDG desktop entry for Linux systems";
    };
  };

  config = lib.mkIf (cfg.enable && cfg.createDesktopEntry) {
    home.packages = [ pkgs.zoom-us ];
    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        let
          zoom = "Zoom.desktop";
        in
        {
          "x-scheme-handler/zoommtg" = "${zoom}";
          "x-scheme-handler/zoomus" = "${zoom}";
          "x-scheme-handler/zoomphonecall" = "${zoom}";
          "x-scheme-handler/zoomphonesms" = "${zoom}";
          "x-scheme-handler/zoomcontactcentercall" = "${zoom}";
        };
    };

    xdg.desktopEntries.Zoom = {
      name = "Zoom (Native)";
      comment = "Zoom Video Conference";
      exec = "env QT_QPA_PLATFORMTHEME=xdgdesktopportal ${pkgs.zoom-us}/bin/zoom %U";
      icon = "Zoom";
      terminal = false;
      type = "Application";
      categories = [
        "Network"
        "Application"
      ];
      settings = {
        Keywords = "zoom;zoom-us;zoomus";
      };
      mimeType = [
        "x-scheme-handler/zoommtg"
        "x-scheme-handler/zoomus"
        "x-scheme-handler/zoomphonecall"
        "x-scheme-handler/zoomphonesms"
        "x-scheme-handler/zoomcontactcentercall"
        "application/x-zoom"
      ];
    };
  };
}
