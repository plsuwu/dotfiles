{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.systemModules.desktop;
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    # wayland.windowManager.hyprland.settings.windowRulev2 = [
    #   "animation slide left,initialclass:(SwayNotificationCenterControlCenter)"
    # ];
    home.packages = [ pkgs.libnotify ];


    services.swaync = {
      enable = true;
      settings = {
        positionX = "right";
        positionY = "top";
        layer = "overlay";

        control-center-positionX = "right";
        control-center-positionY = "top";
        control-center-margin-top = 4;
        control-center-margin-bottom = 4;
        control-center-margin-right = 4;
        control-center-margin-left = 4;
        control-center-width = 500;
        control-center-height = 600;
        control-center-layer = "overlay";
        fit-to-screen = false;

        cssPriority = "user";

        notification-icon-size = 64;
        notification-body-image-height = 100;
        notification-body-image-width = 200;
        notification-inline-replies = true;
        notification-window-width = 500;

        timeout = 10;
        timeout-low = 5;
        timeout-critical = 0;

        keyboard-shortcuts = true;
        image-visibility = "when-available";
        transition-time = 200;
        hide-on-action = true;
        hide-on-clear = true;
        script-fail-notify = true;

        widgets = [
          "inhibitors"
          "dnd"
          "notifications"
        ];

        widget-config = {
          inhibitors = {
            text = "Inhibitors";
            button-text = "Clear All Inhibitors";
            clear-all-button = true;
          };

          title.text = "Notifications";
          dnd.text = "Do Not Disturb";
          # label.text = "label text";
        };
      };
    };
  };
}
