{
  lib,
  inputs,
  pkgs,
  ...
}:
let
  workspaces = builtins.concatLists (
    builtins.genList (
      x:
      let
        ws =
          let
            c = (x + 1) / 10;
          in
          builtins.toString (x + 1 - (c * 10));
      in
      [
        "$mod SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}"
        "$mod CTRL, ${ws}, movetoworkspace, ${toString (x + 1)}"
        "$mod, ${ws}, focusworkspaceoncurrentmonitor, ${toString (x + 1)}"
      ]
    ) 10
  );
in
{
  home.packages = with pkgs; [
    dconf

    grim
    slurp
  ];

  home.sessionVariables = {
    QT_QPA_PLATFORM = "wayland";
    SDL_VIDEODRIVER = "wayland";
    XDG_SESSION_TYPE = "wayland";
  };

  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    config = {
      common.default = [ "gtk" ];
      hyprland.default = [
        "gtk"
        "hyprland"
      ];
    };

    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
  wayland.windowManager.hyprland = {
    enable = true;
    systemd = {
      enable = true;
      variables = [ "--all" ];
      extraCommands = [
          "systemctl --user stop graphical-session.target"
        "systemctl --user start hyprland-session.target"
      ];
    };

    settings = lib.mkOptionDefault {
      "$mod" = "SUPER";

      bind = [
        "$mod, mouse:272, setfloating"
        "$mod, Return, exec, alacritty"
        "$mod, T, settiled,"
        "$mod, Q, killactive,"
        "$mod, F, fullscreen, 0"
        "$mod, D, exec, tofi-drun"
        "$mod, W, exec, firefox"
        "$mod SHIFT, C, pin,"
        "$mod SHIFT, E, exit,"

        "$mod, L, movefocus, right"
        "$mod, H, movefocus, left"
        "$mod, K, movefocus, up"
        "$mod, H, movefocus, down"

      ] ++ workspaces;

      binde = [
        "$mod SHIFT, H, resizeactive, -25 0"
        "$mod SHIFT, L, resizeactive, 25 0"
      ];

      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
        "$mod ALT, mouse:272, resizewindow"
      ];

      input = {
        repeat_delay = 200;
        repeat_rate = 50;
      };

      animations = {
        enabled = true;
        bezier = [
          "expin, 0.01, 1, 0.2, 0.99"
          "expout, 0.75, 0.15, 0.95, 0.25"
          "easioquint, 0.86, 0, 0.07, 1"
        ];
        animation = [
          "border, 1, 2, default"
          "fade, 1, 1.3, default"
          "windows, 1, 1, easioquint"
          "workspaces, 1, 1, easioquint, slide"
        ];
      };

      monitor = [
        # "Dell Inc. DELL S2721DGF GY2PS83 (DP-1)"
        "DP-1, 2560x1440@165.08, 0x-1440, 1"

        # "Acer Technologies XB323U TKWSA0018523 (DP-2)"
        "DP-2, 2560x1440@170.02, 0x0, 1"

        # "Samsung Electric Company Odyssey G40B HNMW300577 (DP-3)"
        "DP-3, 1920x1080@239.76, 2560x-720, 1, transform, 1"
      ];
    };

    extraConfig = ''
      bind = $mod, E, submap, changeMonitor

      submap = changeMonitor
      bind = , E, focusmonitor, DP-2
      bind = , W, focusmonitor, DP-1
      bind = , R, focusmonitor, DP-3

      bind = , catchall, submap, reset
      submap = reset
    '';

  };
}
