{
  inputs,
  lib,
  config,
  pkgs,
  user,
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
        "${mod} SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}"
        "${mod} CTRL, ${ws}, movetoworkspace, ${toString (x + 1)}"
        "${mod}, ${ws}, focusworkspaceoncurrentmonitor, ${toString (x + 1)}"
      ]
    ) 10
  );

  cfg = config.modules.hypr;
  mod = cfg.modifier;
in
{
  imports = [
    ./cursor.nix
  ];

  options.modules.hypr = {
    enable = lib.mkEnableOption "hypr";
    modifier = lib.mkOption {
      default = "SUPER";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      swaybg

      wl-clipboard
      cliphist
      slurp
      grimblast
      polkit_gnome
      swappy
    ];

    services.hyprpolkitagent.enable = true;
    home.sessionVariables = {
      QT_QPA_PLATFORM = "wayland";
      SDL_VIDEODRIVER = "wayland";
      XDG_SESSION_TYPE = "wayland";
    };

    xdg.userDirs = {
      enable = true;
      pictures = "${config.home.homeDirectory}/Pictures";
    };

    home.activation.createScreenshots = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p ${config.home.homeDirectory}/Pictures/screenshots
    '';

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
        variables = [
          "DISPLAY"
          "HYPRLAND_INSTANCE_SIGNATURE"
          "WAYLAND_DISPLAY"
          "XDG_CURRENT_DESKTOP"

          "XDG_SESSION_ID"
          "XDG_SEAT"
          "XDG_SESSION_TYPE"
          "XDG_BACKEND"
          "XDG_VNTR"
        ];
        extraCommands = [
          "systemctl --user start hyprpolkitagent"
          "systemctl --user stop graphical-session.target"
          "systemctl --user start hyprland-session.target"
        ];
      };

      settings = {
        "$mod" = mod;

        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
        };

        general = {
          gaps_in = 5;
          gaps_out = 5;
          layout = "dwindle";
        };

        exec-once = [
          "${pkgs.hyprpaper}/bin/hyprpaper"
          "${pkgs.vesktop}/bin/vesktop"
          "${pkgs.wl-clipboard}/bin/wl-paste --type text --watch ${pkgs.cliphist}/bin/cliphist store"
          "${pkgs.wl-clipboard}/bin/wl-paste --type image --watch ${pkgs.cliphist}/bin/cliphist store"
        ];

        windowrule = [
          "workspace 10, class:vesktop"
        ];

        windowrulev2 = [
          "float,class:(polkit-gnome-authentication-agent-1)"
          "move 37% 2%,class:(polkit-gnome-authentication-agent)"
          "size 25% 10%,class:(polkit-gnome-authentication-agent)"
          # "center,class:(polkit-gnome-authentication-agent-1)"
          "pin,class:(polkit-gnome-authentication-agent-1)"
          "stayfocused,class:(polkit-gnome-authentication-agent-1)"
        ];

        layerrule = [
          "noanim,hyprpaper"
        ];

        bind = [
          "${mod}, mouse:272, setfloating"
          "${mod}, Return, exec, alacritty"
          "${mod}, T, settiled,"
          "${mod}, Q, killactive,"
          "${mod}, F, fullscreen, 0"
          "${mod}, D, exec, wofi -G --show drun"
          "${mod}, W, exec, firefox"
          "${mod} SHIFT, C, pin,"
          "${mod} SHIFT, E, exit,"

          "${mod}, V, exec, cliphist list | wofi --dmenu | cliphist decode | wl-copy"
          # "${mod} SHIFT, S, exec, grim -g \"$(slurp -d)\" - | wl-copy"
          # "${mod} SHIFT, Print, exec, grim -g \"$(slurp -d)\" - | wl-copy"
          "${mod} SHIFT, S, exec, grimblast save area - | wl-copy"
          "${mod}, S, exec, grimblast save area ~/Pictures/screenshots/$(date +%4Y%m%d-%S%N).png"
          "${mod}, Print, exec, grimblast save screen ~/Pictures/screenshots/$(date +%4Y%m%d-%S%N).png"

          "${mod}, L, movefocus, right"
          "${mod}, H, movefocus, left"
          "${mod}, K, movefocus, up"
          "${mod}, H, movefocus, down"

        ]
        ++ workspaces;

        binde = [
          "${mod} SHIFT, H, resizeactive, -25 0"
          "${mod} SHIFT, L, resizeactive, 25 0"
        ];

        bindm = [
          "${mod}, mouse:272, movewindow"
          "${mod}, mouse:273, resizewindow"
          "${mod} ALT, mouse:272, resizewindow"
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
            "windows, 1, 1, easioquint, gnomed"
            "workspaces, 1, 1, easioquint, fade"
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

    services.hypridle = {
      enable = true;
      settings = {
        listener = [
          {
            timeout = 300;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };

    services.hyprpaper = {
      enable = true;

      settings = {
        ipc = "off";
        splash = false;
        # splash_offset = 2.0;
        preload = [ "/home/${user.name}/.config/hypr/kronii.jpg" ];
        wallpaper = [ ", /home/${user.name}/.config/hypr/kronii.jpg" ];
      };
    };

    programs.hyprlock = {
      enable = true;
      settings = {
        general = {
          disable_loading_bar = true;
        };
      };
    };

    dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/interface" = {
          gtk-theme = "Tokyonight-Dark";
          color-scheme = "prefer-dark";
          # cursor-theme = "macOS";
          cursor-theme = "myracursor";
        };
      };
    };

    gtk = {
      enable = true;
      theme = {
        name = "Tokyonight-Dark";
        package = pkgs.tokyonight-gtk-theme;
      };
      iconTheme = {
        name = "Adwaita-dark";
        package = pkgs.adwaita-icon-theme;
      };

      # cursorTheme = {
      #   name = "myracursor";
      #   # package = pkgs.apple-cursor;
      # };

      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = 1;
      };
    };

    qt = {
      enable = true;
      platformTheme.name = "gtk";
    };

    home.file.".config/hypr/kronii.jpg".source = ./kronii.jpg;
    systemd.user.sessionVariables = config.home.sessionVariables;
  };
}
