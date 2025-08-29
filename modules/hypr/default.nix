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
      hyprland-qtutils

      wl-clipboard
      cliphist
      slurp
      grimblast
      # polkit_gnome
      swappy

      pinta
    ];

    services.hyprpolkitagent.enable = true;
    home.sessionVariables = {
      QT_QPA_PLATFORM = "wayland";
      SDL_VIDEODRIVER = "wayland";
      XDG_SESSION_TYPE = "wayland";
      GRIMBLAST_EDITOR = "${pkgs.pinta}/bin/pinta";
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
          gaps_in = 2;
          gaps_out = 3;
          layout = "dwindle";
        };

        exec-once = [
          # "${pkgs.hyprpaper}/bin/hyprpaper"
          "${pkgs.vesktop}/bin/vesktop"
          "${pkgs.wl-clipboard}/bin/wl-paste --type text --watch ${pkgs.cliphist}/bin/cliphist store"
          "${pkgs.wl-clipboard}/bin/wl-paste --type image --watch ${pkgs.cliphist}/bin/cliphist store"
        ];

        decoration = {
          rounding = 4;
        };

        windowrule = [
          "workspace 10, class:vesktop"
        ];

        windowrulev2 = [

          # this (probably) only floats Pinta windows called via `grimblast edit`
          "float,title:^(\\d{4}\-\\d{2}\-\\d{2}T\\d{2}:\\d{2}:\\d{2},\\d+.*\\.png\ \-\ Pinta)$"
          "size 70% 80%,title:^(\\d{4}\-\\d{2}\-\\d{2}T\\d{2}:\\d{2}:\\d{2},\\d+.*\\.png\ \-\ Pinta)$"

          "float,class:(*polkit*agent)"
          "pin,class:(*polkit*agent)"
          "stayfocused,class:(*polkit*agent)"

          "float,title:(*Bitwarden\ Password\ Manager*),class:firefox"
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
          "${mod}, S, exec, grimblast edit area"
          "${mod} SHIFT, S, exec, grimblast copysave area ~/Pictures/Screenshots/$(date +%4Y%m%d-%S%N).png"
          "${mod}, End, exec, grimblast --cursor copysave screen ~/Pictures/screenshots/$(date +%4Y%m%d-%S%N).png"

          "${mod}, L, movefocus, r"
          "${mod}, H, movefocus, l"
          "${mod}, K, movefocus, u"
          "${mod}, J, movefocus, d"

          "${mod} SHIFT, K, movewindow, r"
          "${mod} SHIFT, J, movewindow, l"
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

        workspace = [
          "10,monitor:DP-3"
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
            "easio, 0.74, 0.12, 0.28, 0.9"
          ];
          animation = [
            "border, 1, 2.5, default"

            "fadeIn, 1, 1, default"
            "fadeOut, 1, 1, default"

            "workspaces, 0"
            "windows, 0"

            # "windowsIn, 1, 1, easioquint, slide"
            # "windowsOut, 1, 1, easioquint, slide"
            # "windowsMove, 1, 1, easio, slide"

            # "workspacesIn, 1, 2, expin, fade"
            # "workspacesOut, 1, 2, expout, fade"
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
            timeout = 1800;
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
          cursor-theme = "myramors";
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
        gtk-recent-files-enabled = 0;
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
