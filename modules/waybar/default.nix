{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.waybar;
in
{
  options.modules.waybar = {
    enable = lib.mkEnableOption "waybar";
  };

  config = lib.mkIf cfg.enable {
    home.file.".config/waybar/power.xml".source = ./power.xml;
    home.file.".config/waybar/svg" = {
      source = ./svg;
      recursive = true;
    };

    programs.waybar = {
      enable = true;
      systemd.enable = true;
      # systemd.target = "hyprland-session.target";
      systemd.targets = [ "hyprland-session.target" ];
      style = ./style.css;

      settings = [
        {
          modules-left = [
            "custom/power"
            "custom/separator"
            "hyprland/workspaces"
            "custom/separator"
            "hyprland/window"
          ];

          modules-center = [
          ];

          modules-right = [
            "network#ethernet"
            "network#wifi"
            "custom/separator"
            "pulseaudio"
            # "pulseaudio#source"
            "custom/separator"
            "clock"
          ];

          pulseaudio = {
            format = "";
            format-muted = "";
            on-click = "${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
            on-click-right = "${pkgs.pavucontrol}/bin/pavucontrol";
            scroll-step = 5;
            tooltip-format = "{desc}\n{volume}%";
            states = {
              "low" = 1;
              "medium" = 34;
              "high" = 67;
            };
          };

          # "ethernet" = {
          #   interface = "enp6s0";
          #   format = "";
          #   format-ethernet = "";
          #   format-disconnected = "";
          # };

          network = {
            interface = "wlo1";
            format-wifi = "";
            states = {
              "weak" = 1;
              "fair" = 20;
              "good" = 40;
              "strong" = 60;
              "excellent" = 80;
            };
          };

          # network = {
          #   # interface = "enp6s0";
          #   interface = "wlo1";
          #   format-ethernet = "[o]   ";
          #   format-wifi = "[o]   ";
          #
          #   format-disconnected = "[x] ";
          #   tooltip-format = "\n\n{ifname}: {ipaddr}/{cidr}\n\n : {bandwidthDownBytes}\n : {bandwidthUpBytes}";
          # };

          "hyprland/window" = {
            format = "{title}";
            separate-outputs = true;
          };
          "hyprland/workspaces" = {
            persistent-workspaces = {
              "*" = builtins.genList (i: i + 1) 10;
            };
          };

          clock = {
            format = "{:%a %d %b   %H:%M %p}";
          };

          "custom/separator" = {
            "format" = "    ";
          };

          "custom/power" = {
            "format" = "      ";
            "tooltip" = false;
            "menu" = "on-click";
            "menu-file" = "$HOME/.config/waybar/power.xml";
            "menu-actions" = {
              "shutdown" = "shutdown";
              "reboot" = "reboot";
              "lock" = "hyprlock";
              "sleep" = "";
            };
          };
        }
      ];
    };
  };
}
