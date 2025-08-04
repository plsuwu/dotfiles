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
      systemd.target = "hyprland-session.target";
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

          modules-right = [
            "custom/separator"
            "network"
            "pulseaudio"
            "custom/separator"
            "clock"
          ];

          pulseaudio = {
            format = "      {volume}%";
            on-click = "pavucontrol";
            tooltip-format = "\n\n{desc}";
          };

          network = {
            interface = "enp6s0";
            format-ethernet = "    ";
            format-disconnected = "    ";
            tooltip-format = "\n\n{ifname}: {ipaddr}/{cidr}\n\n : {bandwidthDownBytes}\n : {bandwidthUpBytes}";
          };

          "hyprland/window" = {
            format = "{initialTitle}";
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
              "sleep" = "hyprctl dispatch dpms off";
            };
          };
        }
      ];
    };
  };
}
