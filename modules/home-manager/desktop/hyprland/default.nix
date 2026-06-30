{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.systemModules.desktop;
  luaPath = "src/dotfiles/modules/home-manager/desktop/hyprland/lua";
in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    home = {
      activation.createScreenshotDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        mkdir -p ${config.home.homeDirectory}/Pictures/Screenshots
        mkdir -p ${config.home.homeDirectory}/Videos/Recordings
      '';

      packages = with pkgs; [
        wl-clipboard
        ffmpeg-full
        wf-recorder
        pwvucontrol
        grimblast
        cliphist
        luajit
        slurp
        awww
        wofi
        wev
      ];
    };

    xdg.configFile."hypr/hyprland.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/${luaPath}/hyprland.lua";

    # link lsp stubs for hyprland lua API
    home.file.".local/share/hypr/stubs".source =
      "${pkgs.hyprland}/share/hypr/stubs";

    # force nix to eval plugins properly and gives us a reliable way to source them in our config
    xdg.configFile."hypr/nix-plugin.lua".text = ''
      return { 
        "${pkgs.hyprlandPlugins.hyprbars}/lib/libhyprbars.so" 
      }
    '';

    wayland.windowManager.hyprland = {
      enable = true;
      configType = "lua";
      package = pkgs.hyprland;
      systemd = {
        enable = true;
        variables = [ "--all" ];
      };

      plugins = [
        pkgs.hyprlandPlugins.hyprbars
      ];

      # populate some config field with junk to silence heuristic warnings
      # about our hpyrland config being missing
      extraConfig = "#";
    };

    systemd.user.sessionVariables = config.home.sessionVariables;
  };
}
