{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.systemModules.desktop;
  luaPath = "src/dotfiles/modules/home-manager/desktop/swayimg/config.lua";
  mimeType =
    let
      imgExts = [
        "avif"
        "bmp"
        "gif"
        "heif"
        "jpeg"
        "jpg"
        "jxl"
        "pbm"
        "pjpeg"
        "png"
        "svg+xml"
        "tiff"
        "webp"
        "x-bmp"
        "x-exr"
        "x-png"
        "x-portable-anymap"
        "x-portable-bitmap"
        "x-portable-graymap"
        "x-portable-pixmap"
        "x-targa"
        "x-tga"
      ];
    in
    map (m: "image/${m}") imgExts;

in
{
  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    home.packages = [
      pkgs.swayimg
    ];

    home.file.".local/share/swayimg/swayimg.lua".source =
      "${pkgs.swayimg}/share/swayimg/swayimg.lua";

    xdg.configFile."swayimg/init.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/${luaPath}";

    xdg.desktopEntries.swayimg = {
      inherit mimeType;
      type = "Application";
      name = "Swayimg";
      genericName = "Image viewer";
      comment = "Image viewer for Wayland";
      exec = "${pkgs.swayimg}/bin/swayimg --viewer %F";
      icon = "swayimg";
      terminal = false;
      categories = [
        "Graphics"
        "Viewer"
      ];
      settings = {
        Keywords = "Sway;img;image;viewer;view";
      };
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications = lib.attrsets.genAttrs mimeType (_: [ "swayimg.desktop" ]);
    };
  };
}
