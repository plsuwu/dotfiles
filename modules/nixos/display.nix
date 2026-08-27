{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.systemModules.display;
in
{
  options.systemModules.display = {
    enable = lib.mkEnableOption "display";
  };

  config = lib.mkIf cfg.enable {
    services.gnome.gnome-keyring.enable = true;
    services.dbus.packages = [ pkgs.gcr ];

    hardware = {
      graphics = {
        enable = true;
      };

      nvidia = {
        open = true;
        
        # package = config.boot.kernelPackages.nvidiaPackages.latest;
        # ---------------------------------------------------------------------
        # TODO: Pin to nvidia-open v610.57.04 while we wait for changes
        #       to make their way into nixpkgs. 
        #       (See: 
        #         - https://github.com/NVIDIA/open-gpu-kernel-modules/pull/1227,
        #         - https://github.com/NixOS/nixpkgs/issues/554125)
        #
        # ```
        # nvidia/os-interface.c: In function 'os_get_current_process_name':
        # nvidia/os-interface.c:764:5: error: implicit declaration of function 'strncpy' [-Wimplicit-function-declaration]
        #   764 |     strncpy(buf, current->comm, len - 1);
        #       |     ^~~~~~~
        # nvidia/os-interface.c:42:1: note: 'strncpy' is defined in header '<string.h>'; this is probably fixable by adding '#include <string.h>'
        #    41 | #include <linux/pid_namespace.h>
        #   +++ |+#include <string.h>
        #    42 | #if defined(CONFIG_LOCKDEP)
        #   CC [M]  nvidia/nv-report-err.o
        # make[5]: *** [/nix/store/wspyvii087y26q4jp83vvhp88x6m6wmz-linux-7.2-dev/lib/modules/7.2.0/source/scripts/Makefile.build:289: nvidia/os-interface.o] Error 1
        # ```
        #
        package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
          version = "610.57.04";
          sha256_64bit = "sha256-suk1xmuDuwDAyFe8jg7g/VLekoa0DJzB7sKafOfrEW0=";
          sha256_aarch64 = "sha256-QCefrMBCmpOwuOyXv1k5Gj0iB2CYlPgnG3JToUw/j54=";
          openSha256 = "sha256-rQHOOOY4KL92Ww3KDwh+j4eGU7oNAH8LutZC5wmFnPo=";
          settingsSha256 = "sha256-ZEMo8I8Zc2Tq6RVDNYpAH+f094dUaZiBqO+5f6lIjRI=";
          persistencedSha256 = "sha256-aXmD2VY1RLlgAnlHhOUMWzvMyhI6JTClcFLm4imF/mA=";
        };

        nvidiaSettings = true;
        modesetting.enable = true;
      };
    };

    services.xserver.videoDrivers = [ "nvidia" ];
    environment.sessionVariables.NIXOS_OZONE_WL = "1";
  };
}
