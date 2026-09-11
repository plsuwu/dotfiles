{
  pkgs,
  ...
}:
let
  stateDir = "/var/lib/inetsim";
in
{
  environment.etc."inetsim/inetsim.conf".source = ./inetsim.conf;
  environment.etc."libvirt/qemu/networks/net-isolated.xml".source =
    ./net-isolated.xml;

  users.groups.inetsim = { };
  users.users.inetsim = {
    isSystemUser = true;
    group = "inetsim";
  };

  systemd.services.inetsim-network = {
    description = "ensure isolated libvirt network is defined and active";
    after = [ "libvirtd.service" ];
    requires = [ "libvirtd.service" ];

    # trigger via manual `inetsim.service` start
    wantedBy = [ ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    path = [ pkgs.libvirt ];

    script = ''
      set -euo pipefail

      NET=isol
      XML=/etc/libvirt/qemu/networks/net-isolated.xml

      if ! virsh net-info "$NET" >/dev/null 2>&1; then
        virsh net-define "$XML"
      fi

      virsh net-autostart "$NET"
      if ! virsh net-info "$NET" 2>/dev/null | grep -q '^Active: *yes'; then
        virsh net-start "$NET"
      fi

      virsh net-info "$NET" | grep -q '^Active: *yes'
    '';
  };

  systemd.services.inetsim = {
    description = "INetSim: Internet Services Simulation Suite";
    after = [
      "libvirtd.service"
      "network.target"
      "inetsim-network.service"
    ];
    requires = [ "inetsim-network.service" ];
    path = [ pkgs.openssl ];
    wantedBy = [ ]; # manual start

    preStart = ''
      echo "~!: running systemd-unit as: $(whoami)"
      echo "~!: running systemd-unit as: $(whoami)"
      echo "~!: running systemd-unit as: $(whoami)"

      mkdir -p /var/log/inetsim/report

      if [ ! -d ${stateDir}/data ]; then
        cp -r --no-preserve=mode,ownership  \
          ${pkgs.inetsim}/share/inetsim/data ${stateDir}/data
      fi

      # chgrp -R inetsim ${stateDir}/data /var/log/inetsim
      # chmod -R g+rwX ${stateDir}/data
      # chmod -R 770 /var/log/inetsim
      # chmod -R g+rw /var/log/inetsim/* 2>/dev/null || true
    
    #  ---------------------------------
    #  I think we just let this die...
    #  ---------------------------------
    #   if [ ! -f ${stateDir}/data/certs/default_key.pem ]; then
    #     openssl req -new -x509 -days 3650 -nodes -sha256      \
    #         -keyout ${stateDir}/data/certs/default_key.pem    \
    #         -out    ${stateDir}/data/certs/default_cert.pem   \
    #         -subj "/O=INetSim/OU=Development/CN=inetsim.org"
    #
    #     chgrp inetsim ${stateDir}/data/certs/default_*.pem
    #     chmod 640     ${stateDir}/data/certs/default_key.pem
    #   fi
    # '';

    serviceConfig = {
      Type = "exec";
      RuntimeDirectory = "inetsim";
      ExecStart = pkgs.lib.concatStringsSep " " [
        "${pkgs.inetsim}/bin/inetsim"
        "--config /etc/inetsim/inetsim.conf"
        "--data-dir ${stateDir}/data"
        "--log-dir /var/log/inetsim"
        "--report-dir /var/log/inetsim/report"
      ];
      CapabilityBoundingSet = [
        "CAP_NET_BIND_SERVICE"
        "CAP_SETUID"
        "CAP_SETGID"
        "CAP_KILL"
        "CAP_CHOWN"
        "CAP_DAC_OVERRIDE"
      ];

      ReadWritePaths = [
        "/var/lib/inetsim"
        "/var/log/inetsim"
      ];
      Group = "inetsim";
      StateDirectory = "inetsim";
      StateDirectoryMode = "0770";
      LogsDirectory = "inetsim";
      LogsDirectoryMode = "0770";

      Restart = "no";
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;
      ProtectClock = true;
      ProtectHostname = true;
      ProtectProc = "invisible";
      ProcSubset = "pid";
      PrivateDevices = true;
      NoNewPrivileges = true;
      RestrictSUIDSGID = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      RemoveIPC = true;
      UMask = "0027";
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_UNIX"
      ];
      SystemCallArchitectures = "native";
      SystemCallFilter = [
        "@system-service"
        "@setuid"
        "~@resources"
      ];
    };

  };
}
