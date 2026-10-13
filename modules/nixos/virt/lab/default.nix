{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.malwareLab;
  pyEnv = pkgs.python3.withPackages (ps: [
    ps.mcp
    ps.libvirt
    ps.uvicorn
    ps.pyzipper
  ]);
  serverSrc = pkgs.runCommand "malware-lab-mcp-src" { } ''
    mkdir -p $out
    cp ${./server/lab_mcp.py} $out/lab_mcp.py
    cp ${./server/ida_harness.py} $out/ida_harness.py
  '';

  serverConfig = pkgs.writeText "lab.json" (
    builtins.toJSON {
      libvirt_uri = "qemu:///system";
      domain = cfg.domain;
      clean_snapshot = cfg.cleanSnapshot;
      state_dir = cfg.stateDir;
      guest_workdir = cfg.guestWorkdir;
      idat_path = cfg.idatPath;
      guest_user = cfg.guestUser;
      ida_as_user = cfg.idaAsUser;
      tools = cfg.toolPaths;
      ida_harness = "${serverSrc}/ida_harness.py";
    }
  );

  ingestCmd = "${pyEnv}/bin/python ${serverSrc}/lab_mcp.py --config ${serverConfig} ingest -";
  ingest = pkgs.writeShellScriptBin "lab-ingest" ''
    set -eu 
    [ $# -eq 1 ] || { echo "usage: lab-ingest FILE" >&2; exit 2; }
    exec /run/wrappers/bin/sudo -u lab-user ${ingestCmd} \
      --name "$(basename -- "$1" | tr -c 'A-Za-z0-9._-' _)" < "$1"
  '';
  bridge = "virbr-mal";

  isolatedNet = pkgs.writeText "malnet.xml" ''
    <network>
      <name>malnet</name>
      <bridge name="${bridge}" stp="on" delay="0"/>
    </network>
  '';

  portSet =
    ports:
    if ports == [ ] then "0" else lib.concatMapStringsSep ", " toString ports;
in
{
  options.services.malwareLab = {
    enable = lib.mkEnableOption "libvirt LLM analysis lab + MCP server";
    agentUser = lib.mkOption {
      type = lib.types.str;
      description = ''
        LLM account
      '';
    };

    domain = lib.mkOption {
      type = lib.types.str;
      default = "windows-11_for-LLM";
    };

    cleanSnapshot = lib.mkOption {
      type = lib.types.str;
      default = "clean";
    };

    stateDir = lib.mkOption {
      type = lib.types.str;
      default = "/data/virtual-machine/lab-workdir";
    };

    guestWorkdir = lib.mkOption {
      type = lib.types.str;
      default = ''C:\analysis'';
    };

    toolPaths = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = {
        floss = ''C:\ProgramData\chocolatey\bin\floss.exe'';
      };
      description = "Override guest tool names/paths: floss, mal_unpack, pe_sieve, autorunsc.";
    };

    idatPath = lib.mkOption {
      type = lib.types.str;
      default = ''C:\Program Files\IDA Professional 9.4\idat.exe'';
    };

    guestUser = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "Peas";
      description = ''
        Windows account to run guest_exec and IDA as, via a scheduled task in its
        logged-on session, so its PATH, %APPDATA%, HKCU and licences apply. The
        account must be logged on in the clean snapshot. null = run as SYSTEM.
      '';
    };

    idaAsUser = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "With guestUser set, also run IDA as that user (needed for a per-user IDA licence).";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 7765; # outside the 8000-10000 range opened on virbr0 elsewhere
    };

    ingestUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "user accounts allowed to run `lab-ingest` (sudo to the service user)";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !(lib.hasInfix " " cfg.guestWorkdir);
        message = "services.malwareLab.guestWorkdir must not contain spaces";
      }
      {
        assertion = config.virtualisation.libvirtd.enable;
        message = "services.malwareLab needs virtualisation.libvirtd.enable";
      }
      {
        assertion = config.networking.nftables.enable;
        message = "services.malwareLab's network fence uses nftables (networking.nftables.enable)";
      }
      {
        assertion =
          !(lib.elem "libvirtd" (config.users.users.${cfg.agentUser}.extraGroups or [ ]));
        message = "services.malwareLab.agentUser must not be in libvirtd; that would bypass the MCP boundary";
      }
    ];

    # systemd.services.lab-net = {
    #   description = "Define isolated libvirt network for the malware lab";
    #   after = [ "libvirtd.service" ];
    #   requires = [ "libvirtd.service" ];
    #   wantedBy = [ "multi-user.target" ];
    #   serviceConfig.Type = "oneshot";
    #   path = [ pkgs.libvirt ];
    #
    #   script = ''
    #     virsh -c qemu:///system net-define ${isolatedNet}
    #     virsh -c qemu:///system net-autostart malnet
    #     virsh -c qemu:///system net-start malnet 2>/dev/null || true
    #   '';
    # };

    networking.nftables.tables.malware-lab = {
      family = "inet";
      content = ''
        chain prerouting {
          type nat hook prerouting priority dstnat - 10; policy accept;
        }

        chain input {
          type filter hook input priority filter - 10; policy accept;
          iifname "${bridge}" drop
        }

        chain forward {
          type filter hook forward priority filter - 10; policy accept;
          iifname "${bridge}" oifname != "${bridge}" drop
          oifname "${bridge}" iifname != "${bridge}" drop
        }

        chain output {
          type filter hook output priority filter - 10; policy accept;
          # The host only ever answers on the bridge; it never initiates.
          oifname "${bridge}" ct state new drop
        }
      '';
    };

    security.polkit.extraConfig = ''
      // lab-mcp: libvirt management for the MCP service account
      polkit.addRule(function(action, subject) {
        if (action.id == "org.libvirt.unix.manage" && subject.user == "lab-user") {
          return polkit.Result.YES;
        }
      });
    '';
    systemd.tmpfiles.rules = [
      "d ${cfg.stateDir} 0700 lab-user lab-clients -"
      "d ${cfg.stateDir}/samples 0700 lab-user lab-clients -"
    ];

    users.groups.lab-clients = { };
    users.groups.lab-user = { };
    users.users.lab-user = {
      isSystemUser = true;
      group = "lab-user";
      extraGroups = [
        "libvirtd"
        "lab-clients"
      ];
      home = cfg.stateDir;
    };
    users.users.${cfg.agentUser}.extraGroups = [ "lab-clients" ];
    security.sudo.extraRules = [
      {
        users = cfg.ingestUsers;
        runAs = "lab-user";
        commands = [
          {
            command = "${ingestCmd} --name *";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];

    environment.systemPackages = [ ingest ];

    # ---- MCP server ------------------------------------------------------------

    systemd.services.lab-mcp = {
      description = "MCP control channel for the malware-analysis VM";
      after = [
        "libvirtd.service"
        "network.target"
      ];
      requires = [ "libvirtd.service" ];
      wantedBy = [ "multi-user.target" ];

      preStart = ''
        umask 027
        if [ ! -s /run/malware-lab/token ]; then
          ${pkgs.coreutils}/bin/head -c 32 /dev/urandom | ${pkgs.coreutils}/bin/base64 -w0 | tr '+/' '-_' > /run/malware-lab/token
        fi
        chmod 0640 /run/malware-lab/token
        mkdir -p ${cfg.stateDir}/samples
      '';

      serviceConfig = {
        User = "lab-user";
        # Group owns /run/malware-lab, so the agent user can read the token.
        Group = "lab-clients";
        ExecStart = lib.concatStringsSep " " [
          "${pyEnv}/bin/python ${serverSrc}/lab_mcp.py"
          "--config ${serverConfig}"
          "serve --transport http --host 127.0.0.1 --port ${toString cfg.port}"
          "--token-file /run/malware-lab/token"
        ];
        Restart = "on-failure";

        StateDirectory = "malware-lab";
        StateDirectoryMode = "0700";
        RuntimeDirectory = "malware-lab";
        RuntimeDirectoryMode = "0750";
        RuntimeDirectoryPreserve = "yes"; # keep the token across restarts
        UMask = "0077";

        # Sandbox the server itself: it only needs libvirt's unix socket and loopback.
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        CapabilityBoundingSet = "";
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        IPAddressDeny = "any";
        IPAddressAllow = "localhost";
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
        ];
        # libvirt client socket lives here:
        ReadWritePaths = [
          "/run/libvirt"
          cfg.stateDir
        ];
      };
    };
  };
}
