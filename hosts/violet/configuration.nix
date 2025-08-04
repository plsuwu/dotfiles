{
  # inputs,
  config,
  pkgs,
  user,
  ...
}:
{

  config.modules = {
    boot.enable = true;
    virt.enable = true;
    docker.enable = true;
    desktop-manager.enable = true;
    sound.enable = true;
    tz.enable = true;
    users.enable = true;
    thunar.enable = true;
    net.enable = true;
    auth.enable = true;
  };

  config = {
    programs.zsh.enable = true;
  };

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
}
