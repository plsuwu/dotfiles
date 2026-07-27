{ ... }:
{
  imports = [
    ./virt

    ./boot.nix
    ./display.nix
    ./docker.nix
    ./greeter.nix
    ./files.nix
    ./network.nix
    ./nh.nix
    ./nix-ld.nix
    ./security.nix
    ./sound.nix
    ./sshd.nix
  ];
}
