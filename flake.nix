{
  description = "flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-std.url = "github:chessai/nix-std";

    # not actually enabled yet
    vfio-hooks = {
      url = "github:PassthroughPOST/VFIO-Tools";
      flake = false;
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # violetshell = {
    #   url = "path:./modules/ags";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      nix-std,
      ...
    }@inputs:
    let
      std = nix-std.lib;

      user = {
        name = "please";
      };
      mkSystem =
        {
          hostname,
          system,
          user,
        }:
        inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            {
              networking.hostName = hostname;
            }
            ./modules/nixos/configuration.nix
            (./. + "/hosts/${hostname}/configuration.nix")
            (./. + "/hosts/${hostname}/hardware-configuration.nix")

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.please = ./home/violet.nix;

                extraSpecialArgs = {
                  # inherit outputs;
                  inherit inputs;
                  inherit system;
                  inherit user;
                  inherit std;
                };
              };
            }
          ];

          specialArgs = {
            # inherit outputs;
            inherit inputs;
            inherit system;
            inherit user;
            inherit std;
          };
        };
    in
    {

      nixosConfigurations = {
        violet = mkSystem {
          inherit user;
          hostname = "violet";
          system = "x86_64-linux";
        };
      };
    };
}
