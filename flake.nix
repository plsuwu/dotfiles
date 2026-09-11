{
  description = "flake :)";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nur.url = "github:nix-community/NUR";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nh = {
      url = "github:nix-community/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    let
      templates = {
        rust = {
          path = ./templates/rust;
          description = "crane, rust-overlay";
        };

        c = {
          path = ./templates/c;
          description = "clang, gnumake";
        };
      };

      mkNixOS =
        {
          host,
          system,
          users,
        }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            {
              networking.hostName = host;
              nixpkgs.overlays = import ./overlays;
            }
            ./hosts/${host}
            ./modules/nixos
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; };
              home-manager.users = nixpkgs.lib.genAttrs users (username: {
                imports = [ ./users/${username}/home.nix ];
                _module.args.username = username;
              });
            }
          ]
          ++ map (username: {
            imports = [ ./users/${username}/nixos.nix ];
            _module.args.username = username;
            _module.args.hostname = host;
          }) users;
        };
    in
    {
      inherit templates;

      nixosConfigurations = {
        violet = mkNixOS {
          host = "violet";
          system = "x86_64-linux";
          users = [ "please" ];
        };
      };
    };
}
