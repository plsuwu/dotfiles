{
  description = "nixos & home-manager config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    sops-nix = {
        url = "github:Mic92/sops-nix";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      inherit (self) outputs;

      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

    in {
    modules = import ./modules;

    nixosConfigurations = {

      ruby = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs outputs; };
        modules = [ ./hosts/ruby/configuration.nix ];
      };

    };

      homeConfigurations = {

	"please@ruby" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./home/please/ruby.nix ];
          extraSpecialArgs = { inherit inputs outputs; };
        };

      };
    };
}
