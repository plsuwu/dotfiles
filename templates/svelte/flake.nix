{
  description = "svelte project (using bun2nix)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    bun2nix.url = "github:nix-community/bun2nix";
  };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      bun2nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

      in
      {
        packages = {
          # Produce a package for this template with bun2nix in the overlay
          default = pkgs.callPackage ./default.nix { };
        };
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.bun

            # Add the bun2nix binary to our devshell
            # Optional now that we have a binary on npm
            bun2nix.packages.${system}.default
          ];

          shellHook = ''
            bun install --frozen-lockfile
          '';
        };
      }
    );
}
