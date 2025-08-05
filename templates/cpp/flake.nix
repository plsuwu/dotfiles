{
  # this is basically just the C template but with a slightly
  # different makefile ://

  description = "C++ template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # cmake
            gnumake
            pkg-config

            clang
            gcc

            bear
            ccls

            gdb
            gef
            valgrind
          ];

          shellHook = ''
            # generate JSON compilation database for `ccls` if required
            if [ ! -f compile_commands.json ]; then
              bear -- make all clean 2>/dev/null || true
            fi

            alias la="ls -la"
            alias gdb="gef"

            gcc --version
            gdb --version
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "cpp-template";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            gnumake
          ];

          buildPhase = ''
            make release
          '';
        };
      }
    );
}

