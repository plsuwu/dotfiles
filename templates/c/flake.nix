{
  description = "C template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cmake
          gnumake
          pkg-config

          gcc
          clang
          bear

          ccls
          gdb
          valgrind
        ];

        shellHook = ''
          # generate JSON compilation database for `ccls` if required
          if [ ! -f compile_commands.json ]; then
            bear -- make all clean 2>/dev/null || true
          fi

          alias la="ls -la"
          gcc --version
        '';
      };

      # packages.${system}.default = pkgs.stdenv.mkDerivation {
      #   pname = "cpp-project";
      #   version = "0.1.0";
      #   src = ./.;
      #
      #   nativeBuildInputs = [ pkgs.gcc ];
      # };
    };
}
