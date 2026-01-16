{
  description = "UDP chat server - static build configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        # Use musl-based static package set for fully static binaries
        pkgsStatic = pkgs.pkgsStatic;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            stack
            zlib
            gmp
            libffi
          ];
        };

        # Static build shell with musl libc
        devShells.static = pkgsStatic.mkShell {
          buildInputs = with pkgsStatic; [
            stack
            zlib
            gmp
            libffi
            musl
            binutils
            file
          ];
          # Ensure static libraries are used
          LD_LIBRARY_PATH = "${pkgsStatic.zlib}/lib:${pkgsStatic.gmp}/lib:${pkgsStatic.libffi}/lib";
        };
      });
}
