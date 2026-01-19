{
  description = "UDP chat server - static build configuration with haskell.nix";

  # Configure binary caches for haskell.nix and Garnix
  # This prevents rebuilding GHC from source and enables cache pulls
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.garnix.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Regular nixpkgs with haskell.nix overlay
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay ];
          inherit (haskellNix) config;
        };

        # Musl-based nixpkgs for static builds
        muslPkgs = pkgs.pkgsMusl;

        # Regular project (for development)
        regularProject = pkgs.haskell-nix.project {
          src = pkgs.haskell-nix.haskellLib.cleanGit {
            name = "gimmel";
            src = ./.;
          };
          # Use stack project
          projectFileName = "stack.yaml";
          # LTS-22.44 uses GHC 9.6.7
          compiler-nix-name = "ghc967";
        };

        # Static musl project
        staticProject = muslPkgs.haskell-nix.project {
          src = muslPkgs.haskell-nix.haskellLib.cleanGit {
            name = "gimmel";
            src = ./.;
          };
          projectFileName = "stack.yaml";
          compiler-nix-name = "ghc967";
          # Configure for static linking
          modules = [
            {
              # Static linking flags
              packages.gimmel.components.exes.gimmel-exe.configureFlags = [
                "--ghc-option=-optl=-static"
                "--ghc-option=-optl=-pthread"
                "--ghc-option=-fPIC"
              ];
              packages.gimmel.components.exes."test-net".configureFlags = [
                "--ghc-option=-optl=-static"
                "--ghc-option=-optl=-pthread"
                "--ghc-option=-fPIC"
              ];
            }
          ];
        };

        # Get flake outputs
        regularFlake = regularProject.flake {};
        staticFlake = staticProject.flake {};
      in
      {
        # Regular development shell
        devShells.default = regularProject.shellFor {
          nativeBuildInputs = with pkgs; [
            stack
            zlib
            gmp
            libffi
            ncurses
          ];
        };

        # Static build shell with musl
        devShells.static = staticProject.shellFor {
          nativeBuildInputs = with pkgs; [
            stack
            zlib
            gmp
            libffi
            ncurses
            binutils
            file
            libiconv
            numactl
          ];
        };

        # Packages for building
        packages.default = regularFlake.packages."gimmel:exe:gimmel-exe";
        packages."gimmel-exe" = regularFlake.packages."gimmel:exe:gimmel-exe";
        packages."test-net" = regularFlake.packages."gimmel:exe:test-net";

        # Static packages
        packages.static = staticFlake.packages."gimmel:exe:gimmel-exe";
        packages."static-gimmel-exe" = staticFlake.packages."gimmel:exe:gimmel-exe";
        packages."static-test-net" = staticFlake.packages."gimmel:exe:test-net";
      });
}
