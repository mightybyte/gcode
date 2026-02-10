{
  description = "G-code parsing, rendering, and construction library for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      haskellPackages = pkgs.haskell.packages.ghc910;
    in {
      devShells.default = haskellPackages.shellFor {
        packages = p: [];
        nativeBuildInputs = [
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.ghcid
          haskellPackages.hlint
          pkgs.pkg-config
          pkgs.zlib
        ];
      };
    });
}
