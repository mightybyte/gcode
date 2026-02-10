{
  description = "G-code parsing, rendering, and construction library for Haskell";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          gcode-project =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc982";
              shell.tools = {
                cabal = {};
                ghcid = {};
                hlint = {};
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.gcode-project.flake {};
    in flake // {
      packages.default = flake.packages."gcode:lib:gcode";
      inherit pkgs;
    });
}
