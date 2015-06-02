{ pkgs ? (import /home/anders/devel/nixpkgs/default.nix {}) }:

(import ./default.nix) {
  stdenv          = pkgs.stdenv;
  haskellPackages = pkgs.haskell.packages.ghcjs;
  nodejs          = pkgs.nodejs;
}
