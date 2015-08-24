{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./pipes-csv.nix { }
