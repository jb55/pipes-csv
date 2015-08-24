{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
