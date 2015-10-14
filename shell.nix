{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:
(import ./default.nix { inherit nixpkgs compiler; }).env
