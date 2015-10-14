{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:
let pkgs = nixpkgs.pkgs;
    overrideCabal = pkgs.haskell.lib.overrideCabal;
    cassavaOverride = super: {
      cassava = overrideCabal super.cassava (attrs: {
        version = "0.4.3.1";
        src = pkgs.fetchFromGitHub {
          owner = "jb55";
          repo = "cassava";
          rev = "e3cbd777ccb7026c307b76107f2b00b1004d420a";
          sha256 = "0467h34ap07gh4yczk8fvghl2magbgdhibhmhccd6w3fl345wkq5";
        };
      });
    };
    baseHaskellPackages =
      if compiler != null
        then pkgs.haskell.packages.${compiler}
        else pkgs.haskellPackages;
    haskellPackages = baseHaskellPackages.override {
      overrides = self: super: cassavaOverride super;
    };
    callPackage = haskellPackages.callPackage;
in callPackage ./pipes-csv.nix { }
