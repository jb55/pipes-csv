{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:
let pkgs = nixpkgs.pkgs;
    overrideCabal = pkgs.haskell.lib.overrideCabal;
    cassavaOverride = super: {
      cassava = overrideCabal super.cassava (attrs: {
        version = "0.4.3.1";
        src = pkgs.fetchFromGitHub {
          owner = "jb55";
          repo = "cassava";
          rev = "2eb6e29bd5e141c1a9f0e980f7ac1c915e06e02a";
          sha256 = "1r1dv7yaalxja06jxqi7rjcdkb72mb2prnk8crzqap0gkmbahqcd";
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
