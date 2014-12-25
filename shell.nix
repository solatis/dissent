let pkgs = import <nixpkgs> {};
    myHaskellPackages = pkgs.haskellPackages;
    haskellPackages = myHaskellPackages.override {
      extension = self: super: {
        dissentCore = myHaskellPackages.callPackage ./. {};
      };
    };
in haskellPackages.dissentCore
