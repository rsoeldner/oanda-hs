self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = nself: nsuper: {
      oanda = nself.callPackage ./default.nix {};
    };
  };
}
