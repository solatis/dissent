# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, cabalInstall, cryptoApi, errors, HsOpenSSL, resourcet, transformers, transformersCompat, liftedBase, criterion, hspec, binary, either, mmorph
, network, async, QuickCheck, vector, random, mtl, process, regexCompat, hlint
}:

cabal.mkDerivation (self: {
  pname = "dissent";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    cryptoApi errors HsOpenSSL network async vector resourcet liftedBase transformers transformersCompat binary either mmorph mtl
  ];
  testDepends = [ cabalInstall criterion hspec network async QuickCheck vector resourcet transformers transformersCompat liftedBase random mtl process regexCompat hlint];
  meta = {
    description = "Accountable group anonymity protocol";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
