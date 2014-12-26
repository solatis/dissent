# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, cabalInstall, errors, hslogger, HsOpenSSL, hspec, network, networkSimple
, vector
}:

cabal.mkDerivation (self: {
  pname = "dissent";
  version = "0.0.1";
  src = ./.;
  buildDepends = [ errors hslogger HsOpenSSL networkSimple vector network ];
  testDepends = [ cabalInstall hspec vector ];
  meta = {
    description = "Accountable group anonymity protocol";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
