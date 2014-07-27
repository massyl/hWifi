{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc763
, networkmanager ? pkgs.networkmanager # nmcli used by hWifi
}:

let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_3 HUnit QuickCheck mtl; # Haskell dependencies here

in cabal.mkDerivation (self: {
  pname = "hWifi";
  version = "0.0.0.1";
  sha256 = "dummy-sha1-for-the-moment";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildTools = [ cabalInstall_1_18_0_3 networkmanager ];
  buildDepends = with haskellPackages; [ mtl ];
  testDepends = with haskellPackages; [ HUnit QuickCheck ];
  meta = {
    homepage = "https://github.com/lambdatree/hWifi.git";
    description = "Wifi connections manager in Haskell";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
