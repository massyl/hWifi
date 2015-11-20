{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, HUnit, mtl, process, QuickCheck, stdenv
      , text
      }:
      mkDerivation {
        pname = "hWifi";
        version = "0.0.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base mtl process text ];
        executableHaskellDepends = [ base mtl process text ];
        testHaskellDepends = [ base HUnit QuickCheck text ];
        homepage = "https://github.com/lambdatree/hWifi.git";
        description = "Wifi connections manager in Haskell";
        license = stdenv.lib.licenses.gpl2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
