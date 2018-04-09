{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers
      , ghcjs-base, hspec, hsx2hs, lens, mtl, stdenv, stm, text
      }:
      mkDerivation {
        pname = "chili";
        version = "0.1.2.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring containers ghcjs-base hsx2hs lens mtl stm
          text
        ];
        testHaskellDepends = [ base containers hspec hsx2hs text ];
        description = "yet another clientside ui library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
