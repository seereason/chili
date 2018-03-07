{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers, ghcjs-base, lens, mtl
      , stdenv, text, hsx2hs, stm, hspec
      }:
      mkDerivation {
        pname = "chili";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = false;
        libraryHaskellDepends = [
          aeson base bytestring containers ghcjs-base lens mtl text hsx2hs stm hspec pkgs.haskellPackages.cabal-install
        ]; 
        buildTools = [ pkgs.haskellPackages.ghc  ];
        description = "yet another clientside ui library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
