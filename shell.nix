{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, containers, ghcjs-base, lens, machines, mtl
      , stdenv, text, hsx2hs, stm
      }:
      mkDerivation {
        pname = "chili";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base containers ghcjs-base lens machines mtl text hsx2hs stm
        ];
        buildTools = [ pkgs.haskellPackages.cabal-install ];
        description = "yet another clientside ui library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
