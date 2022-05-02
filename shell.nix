{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers
      , ghcjs-base, haskell-src-meta, hspec, hsx2hs, html-parse, lens
      , mtl, stdenv, stm, template-haskell, text
      }:
      mkDerivation {
        pname = "chili";
        version = "0.2.8";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring containers ghcjs-base haskell-src-meta hsx2hs
          html-parse lens mtl stm template-haskell text
        ];
        testHaskellDepends = [ base containers hspec hsx2hs text ];
        testToolDepends = [ hsx2hs pkgs.haskell.packages.ghc865.cabal-install ];
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
