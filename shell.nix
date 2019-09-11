{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs", doBenchmark ? false, nodejs ? nixpkgs."nodejs-10_x" }:

let

  inherit (nixpkgs) pkgs;
    nodeEnv = import ./tests/node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
    libtool = if pkgs.stdenv.isDarwin then pkgs.darwin.cctools else null;
  };

  np = import ./tests/node-packages.nix {
         inherit (pkgs) fetchurl fetchgit;
         inherit nodeEnv;
       };

  np2 = import ./tests/default.nix { pkgs = nixpkgs; };
  f = { mkDerivation, aeson, base, bytestring, containers
      , ghcjs-base, hspec, hsx2hs, lens, mtl, stdenv, stm, text, hedgehog, html-parse, haskell-src-meta
      }:
      mkDerivation {
        pname = "chili";
        version = "0.1.2.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring containers ghcjs-base hsx2hs lens mtl stm
          text pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc
          hedgehog nodejs html-parse haskell-src-meta
        ];
        testHaskellDepends = [ base containers hspec hsx2hs text ];
        description = "yet another clientside ui library";
        license = stdenv.lib.licenses.bsd3;
        inherit (np2.shell.nodeDependencies);
        shellHook = ''
             NODE_PATH=${np2.shell.nodeDependencies}/lib/node_modules
        '';
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
