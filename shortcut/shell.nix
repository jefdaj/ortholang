{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, Command, containers
      , cryptohash, directory, enclosed-exceptions, filepath, haskeline
      , hspec, MissingH, mtl, parsec, pretty, QuickCheck, scientific
      , shake, stdenv, transformers
      }:
      mkDerivation {
        pname = "ShortCut";
        version = "0.7.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring Command containers cryptohash directory
          enclosed-exceptions filepath haskeline MissingH mtl parsec pretty
          scientific shake transformers
        ];
        executableHaskellDepends = [
          base bytestring Command containers cryptohash directory
          enclosed-exceptions filepath haskeline MissingH mtl parsec pretty
          scientific shake transformers
        ];
        testHaskellDepends = [
          base bytestring Command containers cryptohash directory
          enclosed-exceptions filepath haskeline hspec MissingH mtl parsec
          pretty QuickCheck scientific shake transformers
        ];
        description = "First draft of a ShortCut parser";
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
