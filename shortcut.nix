{ mkDerivation, base, bytestring, Command, configurator, containers
, cryptohash, directory, docopt, enclosed-exceptions, filepath
, haskeline, hspec, MissingH, mtl, parsec, pretty, QuickCheck
, scientific, shake, stdenv, text, transformers
}:
mkDerivation {
  pname = "ShortCut";
  version = "0.7.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring Command configurator containers cryptohash
    directory docopt enclosed-exceptions filepath haskeline hspec
    MissingH mtl parsec pretty QuickCheck scientific shake text
    transformers
  ];
  description = "A scripting language that makes short work of phylogenomic cuts";
  license = stdenv.lib.licenses.gpl3;
}
