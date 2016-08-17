{ mkDerivation, base, bytestring, Command, containers, cryptohash
, directory, enclosed-exceptions, filepath, haskeline, hspec
, MissingH, mtl, parsec, pretty, QuickCheck, scientific, shake
, stdenv, transformers
}:
mkDerivation {
  pname = "ShortCut";
  version = "0.7.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
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
  description = "A language for making short work of phylogenomic cuts";
  license = stdenv.lib.licenses.unfree;
}
