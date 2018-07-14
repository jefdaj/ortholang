{ mkDerivation, base, bytestring, Command, concurrent-extra
, configurator, containers, cryptohash, data-default-class
, directory, docopt, enclosed-exceptions, exceptions, filelock
, filepath, Glob, haskeline, hspec, MissingH, mtl, parsec, path
, path-io, posix-escape, pretty, process, QuickCheck, random
, random-shuffle, regex-compat, retry, safe-exceptions, scientific
, shake, silently, split, stdenv, strict, tasty, tasty-golden
, tasty-hspec, tasty-hunit, tasty-quickcheck, temporary
, terminal-size, text, time, transformers, unbounded-delays, unix
, utility-ht
}:
mkDerivation {
  pname = "ShortCut";
  version = "0.8.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base bytestring Command concurrent-extra configurator containers
    cryptohash data-default-class directory docopt enclosed-exceptions
    exceptions filelock filepath Glob haskeline hspec MissingH mtl
    parsec path path-io posix-escape pretty process QuickCheck random
    random-shuffle regex-compat retry safe-exceptions scientific shake
    silently split strict tasty tasty-golden tasty-hspec tasty-hunit
    tasty-quickcheck temporary terminal-size text time transformers
    unbounded-delays unix utility-ht
  ];
  description = "Short, reproducible phylogenomic cuts";
  license = stdenv.lib.licenses.agpl3;
}
