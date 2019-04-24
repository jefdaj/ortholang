{ mkDerivation, ansi-terminal, base, bytestring, concurrent-extra
, configurator, containers, cryptohash, data-default-class
, directory, dlist, docopt, filelock, filepath, Glob, haskeline
, hspec, MissingH, mtl, parsec, path, path-io, posix-escape, pretty
, process, QuickCheck, random, random-shuffle, regex-compat
, regex-posix, retry, safe-exceptions, scientific, setlocale, shake
, silently, split, stdenv, strict, tasty, tasty-golden, tasty-hspec
, tasty-hunit, tasty-quickcheck, temporary, terminal-size, text
, time, transformers, unbounded-delays, unix, utility-ht
}:
mkDerivation {
  pname = "ShortCut";
  version = "0.8.4.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class directory dlist docopt
    filelock filepath Glob haskeline hspec MissingH mtl parsec path
    path-io posix-escape pretty process QuickCheck random
    random-shuffle regex-compat regex-posix retry safe-exceptions
    scientific setlocale shake silently split strict tasty tasty-golden
    tasty-hspec tasty-hunit tasty-quickcheck temporary terminal-size
    text time transformers unbounded-delays unix utility-ht
  ];
  description = "Short, reproducible phylogenomic cuts";
  license = stdenv.lib.licenses.agpl3;
}
