{ mkDerivation, ansi-terminal, base, bytestring, concurrent-extra
, configurator, containers, cryptohash, data-default-class
, directory, dlist, docopt, download, filelock, filepath, Glob
, haskeline, hspec, logging, MissingH, mtl, parsec, path, path-io
, posix-escape, pretty, process, progress-meter, QuickCheck, random
, random-shuffle, raw-strings-qq, regex-compat, regex-posix, retry
, safe-exceptions, scientific, setlocale, shake, silently, split
, stdenv, strict, tasty, tasty-golden, tasty-hspec, tasty-hunit
, tasty-quickcheck, temporary, terminal-size, text, time
, transformers, unbounded-delays, unix, utility-ht, zlib
}:
mkDerivation {
  pname = "ShortCut";
  version = "0.8.4.11";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class directory dlist docopt
    download filelock filepath Glob haskeline hspec logging MissingH
    mtl parsec path path-io posix-escape pretty process progress-meter
    QuickCheck random random-shuffle raw-strings-qq regex-compat
    regex-posix retry safe-exceptions scientific setlocale shake
    silently split strict tasty tasty-golden tasty-hspec tasty-hunit
    tasty-quickcheck temporary terminal-size text time transformers
    unbounded-delays unix utility-ht
  ];
  libraryPkgconfigDepends = [ zlib ];
  executableHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class directory dlist docopt
    download filelock filepath Glob haskeline hspec logging MissingH
    mtl parsec path path-io posix-escape pretty process progress-meter
    QuickCheck random random-shuffle raw-strings-qq regex-compat
    regex-posix retry safe-exceptions scientific setlocale shake
    silently split strict tasty tasty-golden tasty-hspec tasty-hunit
    tasty-quickcheck temporary terminal-size text time transformers
    unbounded-delays unix utility-ht
  ];
  executablePkgconfigDepends = [ zlib ];
  description = "Short, reproducible phylogenomic cuts";
  license = stdenv.lib.licenses.agpl3;
}
