{ mkDerivation, ansi-terminal, base, bytestring, concurrent-extra
, configurator, containers, cryptohash, data-default-class, deepseq
, directory, dlist, docopt, download, filelock, filepath, Glob
, haskeline, hspec, logging, MissingH, mtl, parsec, path, path-io
, posix-escape, pretty, pretty-simple, process, progress-meter
, QuickCheck, random, random-shuffle, raw-strings-qq, regex-compat
, regex-posix, retry, safe-exceptions, scientific, setlocale, shake
, silently, split, stdenv, store, strict, tasty, tasty-golden
, tasty-hspec, tasty-hunit, tasty-quickcheck, temporary
, terminal-size, text, time, transformers, unbounded-delays, unix
, unordered-containers, utility-ht, zlib, graphviz, fgl
}:
mkDerivation {
  pname = "OrthoLang";
  version = "0.9.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class deepseq directory dlist
    docopt download filelock filepath Glob haskeline hspec logging
    MissingH mtl parsec path path-io posix-escape pretty pretty-simple
    process progress-meter QuickCheck random random-shuffle
    raw-strings-qq regex-compat regex-posix retry safe-exceptions
    scientific setlocale shake silently split store strict tasty
    tasty-golden tasty-hspec tasty-hunit tasty-quickcheck temporary
    terminal-size text time transformers unbounded-delays unix
    unordered-containers utility-ht graphviz fgl
  ];
  libraryPkgconfigDepends = [ zlib ];
  executableHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class deepseq directory dlist
    docopt download filelock filepath Glob haskeline hspec logging
    MissingH mtl parsec path path-io posix-escape pretty pretty-simple
    process progress-meter QuickCheck random random-shuffle
    raw-strings-qq regex-compat regex-posix retry safe-exceptions
    scientific setlocale shake silently split store strict tasty
    tasty-golden tasty-hspec tasty-hunit tasty-quickcheck temporary
    terminal-size text time transformers unbounded-delays unix
    unordered-containers utility-ht graphviz fgl
  ];
  executablePkgconfigDepends = [ zlib ];
  description = "Short, reproducible phylogenomic cuts";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;

  # set true for profiling (for example with +RTS -p -RTS):
  enableExecutableProfiling = false;
  enableLibraryProfiling = false;
}
