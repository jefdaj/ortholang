{ mkDerivation, ansi-terminal, base, bytestring, concurrent-extra
, configurator, containers, cryptohash, data-default-class
, directory, dlist, docopt, download, filelock, filepath, Glob
, haskeline, hspec, logging, MissingH, mtl, parsec, path, path-io
, posix-escape, pretty, process, progress-meter, QuickCheck, random
, random-shuffle, raw-strings-qq, regex-compat, regex-posix, retry
, safe-exceptions, scientific, setlocale, shake, silently, split
, stdenv, strict, tasty, tasty-golden, tasty-hspec, tasty-hunit
, tasty-quickcheck, temporary, terminal-size, text, time
, transformers, unbounded-delays, unix, utility-ht, zlib, store
}:
mkDerivation {
  pname = "OrthoLang";
  version = "0.9.4";
  src = ./.;
  isLibrary = true; # TODO remove since only ortholang-demo uses the library?
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class directory dlist docopt
    download filelock filepath Glob haskeline hspec logging MissingH
    mtl parsec path path-io posix-escape pretty process progress-meter
    QuickCheck random random-shuffle raw-strings-qq regex-compat
    regex-posix retry safe-exceptions scientific setlocale shake
    silently split strict tasty tasty-golden tasty-hspec tasty-hunit
    tasty-quickcheck temporary terminal-size text time transformers
    unbounded-delays unix utility-ht store
  ];
  executablePkgconfigDepends = [ zlib ];
  description = "Short, reproducible phylogenomic cuts";
  license = stdenv.lib.licenses.agpl3;

  # uncomment for profiling (for example with +RTS -p -RTS):
  enableExecutableProfiling = true;
  enableLibraryProfiling = true;
}
