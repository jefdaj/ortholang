{ mkDerivation, base, bytestring, Command, concurrent-extra
, configurator, containers, cryptohash, data-default-class
, directory, docopt, enclosed-exceptions, exceptions, filelock
, filepath, Glob, haskeline, hspec, lock-file, MissingH, mtl
, parsec, path, path-io, posix-escape, pretty, process, QuickCheck
, regex-compat, retry, safe-exceptions, scientific, silently
, split, stdenv, strict, tasty, tasty-golden, tasty-hspec
, tasty-hunit, tasty-quickcheck, temporary, terminal-size, text
, time, transformers, unbounded-delays, unix, utility-ht
}:
mkDerivation {
  pname = "ShortCut";
  version = "0.7.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  # enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base bytestring Command concurrent-extra configurator containers
    cryptohash data-default-class directory docopt enclosed-exceptions
    exceptions filelock filepath Glob haskeline hspec lock-file
    MissingH mtl parsec path path-io posix-escape pretty process
    QuickCheck regex-compat retry safe-exceptions scientific
    silently split strict tasty tasty-golden tasty-hspec tasty-hunit
    tasty-quickcheck temporary terminal-size text time transformers
    unbounded-delays unix utility-ht
  ];
  description = "Short, reproducible phylogenomic cuts";
  license = stdenv.lib.licenses.agpl3;
}
