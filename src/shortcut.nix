{ mkDerivation, base, bytestring, Command, configurator, containers
, cryptohash, directory, docopt, enclosed-exceptions, filepath
, Glob, haskeline, MissingH, mtl, parsec, pretty, process
, QuickCheck, regex-compat, scientific, shake, silently, stdenv
, tasty, tasty-golden, tasty-quickcheck, temporary, text
, transformers, strict, utility-ht, safe-exceptions, unix, lock-file
, data-default-class, tagged-exception-core, tasty-hunit
, path, path-io, tasty-hspec, hspec, unbounded-delays, split
, filelock, exceptions, time, concurrent-extra, retry, terminal-size
, posix-escape
}:
mkDerivation {
  pname = "ShortCut";
  description = "Short, reproducible phylogenomic cuts";
  version = "0.7.0.0";
  license = stdenv.lib.licenses.gpl3;
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  executableHaskellDepends = [
    base bytestring Command configurator containers cryptohash
    directory docopt enclosed-exceptions filepath Glob haskeline
    MissingH mtl parsec pretty process QuickCheck regex-compat
    scientific shake silently tasty tasty-golden tasty-quickcheck
    temporary text transformers strict utility-ht safe-exceptions
    unix lock-file data-default-class tagged-exception-core tasty-hunit
    path path-io tasty-hspec hspec unbounded-delays split filelock
    exceptions time concurrent-extra retry terminal-size posix-escape
  ];
}
