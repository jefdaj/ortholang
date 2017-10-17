{ mkDerivation, base, bytestring, Command, configurator, containers
, cryptohash, directory, docopt, enclosed-exceptions, filepath
, Glob, haskeline, MissingH, mtl, parsec, pretty, process
, QuickCheck, regex-compat, scientific, shake, silently, stdenv
, tasty, tasty-golden, tasty-quickcheck, temporary, text
, transformers, strict, utility-ht, safe-exceptions, unix, lock-file
, data-default-class, tagged-exception-core, tasty-hunit
, path, path-io, tasty-hspec, hspec, unbounded-delays
}:
mkDerivation {
  pname = "ShortCut";
  version = "0.7.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring Command configurator containers cryptohash
    directory docopt enclosed-exceptions filepath Glob haskeline
    MissingH mtl parsec pretty process QuickCheck regex-compat
    scientific shake silently tasty tasty-golden tasty-quickcheck
    temporary text transformers strict utility-ht safe-exceptions
    unix lock-file data-default-class tagged-exception-core tasty-hunit
    path path-io tasty-hspec hspec unbounded-delays
  ];
  description = "A scripting language that makes short work of phylogenomic cuts";
  license = stdenv.lib.licenses.gpl3;
}
