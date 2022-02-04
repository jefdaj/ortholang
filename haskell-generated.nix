# Old style:
# (import ./haskell.nix).ortholang

# New style leaves default.nix as the one to be imported from other Haskell projects,
# since this seems to be the least flexible. For other uses, we can customize.
# This was generated with cabal2nix ./., then customized a bit by hand.
# It will build the main interpreter, but without required non-Haskell dependencies.
# See haskell.nix for the version with all those added on.

{ mkDerivation, ansi-terminal, base, bytestring, concurrent-extra
, configurator, containers, cryptohash, data-default-class, deepseq
, directory, dlist, docopt, download, fgl, filelock, filepath, Glob
, graphviz, haskeline, hspec, logging, MissingH, mtl, parsec, path
, path-io, posix-escape, pretty, pretty-simple, process
, progress-meter, QuickCheck, random, random-shuffle
, raw-strings-qq, regex-compat, regex-posix, retry, safe-exceptions
, scientific, setlocale, shake, silently, split, stdenv, store
, strict, tasty, tasty-golden, tasty-hspec, tasty-hunit
, tasty-quickcheck, temporary, terminal-size, text, time
, transformers, unbounded-delays, unix, unordered-containers
, utility-ht, zlib
}:
mkDerivation {
  pname = "OrthoLang";
  version = "0.10.2";

  # surprisingly, this works as a drop-in replacement for filterSource
  # except with better filtering out of non-source files
  # based on https://github.com/NixOS/nix/issues/885#issuecomment-381904833
  # src = builtins.fetchGit { url = ./.; };
  src = ./.;

  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class deepseq directory dlist
    docopt download fgl filelock filepath Glob graphviz haskeline hspec
    logging MissingH mtl parsec path path-io posix-escape pretty
    pretty-simple process progress-meter QuickCheck random
    random-shuffle raw-strings-qq regex-compat regex-posix retry
    safe-exceptions scientific setlocale shake silently split store
    strict tasty tasty-golden tasty-hspec tasty-hunit tasty-quickcheck
    temporary terminal-size text time transformers unbounded-delays
    unix unordered-containers utility-ht
  ];
  libraryPkgconfigDepends = [ zlib ];
  executableHaskellDepends = [
    ansi-terminal base bytestring concurrent-extra configurator
    containers cryptohash data-default-class deepseq directory dlist
    docopt download fgl filelock filepath Glob graphviz haskeline hspec
    logging MissingH mtl parsec path path-io posix-escape pretty
    pretty-simple process progress-meter QuickCheck random
    random-shuffle raw-strings-qq regex-compat regex-posix retry
    safe-exceptions scientific setlocale shake silently split store
    strict tasty tasty-golden tasty-hspec tasty-hunit tasty-quickcheck
    temporary terminal-size text time transformers unbounded-delays
    unix unordered-containers utility-ht
  ];
  executablePkgconfigDepends = [ zlib ];
  description = "Short, reproducible phylogenomic cuts";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
