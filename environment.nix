# This is a list of packages needed by the OrthoLang interpreter at runtime,
# and also by some/all of the modules.

# TODO put this in release.nix instead?

# TODO can a lot/all of this be removed?

with import ./nix;

# TODO should this go in a separate file with the haskell binary definition?
[
  # TODO which of these are needed?
  stdenv
  bash
  bashInteractive
  coreutils
  diffutils
  glibcLocales # TODO even on mac?
  tree
  # tarWithGzip # TODO is this needed at runtime?
  gnugrep
  gnused
  gawk
  curl
  cacert # for curl https
  fontconfig.lib # for R plotting scripts
  graphviz
  rsync
  file

  # TODO is this a good idea?
  # myR # TODO is this needed at runtime?
  gettext # for envsubst in custom scripts

] ++ pkgs.lib.optionals (stdenv.isLinux) [

  # works, but remove for release
  # fsatrace # for Shake linting. still not available on Mac?

]
