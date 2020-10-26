# TODO remove in favor of nix-shell release.nix or modules.nix?

# This file defines the default shell for working on the
# OrthoLang interpreter (the main Haskell program).
#
# But you can also use it to run a local Hoogle instance:
# nix-shell --run hoogle server --port=8080 --local --haskell
#
# You can also get a shell for working on a specific OrthoLang
# module rather than for the interpreter, like this:
#
# nix-shell modules.nix -A ortholang-seqio

let
  sources = import ./nix/sources.nix {};
  pkgs    = import sources.nixpkgs {};
  myHs    = import ./haskell.nix;
  release = import ./release.nix;

in myHs.shellFor {

  # TODO would there be any reason to add other packages here?
  packages = p: with p; [ release.project ];

  # Put any packages you want during development here.
  # You can optionally go "full reproducible" by adding your text editor
  # and using `nix-shell --pure`, but you'll also have to add some common
  # unix tools as you go.
  buildInputs = with myHs; [
    ghcid
    hlint
    stack
  ];

  # Run a local Hoogle instance like this:
  # nix-shell --run hoogle server --port=8080 --local --haskell
  withHoogle = true;
}
