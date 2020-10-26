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

(import ./haskell.nix).shell
