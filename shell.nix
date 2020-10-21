# This is for working on the OrthoLang interpreter.
# To work on module scripts, see module-shell.nix
#   
# You can also use it to run a local Hoogle instance:
# nix-shell --run hoogle server --port=8080 --local --haskell

(import ./ortholang.nix).shell
