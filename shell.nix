with import ./nixpkgs;

let
  scripts  = import ./scripts;
  shortcut = haskellPackages.callPackage ./shortcut.nix {};

# TODO import from default.nix here instead to unify with the runtime dependencies thing?
# ah, adding ncbi-blast here picks up the correct version for crb-blast!
in stdenv.lib.overrideDerivation shortcut (attrs: {
  buildInputs = attrs.buildInputs ++ [ scripts ncbi-blast crb-blast ];
})
