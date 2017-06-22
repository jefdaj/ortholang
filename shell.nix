with import ./nixpkgs;

let
  scripts  = import ./scripts;
  shortcut = haskellPackages.callPackage ./shortcut.nix {};

# TODO unify with addRuntimeDependencies?
in stdenv.lib.overrideDerivation shortcut (attrs: {
  buildInputs = attrs.buildInputs ++ [ bblast scripts crb-blast ];
})
