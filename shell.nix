with import ./nixpkgs;

let
  scripts  = import ./scripts;
  shortcut = haskellPackages.callPackage ./shortcut.nix {};

in stdenv.lib.overrideDerivation shortcut (attrs: {
  buildInputs = attrs.buildInputs ++ [ bblast scripts ];
})
