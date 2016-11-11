# TODO remove and replace with one shell per subdir?

with import ../nixpkgs;
let shortcut = haskellPackages.callPackage ./default.nix {};
in shortcut.env
