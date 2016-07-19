with import <mypkgs>;
let shortcut = haskellPackages.callPackage ./shortcut.nix {};
in shortcut.env
