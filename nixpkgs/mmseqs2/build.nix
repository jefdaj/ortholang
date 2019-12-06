with import <nixpkgs> {};

# TODO can I enable AVX2 on the demo server, or is the CPU just too old??
# TODO replace perl with xxd?
# TODO why isn't the new version being used in sonicparanoid?

let p = import ./default.nix;

in callPackage p {}
