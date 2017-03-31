# Maintainers willing, everything here is destined to be merged into
# the main upstream repository at https://github.com/nixos/nixpkgs

let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs;

  ncbi-blast = pkgs.callPackage ./ncbi-blast {};

  # TODO remove this, as it's probably not useful to anyone
  bblast = pkgs.callPackage ./bblast {
    inherit ncbi-blast;
    inherit (pkgs) parallel pythonPackages;
  };

  myPython = pkgs.pythonPackages // {
    biopython =  pkgs.callPackage ./biopython {
      inherit pkgs;
      inherit (pkgs) pythonPackages;
    };
  };

in nixpkgs // {
  inherit ncbi-blast bblast;
  pythonPackages = myPython;
}
