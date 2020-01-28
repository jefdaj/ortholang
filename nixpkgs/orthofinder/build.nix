with import ./..;
with pkgs;

let
  orthofinder = callPackage ./default.nix {
    inherit (lib) makeBinPath;
    inherit mcl fastme ncbi-blast diamond;
  };

in orthofinder
