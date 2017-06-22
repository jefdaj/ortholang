# Maintainers willing, everything here is destined to be merged into
# the main upstream repository at https://github.com/nixos/nixpkgs

let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs;

  # crb-blast only supports this version, and there are reports of a bug in newer ones (still? not sure)
  # TODO patch crb-blast to use the newest one
  ncbi-blast = (pkgs.callPackage ./ncbi-blast {}).overrideDerivation (old: rec {
    version="2.2.29";
    name="ncbi-blast-${version}";
    src = pkgs.fetchurl {
      url = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/${version}/ncbi-blast-${version}+-x64-linux.tar.gz";
      sha256="1pzy0ylkqlbj40mywz358ia0nq9niwqnmxxzrs1jak22zym9fgpm";
    };
  });

  last-align = pkgs.callPackage ./last-align {};
  shmlast    = pkgs.callPackage ./shmlast    { inherit last-align; };
  crb-blast  = pkgs.callPackage ./crb-blast  { inherit ncbi-blast; };

  # TODO remove this, as it's probably not useful to anyone
  # bblast = pkgs.callPackage ./bblast {
  #   inherit ncbi-blast;
  # inherit (pkgs) parallel pythonPackages;
  # };

  myPython = pkgs.pythonPackages // {
    biopython =  pkgs.callPackage ./biopython {
      inherit pkgs;
      inherit (pkgs) pythonPackages;
    };
  };

in nixpkgs // {
  inherit ncbi-blast last-align shmlast crb-blast;
  pythonPackages = myPython;
}
