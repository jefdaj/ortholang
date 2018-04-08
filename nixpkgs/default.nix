# Maintainers willing, everything here is destined to be merged into
# the main upstream repository at https://github.com/nixos/nixpkgs

let
  nixpkgs = import <nixpkgs> {};
  inherit (nixpkgs) pkgs;

  # this is used in place of ncbi-blast everywhere except in crb-blast...
  psiblast-exb = pkgs.callPackage ./psiblast-exb { };

  # ... because it only supports exactly 2.2.29
  # and there are reports of a bug in newer ones (still?)
  # TODO patch crb-blast to use the newest one?
  ncbi-blast = (pkgs.callPackage ./ncbi-blast {}).overrideDerivation (old: rec {
    version="2.2.29";
    name="ncbi-blast-${version}";
    src = pkgs.fetchurl {
      url = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/${version}/ncbi-blast-${version}+-x64-linux.tar.gz";
      sha256="1pzy0ylkqlbj40mywz358ia0nq9niwqnmxxzrs1jak22zym9fgpm";
    };
  });
  crb-blast  = pkgs.callPackage ./crb-blast  { inherit ncbi-blast; };

  myPython = pkgs.pythonPackages // {
    blastdbget = pkgs.callPackage ./blastdbget {};
    biopython  = pkgs.callPackage ./biopython {
      inherit pkgs;
      inherit (pkgs) pythonPackages;
    };
  };

in nixpkgs // {
  inherit crb-blast psiblast-exb;
  pythonPackages = myPython;
}
