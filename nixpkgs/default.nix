# Maintainers willing, everything here is destined to be merged into
# the main upstream repository at https://github.com/nixos/nixpkgs
# as soon as I have the time

let
  # fetch my pinned nixpkgs for reproducibility.
  # use this instead to try to build it with your system's current nixpkgs:
  # pkgs = import <nixpkgs> {};
  # to update the the sha256sum, use nix-prefetch-url --unpack
  # (see https://github.com/NixOS/nix/issues/1381#issuecomment-300755992)
  pkgs = import (fetchTarball {
    url = "https://github.com/jefdaj/nixpkgs/archive/2019-03-20_nixpkgs-shortcut.tar.gz";
    sha256 = "1lj3paw9z0n8v1dk8nxmnd7i0z209746cyz19vsadkswd87x7ipm";
  }) {};

  psiblast-exb = pkgs.callPackage ./psiblast-exb { };

  hmmer = pkgs.callPackage ./hmmer { };

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

  # cdhit = pkgs.callPackage ./cdhit { };

  mcl = pkgs.callPackage ./mcl { };

  fastme = pkgs.callPackage ./fastme { };

  # muscle = pkgs.callPackage ./muscle { }; # TODO got this already somewhere!

  diamond = pkgs.callPackage ./diamond { };

  mmseqs2 = pkgs.callPackage ./mmseqs2 { };

  orthofinder = pkgs.callPackage ./orthofinder {
    inherit mcl fastme psiblast-exb diamond;
  };

  # TODO push new sh-1.12.14 upstream! haven't managed to include it properly here
  # TODO is this pulling in python3 and messing up treeCl?
  sonicparanoid = pkgs.callPackage ./sonicparanoid {
    inherit mmseqs2;
    inherit (pkgs) python36Packages;
  };

  # TODO detect whether MPI version will work on a given computer and adjust
  raxml  = pkgs.callPackage ./raxml { mpi = true; };

  # TODO should treeCl go inside the python packages instead of the other way around?
  myPython2 = pkgs.python27Packages // rec {
    fastcluster        = pkgs.python27Packages.callPackage ./fastcluster {};
    fasttree           = pkgs.python27Packages.callPackage ./fasttree {};
    tree_distance      = pkgs.python27Packages.callPackage ./tree_distance {};
    progressbar-latest = pkgs.python27Packages.callPackage ./progressbar-latest {};
    CacheControl       = pkgs.python27Packages.callPackage ./CacheControl {};
    scikit-bio         = pkgs.python27Packages.callPackage ./scikit-bio { inherit CacheControl; };
    phylo_utils        = pkgs.python27Packages.callPackage ./phylo_utils {};
    blastdbget         = pkgs.python27Packages.callPackage ./blastdbget {};
    treeCl = pkgs.python27Packages.callPackage ./treeCl {
      inherit raxml; # TODO why doesn't it find this?
      inherit fastcluster fasttree tree_distance progressbar-latest CacheControl scikit-bio phylo_utils;
      inherit (pkgs.python27Packages) pyyaml biopython cython dendropy futures ipython;
      inherit (pkgs.python27Packages) matplotlib nose numpy pandas progressbar scikitlearn scipy;
    };
  };
in pkgs // {
  python27Packages = myPython2;
  inherit ncbi-blast crb-blast psiblast-exb;
  inherit diamond hmmer mmseqs2;
  inherit (myPython2) blastdbget treeCl;
  inherit raxml;
  inherit orthofinder sonicparanoid;

  # python27Packages = myPython27; # used by treeCl, probably others
  # python36Packages = myPython36; # used by sonicparanoid
  # inherit (myPython) treeCl;
  # python3Packages = myPython3;
}
