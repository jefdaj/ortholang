# Maintainers willing, everything here is destined to be merged into
# the main upstream repository at https://github.com/nixos/nixpkgs
# as soon as I have the time

let
  # fetch my pinned nixpkgs for reproducibility.
  # see https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/
  # to update the the sha256sum:
  # nix-prefetch-url --unpack https://github.com/jefdaj/nixpkgs/archive/<rev>.tar.gz
  pkgs = let
    inherit (import <nixpkgs> {}) stdenv fetchFromGitHub;
  in import (fetchFromGitHub {
    owner  = "jefdaj";
    repo   = "nixpkgs";
    rev    = "fd3aa9e6e403549f97f3ff2be4b12b8227d6f8f9";
    sha256 = "0pnbq5119218nhrsm8m6kxxdq2r8zg1abrg31yb6ihbvmin43hv0";
  }) {};

  # use this instead to try to build it with your system's current nixpkgs:
  # pkgs = import /home/jefdaj/nixpkgs {};

  psiblast-exb = pkgs.callPackage ./psiblast-exb { };

  hmmer = pkgs.callPackage ./hmmer { };
  ncbi-blast = pkgs.callPackage ./ncbi-blast {}; # follows latest version (2.9.0 now)

  # crb-blast only supports exactly 2.2.29
  # and there are reports of a bug in newer ones (TODO still?)
  ncbi-blast-2_2_29 = (pkgs.callPackage ./ncbi-blast {}).overrideDerivation (old: rec {
    version="2.2.29";
    name="ncbi-blast-${version}";
    src = if pkgs.stdenv.hostPlatform.system == "x86_64-darwin"
      then (pkgs.fetchurl {
        url = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/2.2.29/ncbi-blast-2.2.29+-universal-macosx.tar.gz";
        sha256="00g8pzwx11wvc7zqrxnrd9xad68ckl8agz9lyabmn7h4k07p5yll";
      }) else (pkgs.fetchurl {
        url = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/2.2.29/ncbi-blast-2.2.29+-x64-linux.tar.gz";
        sha256="1pzy0ylkqlbj40mywz358ia0nq9niwqnmxxzrs1jak22zym9fgpm";
      });
  });
  crb-blast  = pkgs.callPackage ./crb-blast  { ncbi-blast = ncbi-blast-2_2_29; };

  # cdhit = pkgs.callPackage ./cdhit { };

  mcl = pkgs.callPackage ./mcl { };

  fastme = pkgs.callPackage ./fastme { };

  # see https://nixos.org/nix-dev/2014-December/015243.html
  muscle = pkgs.callPackage ./muscle { };

  diamond = pkgs.callPackage ./diamond { };

  mmseqs2 = pkgs.callPackage ./mmseqs2 { };

  orthofinder = pkgs.callPackage ./orthofinder {
    inherit (pkgs.lib) makeBinPath;
    inherit mcl fastme ncbi-blast diamond;
  };

  # TODO push new sh-1.12.14 upstream! haven't managed to include it properly here
  # TODO is this pulling in python3 and messing up treeCl?
  sonicparanoid = pkgs.callPackage ./sonicparanoid {
    inherit mmseqs2 mcl;
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
      inherit (pkgs.python27Packages) pyyaml biopython cython dendropy futures;
      inherit (pkgs.python27Packages) matplotlib nose numpy pandas progressbar scikitlearn scipy;
    };
  };

  myPython3 = pkgs.python36Packages // rec {
    busco = pkgs.python36Packages.callPackage ./busco {
      inherit (pkgs.lib) makeBinPath;
      inherit ncbi-blast hmmer;
    };
  };

  justorthologs = pkgs.callPackage ./justorthologs {};

in pkgs // {
  inherit ncbi-blast crb-blast psiblast-exb;
  inherit fastme;
  inherit diamond hmmer mmseqs2 muscle;

  # TODO will these interfere with each other?
  python27Packages = myPython2;
  python36Packages = myPython3;
  inherit (myPython2) blastdbget treeCl;
  inherit (myPython3) busco;

  inherit raxml mcl;
  inherit orthofinder sonicparanoid justorthologs;

  # python27Packages = myPython27; # used by treeCl, probably others
  # python36Packages = myPython36; # used by sonicparanoid
  # inherit (myPython) treeCl;
  # python3Packages = myPython3;
}
