# Maintainers willing, everything here is destined to be merged into
# the main upstream repository at https://github.com/nixos/nixpkgs
# as soon as I have the time

let
  # fetch my pinned nixpkgs for reproducibility.
  # use this instead to try to build it with your system's current nixpkgs:
  # pkgs = import <nixpkgs> {};
  # to update the the sha256sum, use nix-prefetch-url --unpack
  # see https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev = "e19054ab3cd5b7cc9a01d0efc71c8fe310541065"; # nixos-19.03 as of 2019-09-11
    sha256 = "0b92yhkj3pq58svyrx7jp0njhaykwr29079izqn6qs638v8zvhl2";
  };
  pkgs = import nixpkgs {};

  psiblast-exb = pkgs.callPackage ./psiblast-exb { };

  hmmer = pkgs.callPackage ./hmmer { };

  # ... because it only supports exactly 2.2.29
  # and there are reports of a bug in newer ones (still?)
  # TODO patch crb-blast to use the newest one?
  ncbi-blast-2_2_29 = (pkgs.callPackage ./ncbi-blast {}).overrideDerivation (old: rec {
    version="2.2.29";
    name="ncbi-blast-${version}";
    src = pkgs.fetchurl {
      url = "ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/2.2.29/ncbi-blast-2.2.29+-x64-linux.tar.gz";
      sha256="1pzy0ylkqlbj40mywz358ia0nq9niwqnmxxzrs1jak22zym9fgpm";
    };
  });

  crb-blast  = pkgs.callPackage ./crb-blast  { ncbi-blast = ncbi-blast-2_2_29; };

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
      inherit (pkgs.python27Packages) pyyaml biopython cython dendropy futures ipython;
      inherit (pkgs.python27Packages) matplotlib nose numpy pandas progressbar scikitlearn scipy;
    };
  };

  myPython3 = pkgs.python36Packages // rec {
    busco = pkgs.python36Packages.callPackage ./busco {
      inherit (pkgs.lib) makeBinPath;
      inherit psiblast-exb;
    };
  };

  justorthologs = pkgs.callPackage ./justorthologs {};

in pkgs // {
  inherit ncbi-blast crb-blast psiblast-exb;
  inherit fastme;
  inherit diamond hmmer mmseqs2;

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
