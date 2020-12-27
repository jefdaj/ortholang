# This is a bit of a transitional mess right now. Eventually, the bigger
# dependencies will go in their own git repos and the smaller ones will be
# moved to ortholang/nix. Either way, they'll be managed with niv.

let
  sources = import ../nix/sources.nix {};
  pkgs = import sources.nixpkgs {};

  # These are self-contained enough I should be able to add them directly to
  # nixpkgs without worrying about maintaining a repo
  # TODO add to ../nix as overlays?
  # TODO pull requests!
  mcl = pkgs.callPackage ./mcl { };

  psiblast-exb = pkgs.callPackage ./psiblast-exb { };

  easel = pkgs.callPackage sources.easel { };
  hmmer = pkgs.callPackage sources.hmmer { inherit easel; };

  # TODO upload separate repos and switch to importing the with niv
  ncbi-blast = pkgs.callPackage ../../ncbi-blast-nix {}; # follows latest version (2.9.0 now)
  crb-blast  = pkgs.callPackage ../../crb-blast-nix  {}; # uses old blast v 2.2.29

  fastme = pkgs.callPackage ./fastme { };

  # see https://nixos.org/nix-dev/2014-December/015243.html
  muscle = pkgs.callPackage ./muscle { };

  diamond = pkgs.callPackage sources.diamond { };

  mmseqs2 = pkgs.callPackage sources.mmseqs2 {};

  orthofinder = pkgs.callPackage ./orthofinder {
    inherit (pkgs.lib) makeBinPath;
    inherit mcl fastme ncbi-blast diamond;
  };

  # TODO inherit mmseqs2 + mcl again? probably use an overlay to build pkgs instead
  # TODO also get python3Packages back?
  sonicparanoid = pkgs.callPackage sources.sonicparanoid { inherit mmseqs2 mcl; };

  # TODO detect whether MPI version will work on a given computer and adjust
  # TODO uplaod and import via niv
  raxml  = pkgs.callPackage ../../raxml-nix { mpi = true; };

  # TODO should treeCl go inside the python packages instead of the other way around?
  myPython2 = pkgs.python27Packages // rec {
    fastcluster        = pkgs.python27Packages.callPackage ./fastcluster {};
    fasttree           = pkgs.python27Packages.callPackage ./fasttree {};
    tree_distance      = pkgs.python27Packages.callPackage ./tree_distance {};
    progressbar-latest = pkgs.python27Packages.callPackage ./progressbar-latest {};
    CacheControl       = pkgs.python27Packages.callPackage ./CacheControl {};
    scikit-bio         = pkgs.python27Packages.callPackage ./scikit-bio { inherit CacheControl; };
    phylo_utils        = pkgs.python27Packages.callPackage ./phylo_utils {};

    # TODO upload separate repo and switch to using it with niv
    blastdbget = pkgs.python27Packages.callPackage ../../blastdbget-nix {};

    treeCl = pkgs.python27Packages.callPackage ./treeCl {
      inherit raxml; # TODO why doesn't it find this?
      inherit fastcluster fasttree tree_distance progressbar-latest CacheControl scikit-bio phylo_utils;
      inherit (pkgs.python27Packages) pyyaml biopython cython dendropy futures;
      inherit (pkgs.python27Packages) matplotlib nose numpy pandas progressbar scikitlearn scipy;
    };
  };

  myPython3 = pkgs.python3Packages // rec {
    busco = pkgs.python3Packages.callPackage sources.busco {
      inherit (pkgs.lib) makeBinPath;
      inherit ncbi-blast hmmer;
    };
  };

  justorthologs = pkgs.callPackage ./justorthologs {};

# TODO these should probably be converted to a list of overlays
in pkgs // {
  inherit ncbi-blast crb-blast psiblast-exb;
  inherit fastme;
  inherit diamond hmmer mmseqs2 muscle;

  # TODO will these interfere with each other?
  python27Packages = myPython2;
  python3Packages = myPython3;
  inherit (myPython2) blastdbget treeCl;
  inherit (myPython3) busco;

  inherit raxml mcl;
  inherit orthofinder sonicparanoid justorthologs;

  # python27Packages = myPython27; # used by treeCl, probably others
  # python3Packages = myPython36; # used by sonicparanoid
  # inherit (myPython) treeCl;
  # python3Packages = myPython3;
}
