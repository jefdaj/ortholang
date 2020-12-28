# This is a bit of a transitional mess right now. Eventually, the bigger
# dependencies will go in their own git repos and the smaller ones will be
# moved to ortholang/nix. Either way, they'll be managed with niv.

let
  sources = import ../nix/sources.nix {};
  pkgs = import sources.nixpkgs {};

  psiblast-exb = pkgs.callPackage ./psiblast-exb { };

  easel = pkgs.callPackage sources.easel { };
  hmmer = pkgs.callPackage sources.hmmer { inherit easel; };

  # TODO upload separate repos and switch to importing the with niv
  ncbi-blast = pkgs.callPackage ../../ncbi-blast-nix {}; # follows latest version (2.9.0 now)
  crb-blast  = pkgs.callPackage ../../crb-blast-nix  {}; # uses old blast v 2.2.29


  # see https://nixos.org/nix-dev/2014-December/015243.html
  muscle = pkgs.callPackage ./muscle { };

  diamond = pkgs.callPackage sources.diamond { };

  mmseqs2 = pkgs.callPackage sources.mmseqs2 {};

  mcl    = pkgs.callPackage ../../mcl-nix    { };
  fastme = pkgs.callPackage ../../fastme-nix { };
  orthofinder = pkgs.callPackage sources.orthofinder {
    inherit (pkgs.lib) makeBinPath;
    inherit mcl fastme ncbi-blast diamond;
  };

  # TODO inherit mmseqs2 + mcl again? probably use an overlay to build pkgs instead
  # TODO also get python3Packages back?
  sonicparanoid = pkgs.callPackage sources.sonicparanoid { inherit mmseqs2 mcl; };

  # TODO detect whether MPI version will work on a given computer and adjust
  # TODO upload and import via niv
  raxml  = pkgs.callPackage ../../raxml-nix { mpi = true; };

  # TODO upload and import via niv
  # TODO with treecl and blastdbget packaged separately, does the whole myPython2 set serve a purpose anymore?
  myPython2 = pkgs.python27Packages // rec {
    # TODO upload separate repo and switch to using it with niv
    blastdbget = pkgs.python27Packages.callPackage ../../blastdbget-nix {};
  };

  # TODO upload repo and import via niv
  # TODO and probably put most of release.nix back here? or in its default.nix
  # TODO package biopython 1.76, or whatever the latest was to support python27, for treecl
  treeCl = pkgs.python27Packages.callPackage ../../treecl-nix/release.nix {};

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
  inherit (myPython2) blastdbget;
  inherit (myPython3) busco;

  inherit raxml mcl;
  inherit orthofinder sonicparanoid justorthologs;
}
