with import ./nixpkgs;
let
  mkModule = src: runDepends: extraWraps:
    let name = "Shortcut-" + baseNameOf src;
    in stdenv.mkDerivation {
      inherit src name runDepends extraWraps;
      buildInputs = [ makeWrapper ] ++ runDepends;
      builder = writeScript "builder.sh" ''
        #!/usr/bin/env bash
        source ${stdenv}/setup
        mkdir -p $out/bin
        for script in $src/*; do
          base="$(basename "$script")"
          dest="$out/bin/$base"
          substituteAll $script $dest
          chmod +x $dest
          wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}" ${extraWraps}
        done
      '';
      # TODO shit, that ^ won't work with python3 packages
    };

in rec {
  myBlast = psiblast-exb; # for swapping it out if needed

  myPy2Wrap = "--prefix PYTHONPATH : \"$out/bin:${myPy2.python.sitePackages}\"";
  myPy2 = python27.buildEnv.override {

    # see https://github.com/NixOS/nixpkgs/issues/22319
    ignoreCollisions = true;

    extraLibs = with python27Packages; [
      biopython
      numpy
      scipy
    ];
  };

  myR = rWrapper.override { packages = with rPackages; [
    dplyr
    ggplot2
    readr
    tidyr
    UpSetR
    VennDiagram
    biomartr
    data_table
    futile_logger
  ];};

  shortcut-biomartr      = mkModule ./ShortCut/Modules/BioMartR      [ myR ] "";
  shortcut-blasthits     = mkModule ./ShortCut/Modules/BlastHits     [ myR ] "";
  shortcut-blastrbh      = mkModule ./ShortCut/Modules/BlastRBH      [ myR ] "";
  shortcut-plots         = mkModule ./ShortCut/Modules/Plots         [ myR ] "";
  shortcut-setstable     = mkModule ./ShortCut/Modules/SetsTable     [ myR ] "";
  shortcut-range         = mkModule ./ShortCut/Modules/Range         [ myR ] "";
  shortcut-blast         = mkModule ./ShortCut/Modules/Blast         [ myBlast ] "";
  shortcut-blastdb       = mkModule ./ShortCut/Modules/BlastDB       [ myBlast blastdbget ] "";
  shortcut-crbblast      = mkModule ./ShortCut/Modules/CRBBlast      [ crb-blast ] "";
  shortcut-diamond       = mkModule ./ShortCut/Modules/Diamond       [ diamond ] "";
  shortcut-hmmer         = mkModule ./ShortCut/Modules/Hmmer         [ hmmer ] "";
  shortcut-mmseqs        = mkModule ./ShortCut/Modules/MMSeqs        [ mmseqs2 ] "";
  shortcut-muscle        = mkModule ./ShortCut/Modules/Muscle        [ muscle ] "";
  shortcut-orthofinder   = mkModule ./ShortCut/Modules/OrthoFinder   [ myPy2 myBlast diamond orthofinder mcl fastme ] myPy2Wrap;
  shortcut-psiblast      = mkModule ./ShortCut/Modules/PsiBlast      [ myBlast ] "";
  shortcut-seqio         = mkModule ./ShortCut/Modules/SeqIO         [ myPy2 ] myPy2Wrap;
  # shortcut-sonicparanoid = mkModule ./ShortCut/Modules/SonicParanoid [ sonicparanoid ] "";
  # shortcut-treecl        = mkModule ./ShortCut/Modules/TreeCl        [ python36 treeCl ] "";
  # shortcut-justorthologs = mkModule ./ShortCut/Modules/JustOrthologs [ justorthologs ] "";

  # this config file is only a template; it needs to be completed by busco.sh at runtime
  shortcut-busco = mkModule ./ShortCut/Modules/Busco [ myBlast hmmer busco python36 which ]
                     "--set BUSCO_CONFIG_FILE ${busco}/config/config.ini";

  shortcut-load          = mkModule ./ShortCut/Modules/Load          [ curl ] "";
  shortcut-orthogroups   = mkModule ./ShortCut/Modules/OrthoGroups   [ python36 ] "";
  shortcut-greencut      = mkModule ./ShortCut/Modules/GreenCut      [ myPy2 ] myPy2Wrap;

  modules = [
    shortcut-biomartr
    shortcut-blast
    shortcut-blastdb
    shortcut-blasthits
    shortcut-blastrbh
    shortcut-crbblast
    shortcut-diamond
    shortcut-hmmer
    shortcut-mmseqs
    shortcut-muscle
    shortcut-orthofinder
    shortcut-plots
    shortcut-setstable
    shortcut-psiblast
    shortcut-seqio
    # shortcut-sonicparanoid
    # shortcut-treecl
    # shortcut-justorthologs
    shortcut-busco
    shortcut-load
    shortcut-range
    shortcut-orthogroups
    shortcut-greencut
  ];

  # Things useful for development. The suggested workflow is to uncomment
  # runDepends for only the module(s) you need to develop a given shortcut
  # function, because some of them have incompatible dependencies.
  # Specifically, the python2 and python3 ones interfere with each other.
  devDepends = [
    haskell.compiler.ghc864
    stack
    pypi2nix
  ];
    # ++ shortcut-load.runDepends
    # ++ shortcut-busco.runDepends;
    # ++ shortcut-justorthologs.runDepends; # incompatible with seqio, orthofinder, blastdb?
    # TODO this shouldn't be needed:
    # ++ shortcut-sonicparanoid.runDepends # incompatible with seqio, orthofinder, blastdb?
    # ++ shortcut-biomartr.runDepends
    # ++ shortcut-blast.runDepends
    # ++ shortcut-blastdb.runDepends  # incompatible with sonicparanoid
    #++ shortcut-blasthits.runDepends;
    # ++ shortcut-blastrbh.runDepends
    # ++ shortcut-crbblast.runDepends
    # ++ shortcut-diamond.runDepends
    # ++ shortcut-hmmer.runDepends
    # ++ shortcut-mmseqs.runDepends
    # ++ shortcut-muscle.runDepends
    # ++ shortcut-orthofinder.runDepends;
    # ++ shortcut-orthogroups.runDepends;
    # ++ shortcut-greencut.runDepends;
    # ++ shortcut-plots.runDepends;
    # ++ shortcut-setstable.runDepends;
    # ++ shortcut-psiblast.runDepends
    # ++ shortcut-seqio.runDepends # incompatible with sonicparanoid
    # ++ shortcut-treecl.runDepends # incompatible with sonicparanoid

  # things needed at runtime
  runDepends = modules ++ [
    # cdhit
    diffutils
    glibcLocales
    ncurses # TODO is this needed?
    tree
  ];

  # build this to provide a bunch of "checkpoint" result-* links that could be cached
  # .travis.yml uses is to save the nix store between runs and avoid the 50-min timeout
  travisBuilds = builtins.concatLists (builtins.map (m: m.runDepends) modules) ++ modules;
}
