with import ./nixpkgs;
let

  shortcut-biomartr      = import ./ShortCut/Modules/BioMartR;
  shortcut-blast         = import ./ShortCut/Modules/Blast;
  shortcut-blastdb       = import ./ShortCut/Modules/BlastDB;
  shortcut-blasthits     = import ./ShortCut/Modules/BlastHits;
  shortcut-blastrbh      = import ./ShortCut/Modules/BlastRBH;
  shortcut-crbblast      = import ./ShortCut/Modules/CRBBlast;
  shortcut-diamond       = import ./ShortCut/Modules/Diamond;
  shortcut-hmmer         = import ./ShortCut/Modules/Hmmer;
  shortcut-mmseqs        = import ./ShortCut/Modules/MMSeqs;
  shortcut-muscle        = import ./ShortCut/Modules/Muscle;
  shortcut-orthofinder   = import ./ShortCut/Modules/OrthoFinder;
  shortcut-plots         = import ./ShortCut/Modules/Plots;
  shortcut-psiblast      = import ./ShortCut/Modules/PsiBlast;
  shortcut-seqio         = import ./ShortCut/Modules/SeqIO;
  shortcut-sonicparanoid = import ./ShortCut/Modules/SonicParanoid;
  shortcut-treecl        = import ./ShortCut/Modules/TreeCl;
  shortcut-justorthologs = import ./ShortCut/Modules/JustOrthologs;
  shortcut-busco         = import ./ShortCut/Modules/Busco;
  shortcut-load          = import ./ShortCut/Modules/Load;
  shortcut-range         = import ./ShortCut/Modules/Range;

  # myPython = python27Packages.python.withPackages (ps: with ps; [
    # biopython
    # treeCl
  # ]);

  # it works best if the ghc version here matches the resolver in stack.yaml
  cabalPkg = haskell.packages.ghc844.callPackage ./shortcut.nix {};

  # Things useful for development. The suggested workflow is to uncomment
  # runDepends for only the module(s) you need to develop a given shortcut
  # function, because some of them have incompatible dependencies.
  # Specifically, the python2 and python3 ones interfere with each other.
  devDepends = [
    haskell.compiler.ghc844
    stack
  ];
    # ++ shortcut-load.runDepends
    # ++ shortcut-busco.runDepends;
    # ++ shortcut-justorthologs.runDepends; # incompatible with seqio, orthofinder, blastdb?
    # TODO this shouldn't be needed:
    # ++ shortcut-sonicparanoid.runDepends; # incompatible with seqio, orthofinder, blastdb?
    # ++ shortcut-biomartr.runDepends
    # ++ shortcut-blast.runDepends
    # ++ shortcut-blastdb.runDepends  # incompatible with sonicparanoid
    # ++ shortcut-blasthits.runDepends
    # ++ shortcut-blastrbh.runDepends
    # ++ shortcut-crbblast.runDepends
    # ++ shortcut-diamond.runDepends
    # ++ shortcut-hmmer.runDepends
    # ++ shortcut-mmseqs.runDepends
    # ++ shortcut-muscle.runDepends
    # ++ shortcut-orthofinder.runDepends
    # ++ shortcut-plots.runDepends
    # ++ shortcut-psiblast.runDepends
    # ++ shortcut-seqio.runDepends # incompatible with sonicparanoid
    # ++ shortcut-treecl.runDepends # incompatible with sonicparanoid

  # things needed at runtime
  runDepends = [
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
    shortcut-psiblast
    shortcut-seqio
    shortcut-sonicparanoid
    shortcut-treecl
    shortcut-justorthologs
    shortcut-busco
    shortcut-load
    shortcut-range

    # cdhit
    diffutils
    glibcLocales
    ncurses # TODO is this needed?
    tree
  ];

  # remove some of my big files to prevent duplicating them in /nix/store
  # TODO remove based on .gitignore file coming in nixpkgs 19.03?
  noBigDotfiles = path: type: baseNameOf path != ".stack-work" && baseNameOf path != ".git";

# see https://github.com/jml/nix-haskell-example
# TODO final wrapper with +RTS -N -RTS?
in haskell.lib.overrideCabal cabalPkg (drv: {
  src = builtins.filterSource noBigDotfiles ./.;

  # TODO this isn't being run by overrideCabal at all. get it to work
  shellHook = ''
    ${drv.shellHook or ""}
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    export TASTY_HIDE_SUCCESSES=True
    find "${src}/ShortCut/Modules/* -type d" | while read d; do
      export PATH=$d:$PATH
    done
  '';

  buildDepends = (drv.buildDepends or [])
    ++ [ makeWrapper ]
    ++ runDepends
    ++ (if pkgs.lib.inNixShell then devDepends else []);
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/shortcut" \
      --set LOCALE_ARCHIVE "${glibcLocales}/lib/locale/locale-archive" \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
})
