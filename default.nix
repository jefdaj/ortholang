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

  # myPython = python27Packages.python.withPackages (ps: with ps; [
    # biopython
    # treeCl
  # ]);

  # it works best if the ghc version here matches the resolver in stack.yaml
  cabalPkg = haskell.packages.ghc844.callPackage ./shortcut.nix {};

  # add haskell dev stuff to nix-shell
  devDepends = [
    haskell.compiler.ghc844
    stack
  ];

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
    shortcut-sonicparanoid
    shortcut-seqio
    shortcut-treecl

    # cdhit
    # diamond
    # hmmer
    # psiblast-exb # TODO does this conflict with ncbi-blast+?
    # python27Packages
    # python27Packages.treeCl
    # raxml
    diffutils
    glibcLocales
    ncurses # TODO is this needed?
    # python27Packages.blastdbget
    tree
  ];
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

  # explicitly remove .stack-work from nix source because it's big
  # TODO remove based on .gitignore file coming in nixpkgs 19.03?
  notStack = path: type: baseNameOf path != ".stack-work";

# see https://github.com/jml/nix-haskell-example
# TODO final wrapper with +RTS -N -RTS?
in haskell.lib.overrideCabal cabalPkg (drv: {
  src = builtins.filterSource notStack ./.;
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
