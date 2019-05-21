with import ./nixpkgs;
let
  biomartr   = import ./ShortCut/Modules/BioMartR;
  blast      = import ./ShortCut/Modules/Blast;
  blastdb    = import ./ShortCut/Modules/BlastDB;
  blastrbh   = import ./ShortCut/Modules/BlastRBH;
  crbblast   = import ./ShortCut/Modules/CRBBlast;
  seqio      = import ./ShortCut/Modules/SeqIO;
  plots      = import ./ShortCut/Modules/Plots;
  hmmer      = import ./ShortCut/Modules/Hmmer;
  blasthits  = import ./ShortCut/Modules/BlastHits;
  diamond    = import ./ShortCut/Modules/Diamond;
  mmseqs     = import ./ShortCut/Modules/MMSeqs;

  # TODO will this conflict with the pkg itself?
  muscle     = import ./ShortCut/Modules/Muscle;

  # TODO will this conflict with the pkg itself?
  sonicparanoid = import ./ShortCut/Modules/SonicParanoid;

  myPython = pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);
  # it works best if the ghc version here matches the resolver in stack.yaml
  cabalPkg = haskell.packages.ghc844.callPackage ./shortcut.nix {};
  devDepends = [
    haskell.compiler.ghc844
    stack
  ];
  runDepends = [
    biomartr
    blast
    blastdb
    blastrbh
    muscle
    crbblast
    seqio
    plots
    blasthits
    crb-blast
    ncurses # TODO is this needed?
    pythonPackages.blastdbget
    myPython
    psiblast-exb # TODO does this conflict with ncbi-blast+?
    hmmer
    orthofinder
    diamond
    # cdhit
    sonicparanoid
    glibcLocales
    tree
    diffutils
    mmseqs
  ]
    ++ biomartr.runDepends
    ++ blastdb.runDepends
    ++ blast.runDepends
    ++ blastrbh.runDepends
    ++ crbblast.runDepends
    ++ blasthits.runDepends
    ++ muscle.runDepends
    ++ seqio.runDepends
    ++ plots.runDepends
    ++ hmmer.runDepends
    ++ sonicparanoid.runDepends
    ++ diamond.runDepends
    ++ mmseqs.runDepends;

  # explicitly remove .stack-work from nix source because it's big
  # TODO remove based on .gitignore file coming in nixpkgs 19.03?
  notStack = path: type: baseNameOf path != ".stack-work";

# see https://github.com/jml/nix-haskell-example
# TODO final wrapper with +RTS -N -RTS?
in haskell.lib.overrideCabal cabalPkg (drv: {
  src = builtins.filterSource notStack ./.;
  shellHook = ''
     export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
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
