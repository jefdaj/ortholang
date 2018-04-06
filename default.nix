with import ./nixpkgs;
let
  biomartr   = import ./src/ShortCut/Modules/BioMartR;
  blast      = import ./src/ShortCut/Modules/Blast;
  blastrbh   = import ./src/ShortCut/Modules/BlastRBH;
  seqio      = import ./src/ShortCut/Modules/SeqIO;
  plots      = import ./src/ShortCut/Modules/Plots;
  blasthits  = import ./src/ShortCut/Modules/BlastHits;
  myPython = pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);
  cabalPkg   = haskellPackages.callPackage ./src/shortcut.nix {};
  runDepends = [
    biomartr
    blast
    blastrbh
    seqio
    plots
    blasthits
    ncbi-blast
    crb-blast
    ncurses # TODO is this needed?
    pythonPackages.blastdbget
    myPython
  ]
    ++ biomartr.runDepends
    ++ blast.runDepends
    ++ blastrbh.runDepends
    ++ blasthits.runDepends
    ++ seqio.runDepends
    ++ plots.runDepends;

# see https://github.com/jml/nix-haskell-example
in haskell.lib.overrideCabal cabalPkg (drv: {
  buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ] ++ runDepends;
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/shortcut" \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
})
