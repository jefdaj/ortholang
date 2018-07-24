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
    crb-blast
    ncurses # TODO is this needed?
    pythonPackages.blastdbget
    myPython
    psiblast-exb # TODO does this conflict with ncbi-blast+?
    muscle
  ]
    ++ biomartr.runDepends
    ++ blast.runDepends
    ++ blastrbh.runDepends
    ++ blasthits.runDepends
    ++ seqio.runDepends
    ++ plots.runDepends;

# see https://github.com/jml/nix-haskell-example
# TODO final wrapper with +RTS -N -RTS?
in haskell.lib.overrideCabal cabalPkg (drv: {
  buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ] ++ runDepends;
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/shortcut" \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
})
