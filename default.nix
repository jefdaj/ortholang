with import ./nixpkgs;
let
  biomartr   = import ./ShortCut/Modules/BioMartR;
  blast      = import ./ShortCut/Modules/Blast;
  blastrbh   = import ./ShortCut/Modules/BlastRBH;
  seqio      = import ./ShortCut/Modules/SeqIO;
  plots      = import ./ShortCut/Modules/Plots;
  blasthits  = import ./ShortCut/Modules/BlastHits;
  myPython = pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);
  cabalPkg   = haskellPackages.callPackage ./shortcut.nix {};
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
    hmmer
    orthofinder
    diamond
    mmseqs2
    cdhit
    # sonicparanoid
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
