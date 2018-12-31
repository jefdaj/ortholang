with import ./nixpkgs;
let
  biomartr   = import ./Detourrr/Modules/BioMartR;
  blast      = import ./Detourrr/Modules/Blast;
  blastrbh   = import ./Detourrr/Modules/BlastRBH;
  seqio      = import ./Detourrr/Modules/SeqIO;
  plots      = import ./Detourrr/Modules/Plots;
  blasthits  = import ./Detourrr/Modules/BlastHits;
  diamond    = import ./Detourrr/Modules/Diamond;
  myPython = pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);
  cabalPkg   = haskellPackages.callPackage ./detourrr.nix {};
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
    # cdhit
    sonicparanoid
  ]
    ++ biomartr.runDepends
    ++ blast.runDepends
    ++ blastrbh.runDepends
    ++ blasthits.runDepends
    ++ seqio.runDepends
    ++ plots.runDepends
    ++ diamond.runDepends;

# see https://github.com/jml/nix-haskell-example
# TODO final wrapper with +RTS -N -RTS?
in haskell.lib.overrideCabal cabalPkg (drv: {
  buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ] ++ runDepends;
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/detourrr" \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
})
