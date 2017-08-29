with import ./nixpkgs;
let
  biomartr   = import ./src/ShortCut/Modules/BioMartR;
  blast      = import ./src/ShortCut/Modules/Blast;
  seqio      = import ./src/ShortCut/Modules/SeqIO;
  cabalPkg   = haskellPackages.callPackage ./src/shortcut.nix {};
  runDepends = [ biomartr seqio ncbi-blast crb-blast ncurses ] # TODO ncurses?
               ++ biomartr.runDepends
               ++ blast.runDepends
               ++ seqio.runDepends;

# see https://github.com/jml/nix-haskell-example
in haskell.lib.overrideCabal cabalPkg (drv: {
  buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ncurses ] ++ runDepends;
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/shortcut" \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
})
