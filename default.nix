with import ./nixpkgs;
let
  biomartr   = import ./ShortCut/Modules/BioMartR;
  seqio      = import ./ShortCut/Modules/SeqIO;
  cabalPkg   = haskellPackages.callPackage ./shortcut.nix {};
  runDepends = [ biomartr seqio ncbi-blast crb-blast ncurses ] # TODO ncurses?
               ++ biomartr.runDepends
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
