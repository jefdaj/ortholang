with import ./nixpkgs;
let
  biomartr   = import ./ShortCut/Modules/BioMartR;
  seqio      = import ./ShortCut/Modules/SeqIO;
  cabalPkg   = haskellPackages.callPackage ./shortcut.nix {};
  runDepends = biomartr.runDepends ++ seqio.runDepends ++ [ ncbi-blast crb-blast ];

# see https://github.com/jml/nix-haskell-example
in haskell.lib.overrideCabal cabalPkg (drv: {
  buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ] ++ runDepends;
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/shortcut" \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
})
