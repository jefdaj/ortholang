with import ./nixpkgs;
let
  scripts    = import ./scripts;
  cabalPkg   = haskellPackages.callPackage ./shortcut.nix {};
  runDepends = scripts.runDepends ++ [ scripts ncbi-blast crb-blast ];

# see https://github.com/jml/nix-haskell-example
in haskell.lib.overrideCabal cabalPkg (drv: {
  buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ] ++ runDepends;
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/shortcut" \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';
})
