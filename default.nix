with import ./nixpkgs;
let
  scripts     = import ./scripts;
  interpreter = haskellPackages.callPackage ./shortcut.nix {};
  runDepends  = [ scripts ncbi-blast crb-blast ];

# see https://github.com/jml/nix-haskell-example
in haskell.lib.overrideCabal interpreter (drv: {
  buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ];
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/shortcut" \
      --prefix PATH ":" "${pkgs.lib.makeBinPath runDepends}"
  '';
  shellHook = ''
    export PATH="${pkgs.lib.makeBinPath runDepends}:\$PATH"
  '';
})
