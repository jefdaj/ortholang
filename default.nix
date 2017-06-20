# The source code is split up into 3 major parts:
#
# 1. shortcut, which contains the haskell binary of the same name
# 2. scripts, which contains scripts called by the shortcut binary
# 3. nixpkgs, which contains misc dependencies I had to package
#
# Then there are a bunch of misc build instructions at the top level.  To
# update Haskell dependencies, change the cabal file and run
# `cabal2nix . > shortcut.nix`.
# TODO add cabal2nix to global nix pkgs

# TODO auto-run tests if possible (in nix? cabal?)
# TODO use stack and/or make some kind of cabal shell to speed compilation

with import ./nixpkgs;

let
  scripts  = import ./scripts;
  shortcut = haskellPackages.callPackage ./shortcut.nix {};

  # from https://github.com/jml/nix-haskell-example
  # TODO make this cleaner, or maybe remove
  addRuntimeDependencies = drv: xs: haskell.lib.overrideCabal drv (drv: {
    buildDepends = (drv.buildDepends or []) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      ${drv.postInstall or ""}
      for exe in "$out/bin/"* ; do
        wrapProgram "$exe" --prefix PATH ":" ${pkgs.lib.makeBinPath xs}
      done
    '';
  });

in addRuntimeDependencies
     shortcut
     [bblast scripts]
