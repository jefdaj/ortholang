with import ./nixpkgs;
with import ./dependencies.nix;
let

  # it works best if the ghc version here matches the resolver in stack.yaml
  # TODO pass compiler explicitly?
  cabalPkg = ((import ./stack.nix) { inherit pkgs; }).ShortCut;

  # remove some of my big files to prevent duplicating them in /nix/store
  # TODO remove based on .gitignore file coming in nixpkgs 19.03?
  noBigDotfiles = path: type: baseNameOf path != ".stack-work" && baseNameOf path != ".git";

  # see https://github.com/jml/nix-haskell-example
  # TODO final wrapper with +RTS -N -RTS?
  shortcut = haskell.lib.overrideCabal cabalPkg (drv: {
    src = builtins.filterSource noBigDotfiles ./.;

    # TODO this isn't being run by overrideCabal at all. get it to work
    # TODO is the find command going? that could maybe make the difference
    shellHook = ''
      ${drv.shellHook or ""}
      export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
      export TASTY_HIDE_SUCCESSES=True
      find "${src}/ShortCut/Modules/* -type d" | while read d; do
        export PATH=$d:$PATH
      done
    '';

    buildDepends = (drv.buildDepends or [])
      ++ [ makeWrapper ]
      ++ runDepends
      ++ (if pkgs.lib.inNixShell then devDepends else []);
    # TODO set LC_ALL or similar here?
    # TODO PYTHONPATH?
    postInstall = ''
      ${drv.postInstall or ""}
      wrapProgram "$out/bin/shortcut" \
        --set LOCALE_ARCHIVE "${glibcLocales}/lib/locale/locale-archive" \
        --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    '';
  });

# to work on a specific module, substitute it here and enter nix-shell:
in shortcut
