with import ./nixpkgs;
let

  # Things needed at runtime. Modules are only the scripts called by shortcut,
  # not their indirect (propagated) dependencies since those may conflict.
  runDepends = (import ./modules.nix).modules ++ [
    diffutils
    glibcLocales
    tree
    gnutar
    curl
  ];

  # Things useful for development, which are included if going into nix-shell.
  # Uncomment or add to it as needed.
  devDepends = [
    stack
    # pypi2nix
  ];

  myGHC = pkgs.haskell.packages.ghc864;
  haskellPkg = myGHC.callPackage ./shortcut.nix {};

  # remove some of my big files to prevent duplicating them in /nix/store
  # TODO remove based on .gitignore file coming in nixpkgs 19.03?
  noBigDotfiles = path: type: baseNameOf path != ".stack-work"
                           && baseNameOf path != ".git";

  # see https://github.com/jml/nix-haskell-example
  # TODO final wrapper with +RTS -N -RTS?
  shortcut = haskell.lib.overrideCabal haskellPkg (drv: {
    src = builtins.filterSource noBigDotfiles ./.;

    # TODO this isn't being run by overrideCabal at all. get it to work
    # TODO is the find command going? that could maybe make the difference
    shellHook = ''
      ${drv.shellHook or ""}
      export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
      export LC_ALL=en_US.UTF-8
      export LANG=en_US.UTF-8
      export LANGUAGE=en_US.UTF-8
      export TASTY_HIDE_SUCCESSES=True
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
        --set LC_ALL en_US.UTF-8 \
        --set LANG en_US.UTF-8 \
        --set LANGUAGE en_US.UTF-8 \
        --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    '';
  });

# to work on a specific module, substitute it here and enter nix-shell
in shortcut
