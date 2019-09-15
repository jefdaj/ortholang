with import ./nixpkgs;
let

  # Things needed at runtime. Modules are only the scripts called by shortcut,
  # not their indirect (propagated) dependencies since those may conflict with
  # each other.
  runDepends = (import ./modules.nix).modules ++ [
    diffutils
    glibcLocales
    # ncurses # TODO is this needed?
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
  stackPackages = (import ./stack.nix) { inherit pkgs; compiler = myGHC; };
  haskellPkg = myGHC.callPackage ./shortcut.nix {}; # a cabal2nix-generated Cabal package

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
      export TASTY_HIDE_SUCCESSES=True
    '';

    buildDepends = (drv.buildDepends or [])
      ++ [ makeWrapper ]
      ++ runDepends
      ++ (if pkgs.lib.inNixShell then devDepends else []);

    # TODO set LC_ALL or similar here?
    # TODO PYTHONPATH?
    postInstall = ''
      wrapProgram "$out/bin/shortcut" \
        --set LOCALE_ARCHIVE "${glibcLocales}/lib/locale/locale-archive" \
        --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
      ${drv.postInstall or ""}
    '';
  });

# to work on a specific module, substitute it here and enter nix-shell
in shortcut
