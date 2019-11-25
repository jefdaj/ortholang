with import ./nixpkgs;
let

  # Things needed at runtime. Modules are only the scripts called by shortcut,
  # not their indirect (propagated) dependencies since those may conflict.
  runDepends = (import ./modules.nix).modules ++ [
    coreutils
    diffutils
    glibcLocales # TODO even on mac?
    tree
    gnutar
    curl
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

    buildDepends = (drv.buildDepends or []) ++ [ makeWrapper ] ++ runDepends;

    # TODO set LC_ALL or similar here?
    # TODO PYTHONPATH?
    postInstall = ''
      ${drv.postInstall or ""}
      wrapProgram "$out/bin/shortcut" \
        --set LC_ALL en_US.UTF-8 \
        --set LANG en_US.UTF-8 \
        --set LANGUAGE en_US.UTF-8 \
        --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"'' +
    (if stdenv.hostPlatform.system == "x86_64-darwin" then "" else '' \
      --set LOCALE_ARCHIVE "${glibcLocales}/lib/locale/locale-archive"
    '');
  });

# to work on a specific module, substitute it here and enter nix-shell
in shortcut
