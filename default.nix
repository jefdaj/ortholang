with import ./nixpkgs;
let
  sources = import ./nix/sources.nix {};

  # Things needed at runtime. Modules are only the scripts called by ortholang,
  # not their indirect (propagated) dependencies since those may conflict.
  # TODO add the ones that don't conflict for easier development?
  inherit (import ./dependencies.nix) runDepends;

  inherit (pkgs.haskell.lib) overrideCabal;
  myGHC = "ghc865";
  myHaskell = pkgs.haskell.packages.${myGHC}.override {
    overrides = hpNew: hpOld: {

      # haskell packages that can be fixed with simple overrides
      MissingH = overrideCabal hpOld.MissingH (_: {jailbreak = true;});
      progress-meter = overrideCabal hpOld.progress-meter (_: {
        broken = false;
        jailbreak = true;
      });

      # haskell packages that had to be forked
      logging = hpOld.callPackage sources.logging {};

      # TODO upgrade to ghc884 and include this at the same time?
      # docopt  = hpOld.callPackage sources.docopt {};

    };
  };

  # TODO get back the enable{Library,Executable}Profiling options?
  haskellPkg = myHaskell.callCabal2nix "OrthoLang" ./. {};

  # remove some of my big files to prevent duplicating them in /nix/store
  # TODO remove based on .gitignore file coming in nixpkgs 19.03?
  noBigDotfiles = path: type: baseNameOf path != ".stack-work"
                           && baseNameOf path != ".git";

  # see https://github.com/jml/nix-haskell-example
  # TODO final wrapper with +RTS -N -RTS?
  ortholang = pkgs.haskell.lib.overrideCabal haskellPkg (drv: {
    src = builtins.filterSource noBigDotfiles ./.;

    buildDepends = (drv.buildDepends or [])  ++ runDepends ++ [
      makeWrapper zlib.dev zlib.out pkgconfig
    ];

    # TODO PYTHONPATH?
    postInstall = ''
      ${drv.postInstall or ""}
      wrapProgram "$out/bin/ortholang" \
        --set LANG en_US.UTF-8 \
        --set LANGUAGE en_US.UTF-8 \
        --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"'' +
    (if stdenv.hostPlatform.system == "x86_64-darwin" then "" else '' \
      --set LOCALE_ARCHIVE "${glibcLocales}/lib/locale/locale-archive"
    '');
  });

# to work on a specific module, substitute it here and enter nix-shell
in ortholang
