# TODO could everything in here go in a separate haskell-package.nix or similar?

with import ./nixpkgs;
let
  sources = import ./nix/sources.nix {};

  # Things needed at runtime. Modules are only the scripts called by ortholang,
  # not their indirect (propagated) dependencies since those may conflict.
  # TODO add the ones that don't conflict for easier development?
  inherit (import ./dependencies.nix) runDepends;

  inherit (pkgs.haskell.lib) overrideCabal;

  # current version is ghc884, but that requires updating docopt
  # TODO does it break anything else? possibly `fail` calls in OrthoLang
  myGHC = "ghc884";

  haskellPackages = pkgs.haskell.packages.${myGHC}.override {
    overrides = hpNew: hpOld: {

      # packages that can be fixed with simple overrides
      # MissingH = overrideCabal hpOld.MissingH (_: {jailbreak = true;});
      # progress-meter = overrideCabal hpOld.progress-meter (_: {
      #   broken = false;
      #   jailbreak = true;
      # });

      # packages that had to be forked
      logging = hpOld.callPackage sources.logging {};

      # TODO upgrade to ghc884 and include this at the same time?
      docopt  = hpOld.callPackage sources.docopt {};

      # the ortholang package, which includes the main binary
      # see https://github.com/jml/nix-haskell-example
      # TODO final wrapper with +RTS -N -RTS?
      # TODO get back the enable{Library,Executable}Profiling options?
      ortholang = overrideCabal (hpOld.callCabal2nix "OrthoLang" ./. {}) (drv: {

        # surprisingly, this works as a drop-in replacement for filterSource
        # except with better filtering out of non-source files
        # based on https://github.com/NixOS/nix/issues/885#issuecomment-381904833
        src = builtins.fetchGit { url = ./.; };

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

    };
  };

# to work on a specific module, substitute it here and enter nix-shell
# TODO make an example .nix file for that
# in haskellPackages.ortholang
in {
  project = haskellPackages.ortholang;
  shell = haskellPackages.shellFor {
    packages = p: with p; [
      haskellPackages.ortholang
    ];
    buildInputs = with haskellPackages; [
      ghcid
      hlint
    ];
    # run a web version like this:
    # nix-shell --run hoogle server --port=8080 --local --haskell
    withHoogle = true;
  };
}
