# This hooks up the Haskell package with all its runtime dependencies. For
# development, you might want to nix-shell shell.nix instead of this one,
# because it includes fanciness like Hoogle and ghcid. But either should work.

let
  # sources = import ./nix/sources.nix {};
  # pkgs    = import sources.nixpkgs {};
  pkgs    = import ./nix;
  myHs    = import ./haskell.nix;
  modules = (import ./modules.nix).modules;
  environment = import ./environment.nix; # TODO can most of this be removed?
  runDepends = environment ++ modules;

in pkgs.haskell.lib.overrideCabal myHs.OrthoLang (drv: {

  # surprisingly, this works as a drop-in replacement for filterSource
  # except with better filtering out of non-source files
  # based on https://github.com/NixOS/nix/issues/885#issuecomment-381904833
  # src = builtins.fetchGit { url = ./.; };

  # TODO remove these? are they still needed?
  buildDepends = with pkgs; (drv.buildDepends or [])  ++ runDepends ++ [
    makeWrapper
    # zlib.dev zlib.out # TODO remove?
    # pkgconfig # TODO remove?
  ];

  # TODO PYTHONPATH?
  # TODO any reason to factor this out into default.nix?
  postInstall = ''
    ${drv.postInstall or ""}
    wrapProgram "$out/bin/ortholang" \
      --set LANG en_US.UTF-8 \
      --set LANGUAGE en_US.UTF-8 \
      --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"'' +
  (if pkgs.stdenv.hostPlatform.system == "x86_64-darwin" then "" else '' \
    --set LOCALE_ARCHIVE "${pkgs.glibcLocales}/lib/locale/locale-archive"
  '');

  shellHook = ''
    ${drv.shellHook or ""}
    export LANG=en_US.UTF-8
    export LANGUAGE=en_US.UTF-8
    # export TASTY_HIDE_SUCCESSES=True
  '' ++
  (if pkgs.stdenv.hostPlatform.system == "x86_64-darwin" then "" else ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '');
 
})
