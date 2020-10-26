# TODO could everything in here go in a separate haskell-package.nix or similar?
# TODO rename back to release.nix?

# TODO rename haskell.nix or similar? would explain it a bit more, and be good for importing into the demo/docs package

# to work on a specific module, substitute it here and enter nix-shell
# TODO make an example .nix file for that

with import ./nixpkgs;
let
  sources = import ./nix/sources.nix {};

  # Things needed at runtime. Modules are only the scripts called by ortholang,
  # not their indirect (propagated) dependencies since those may conflict.
  # TODO add the ones that don't conflict for easier development?
  # modules = (import ./modules.nix).modules;

  # Haskell stuff! It starts with the upstream haskellPackages set for the
  # chosen compiler, then we override a few dependencies, and finally we define
  # the ortholang package.
  inherit (pkgs.haskell.lib) overrideCabal dontCheck;
  myGHC = "ghc884";

in pkgs.haskell.packages.${myGHC}.override {
  overrides = hpNew: hpOld: {

    # Packages that can be fixed with simple overrides
    # TODO try on hpc: unliftio = dontCheck hpOld.unliftio;
    # TODO try on hpc: unliftio = hpNew.callHackage "unliftio" "0.2.12.1" {};
    progress-meter = overrideCabal hpOld.progress-meter (_: {
      broken = false;
      jailbreak = true;
    });

    # Packages that had to be forked
    logging = hpNew.callPackage sources.logging {};
    docopt  = hpNew.callPackage sources.docopt {};

    # The ortholang package, which includes the main binary
    # TODO final wrapper with +RTS -N -RTS?
    # TODO get back the enable{Library,Executable}Profiling options?
    # TODO can ortholang.nix be replace with callCabal2nix "OrthoLang" ./. {}?
    # ortholang = overrideCabal (hpNew.callPackage ./ortholang.nix {}) (drv: {
    OrthoLang = hpNew.callPackage ./default.nix {};
  };
}
