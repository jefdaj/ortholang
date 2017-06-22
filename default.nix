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
  myR = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      dplyr
      biomartr # TODO is version 0.50 any better for now?
    ];
  };
  myPython = pkgs.pythonPackages.python.withPackages (ps: with ps; [
    biopython
  ]);
  shortcut = haskellPackages.callPackage ./shortcut.nix {};

in stdenv.mkDerivation rec {
  # TODO version?
  name = "shortcut";
  src = ./.;
  buildInputs = [ makeWrapper ];
  runDepends = [
    shmlast
    ncbi-blast
    crb-blast
    coreutils
    last-align
    myR
    myPython
  ];
  shellDepends = [
    which
    stack # TODO stack in nix-shell, or stack with nix-managed dependenencies?
  ];

  # TODO why does this work when the haskell overrideDerivation style doesn't?
  #      (see https://github.com/jml/nix-haskell-example)
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    wrapperScript=$out/bin/shortcut
    cat << EOF > $wrapperScript
    #!/usr/bin/env bash
    ${shortcut}/bin/shortcut \$@
    EOF
    chmod +x $wrapperScript
    wrapProgram $wrapperScript --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
  '';

  # TODO how to avoid building shortcut for this?
  shellHook = ''
    export PATH="${pkgs.lib.makeBinPath   runDepends}:\$PATH"
    export PATH="${pkgs.lib.makeBinPath shellDepends}:\$PATH"
  '';
}
