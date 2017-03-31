# The source code is split up into 3 major parts:
#
# 1. shortcut, which contains the haskell binary of the same name
# 2. scripts, which contains scripts called by the shortcut binary
# 3. nixpkgs, which contains misc dependencies I had to package
#
# Then there are a bunch of misc build instructions at the top level.  To
# update Haskell dependencies, change the cabal file and run
# `cabal2nix shortcut > shortcut.nix`.

# TODO auto-run tests if possible (in nix? cabal?)
# TODO use stack and/or make some kind of cabal shell to speed compilation

with import ./nixpkgs;

let
  scripts  = import ./scripts;
  shortcut = haskellPackages.callPackage ./shortcut.nix {};

in stdenv.mkDerivation {

  name = "shortcut";
  # TODO version?
  buildInputs = [ bblast scripts shortcut ];

  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin

    cat << EOF > $out/bin/shortcut
    #!/usr/bin/env bash
    export PATH=${bblast}/bin:\$PATH
    export PATH=${scripts}/bin:\$PATH
    export PATH=${shortcut}/bin:\$PATH
    shortcut \$@
    EOF

    chmod +x $out/bin/shortcut
  '';
}
