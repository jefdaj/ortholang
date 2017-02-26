with import <nixpkgs> {};

# TODO scripts-shell for working on individual scripts (split by language?)
# TODO auto-run tests if possible (in nix? cabal?)
# TODO don't handle PYTHONPATH here
# TODO don't handle script paths here?

let
  bblast = import ./bblast; # TODO remove
  shortcut-scripts = import ./scripts;

  # To update the Haskell dependencies, change the cabal file and re-run
  # `cabal2nix .. > shortcut-haskell.nix`. Any customizations to that file
  # should be added below instead so they don't get overridden.
  shortcut-haskell = haskellPackages.callPackage ./shortcut.nix {} // {
    # override shortcut-cabal attributes here
  };

in stdenv.mkDerivation {
  name = "shortcut";
  buildInputs = [ shortcut-scripts bblast shortcut-haskell ];
  # inherit bblast;
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash

    source ${stdenv}/setup
    mkdir -p $out/bin

    cat << EOF > $out/bin/shortcut
    #!/usr/bin/env bash
    export PATH=${shortcut-haskell}/bin:\$PATH
    export PATH=${shortcut-scripts}/bin:\$PATH
    export PATH=${bblast}/bin:\$PATH
    shortcut \$@
    EOF

    chmod +x $out/bin/shortcut
  '';
}
