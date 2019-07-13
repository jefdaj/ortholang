# TODO pass proper arguments
# TODO shells for working on individual scripts (split by language?)

with import ../../../nixpkgs;

let
  # TODO any reason to use python 3?
  myPython = pkgs.python27Packages.python.withPackages (ps: with ps; [
    # biopython
    ipython # TODO remove
  ]);
  runDepends = [ myPython ];

in stdenv.mkDerivation {
  name = "shortcut-greencut";
  src = ./.;
  inherit runDepends;
  buildInputs = [ makeWrapper ] ++ runDepends;
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    for script in $src/*; do
      base="$(basename "$script")"
      [[ "$base" == default.nix ]] && continue
      dest="$out/bin/$base"
      install -m755 $script $dest
      wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
}
