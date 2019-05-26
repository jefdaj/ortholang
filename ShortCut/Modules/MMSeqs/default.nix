# TODO pass proper arguments
# TODO shells for working on individual scripts (split by language?)

with import ../../../nixpkgs;

let
  runDepends = [ mmseqs2 ];

in stdenv.mkDerivation {
  name = "shortcut-mmseqs";
  src = ./.;
  inherit runDepends;
  buildInputs = [ makeWrapper ] ++ runDepends;
  builder = writeScript "builder.sh" ''
    #!/usr/bin/env bash
    source ${stdenv}/setup
    mkdir -p $out/bin
    for script in $src/*.sh; do
      dest="$out/bin/$(basename "$script")"
      install -m755 $script $dest
      wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
}
