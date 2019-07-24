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
    for script in $src/*; do
      base="$(basename "$script")"
      [[ "$base" == default.nix ]] && continue
      [[ "$base" == result ]] && continue
      dest="$out/bin/$base"
      install -m755 $script $dest
      wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
}
