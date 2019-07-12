# TODO pass proper arguments
# TODO shells for working on individual scripts (split by language?)

with import ../../../nixpkgs;

let
  myR = pkgs.rWrapper.override { packages = with pkgs.rPackages; [
    dplyr
    ggplot2
    readr
    tidyr
    UpSetR
    VennDiagram
  ];};
  runDepends = [ myR ];

in stdenv.mkDerivation {
  name = "shortcut-setstable";
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
      dest="$out/bin/$base"
      wrapProgram $dest --prefix PATH : "${pkgs.lib.makeBinPath runDepends}"
    done
  '';
}
