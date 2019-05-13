with import ../../../nixpkgs;

# this is probably the simplest module default.nix so it makes a good example

let
  runDepends = [ hmmer ];

in stdenv.mkDerivation {
  name = "shortcut-hmmer";
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
