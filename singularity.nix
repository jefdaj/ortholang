with import ./nixpkgs;

let shortcut = import ./default.nix;

in pkgs.singularity-tools.buildImage {
  name = "shortcut-${shortcut.version}";
  contents = [ shortcut ];
  runScript = "${shortcut}/bin/shortcut";
  diskSize = 10240; # TODO can this be smaller?
}
