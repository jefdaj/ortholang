with import ./nixpkgs;

let shortcut = import ./../shortcut;

in pkgs.dockerTools.buildImage {
  name = "shortcut";
  tag = "${shortcut.version}";
  created = "now";
  contents = [ shortcut ];
  config.Cmd = [ "${shortcut}/bin/shortcut" ];
}
