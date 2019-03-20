with import ./nixpkgs;

let shortcut = import ./default.nix;

in pkgs.singularity-tools.buildImage {
  name = "shortcut-${shortcut.version}";
  contents = [ binutils shortcut ];

  # Seems to work better if I don't mess with this. Instead, run shortcut
  # (or any of its runtime dependencies) from bash inside the container.
  # runScript = "#!${stdenv.shell}\nexec /bin/sh";

  diskSize = 10240;
  # TODO is there a way to have these auto-bind when running the image too?
  # these should be absolute paths but missing the leading slash:
  bindDirs = [
    "global/home/users"
    "global/scratch"
  ];
}
