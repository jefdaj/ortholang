with import ./nixpkgs;

let shortcut = import ./default.nix;

in pkgs.singularity-tools.buildImage {
  name = "shortcut-${shortcut.version}";
  contents = [
    coreutils
    binutils
    util-linux # for flock in berkeley wrapper script
    shortcut
  ];

  # This is for the temporary build image; final output will be smaller
  diskSize = 10240;

  runScript = "#!${shortcut.env}\nexec bash \"\$@\"";

  # These should be absolute paths with the leading / removed.
  # Consult your HPC admin/support for help determining what to bind,
  # or build the image with none first and look for errors like this when run:
  # WARNING: Non existent bind point (file) in container: '/etc/localtime'
  extraBindDirs = [
    "clusterfs/rosalind/users"
    "global/home/users"
    "global/scratch"
  ];
  extraBindFiles = [
    "etc/hosts"
    "etc/localtime"
    "bin/flock"
    "bin/srun"
    "bin/scancel"
    "bin/squeue"
  ];
}
