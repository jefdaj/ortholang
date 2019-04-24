with import ./nixpkgs;

let shortcut = import ./default.nix;

in pkgs.singularity-tools.buildImage {
  name = "shortcut-${shortcut.version}";
  contents = [
    # TODO which of these are actually needed on berkeley hpc?
    coreutils
    binutils
    utillinux
    shortcut
    openssh
    openmpi
    slurm
  ];

  # This is for the temporary build image; final output will be smaller
  diskSize = 10240;

  runScript = "#!${shortcut.env}\nexec bash \"\$@\"";

  # These should be absolute paths with the leading / removed.
  # Consult your HPC admin/support for help determining what to bind,
  # or build the image with none first and look for errors like this when run:
  # WARNING: Non existent bind point (file) in container: '/etc/localtime'
  # TODO should they all be done as files?
  extraBindDirs = [
    "clusterfs/rosalind/users" # TODO remove?

    "global/home/users"
    "global/scratch"
  ];
  extraBindFiles = [
    # bind points expected by berkeley hpc config
    "etc/hosts"
    "etc/localtime"
  ];
}
