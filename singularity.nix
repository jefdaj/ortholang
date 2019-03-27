with import ./nixpkgs;

let shortcut = import ./default.nix;

in pkgs.singularity-tools.buildImage {
  name = "shortcut-${shortcut.version}";
  contents = [
    coreutils
    binutils
    utillinux # for flock in berkeley wrapper script
    shortcut
    slurm # will this enable me to interoperate with the hpc scheduler?
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
    "clusterfs/rosalind/users"

    # part of an attempt to install slurm in the container,
    # but it fails because the "slurm" user doesn't exist
    "etc/slurm"

    "global/home/users"
    "global/scratch"
  ];
  extraBindFiles = [
    # bind points expected by berkeley hpc config
    "etc/hosts"
    "etc/localtime"

    # maybe these are enough to convince slurm to go ahead? no :(
    # "etc/passwd"
    # "etc/group"
    # "etc/shadow"
  ];
}
