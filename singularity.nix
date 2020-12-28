# TODO this requires my fork of singularity right?

with import ./nix;

let ortholang = import ./release.nix;

in pkgs.singularity-tools.buildImage {
  name = "ortholang-${ortholang.version}";
  contents = [
    # TODO which of these are actually needed on berkeley hpc?
    coreutils
    binutils
    utillinux
    ortholang
    openssh
    openmpi
    slurm
  ];

  # This is for the temporary build image; final output will be smaller
  diskSize = 20480;

  # TODO launch ortholang directly rather than bash?
  runScript = "#!${ortholang.env}\nexec bash \"\$@\"";

  # These should be absolute paths with the leading / removed.
  # Consult your HPC admin/support for help determining what to bind,
  # or build the image with none first and look for errors like this when run:
  # WARNING: Non existent bind point (file) in container: '/etc/localtime'
  # TODO should they all be done as files?
  extraBindFiles = [
    # bind points expected by berkeley hpc config
    "etc/hosts"
    "etc/localtime"
  ];
  extraBindDirs = [
    "clusterfs/rosalind/users" # TODO remove?
    "global/home/users"
    "global/scratch"
  ];
}
