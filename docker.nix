with import ./nix;

# TODO can the docker image also be made into a Snap package?

let
  environment = import ./environment.nix;
  modules     = (import ./modules.nix).modules;
  ortholang   = import ./release.nix;

in pkgs.dockerTools.buildImage {
  name = "ortholang";
  tag = "${ortholang.version}";
  created = "now";

  # For the temporary build image. Currently ~7.5GB are required.
  # You might also need to set TMPDIR somewhere with enough space.
  diskSize = 10240;

  contents = lib.lists.unique
    (builtins.concatLists (map (m: m.extraRunDeps) modules)
    ++ environment
    ++ [ ortholang ]);

  runAsRoot = ''
    #!${pkgs.runtimeShell}
    mkdir -p /.ortholang
    mkdir -p /workdir
  '';

  config = {
    # TODO workdir is redundant here?
    Cmd = [ "/bin/ortholang" "--tmpdir" "/.ortholang" "--workdir" "/workdir" ];
    WorkingDir = "/workdir";
    Volumes = {
      "/.ortholang" = {};
      "/workdir"   = {};
    };
  };
}
