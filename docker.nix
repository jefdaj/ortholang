with import ./nixpkgs;

# TODO can the docker image also be made into a Snap package?

let
  shortcut = import ./default.nix;
  inherit (import ./dependencies.nix) modules runDepends;

in pkgs.dockerTools.buildImage {
  name = "shortcut";
  tag = "${shortcut.version}";
  created = "now";

  # For the temporary build image. Currently ~7.5GB are required.
  # You might also need to set TMPDIR somewhere with enough space.
  diskSize = 10240;

  contents = lib.lists.unique
    (builtins.concatLists (map (m: m.extraRunDeps) modules)
    ++ runDepends
    ++ [ shortcut ]);

  runAsRoot = ''
    #!${pkgs.runtimeShell}
    mkdir -p /.shortcut
    mkdir -p /workdir
  '';

  config = {
    # TODO workdir is redundant here?
    Cmd = [ "/bin/shortcut" "--tmpdir" "/.shortcut" "--workdir" "/workdir" ];
    WorkingDir = "/workdir";
    Volumes = {
      "/.shortcut" = {};
      "/workdir"   = {};
    };
  };
}
