# Update sources

```
bash $ nix-shell -p niv nix cacert
nix-shell $ niv update nixpkgs -o nixos -r nixpkgs -b release-20.09
```

# Work on the interpreter

This builds the Haskell dependencies an extra time with Stack, but seems to
result in a better edit -> compile -> test loop compared to pure Nix.

```
nix-shell --run 'stack repl; exit'
```
