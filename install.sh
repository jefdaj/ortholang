#/usr/bin/env bash

which nix-env &> /dev/null
if [[ $? != 0 ]]; then
  echo "Installing the Nix package manager..."
  curl https://nixos.org/nix/install | sh
  source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

echo "Setting Nix to use the ShortCut package cache..."
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use jefdaj

echo "Installing ShortCut..."
branch='feature-installscript'
nix-env -i -f https://github.com/jefdaj/shortcut/archive/${branch}.tar.gz
