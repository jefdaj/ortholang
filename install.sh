#/usr/bin/env bash

case "$(uname)" in
  Linux ); export NIX_PATH=nixpkgs=channel:nixpkgs-19.03;;
  Darwin); export NIX_PATH=nixpkgs=channel:nixpkgs-19.03-darwin;;
  *); (echo "Sorry, unsupported OS: $(uname). Manual install?"; exit 1)
esac

echo "Installing the Nix package manager..."
which nix-env &> /dev/null
if [[ $? != 0 ]]; then
  curl https://nixos.org/nix/install | sh
  source $HOME/.nix-profile/etc/profile.d/nix.sh
fi

# TODO name the cache shortcut?
echo "Setting Nix to use the ShortCut package cache..."
nix-env -iA cachix -f https://cachix.org/api/v1/install
[[ "$(uname)" == Darwin ]] && sudo cachix use jefdaj || cachix use jefdaj

echo "Installing ShortCut from GitHub..."
branch='master'
nix-env -i -f https://github.com/jefdaj/shortcut/archive/${branch}.tar.gz
