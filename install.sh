#/usr/bin/env bash

case "$(uname)" in
  Linux ) export CHANNEL=nixos-19.09 ;;
  Darwin) export CHANNEL=nixpkgs-19.09-darwin ;;
  *) (echo "Sorry, unsupported OS: $(uname). Manual install?"; exit 1) ;;
esac

echo "Installing the Nix package manager..."
which nix-env &> /dev/null
if [[ $? != 0 ]]; then
  curl https://nixos.org/nix/install | sh
  source $HOME/.nix-profile/etc/profile.d/nix.sh
fi
# the options thing works around: https://github.com/NixOS/nix/issues/2733
nix-channel --add https://nixos.org/channels/${CHANNEL}
nix-channel --options substituters "" --update

# TODO name the cache shortcut?
echo "Setting Nix to use the ShortCut package cache..."
nix-env -iA cachix -f https://cachix.org/api/v1/install
[[ "$(uname)" == Darwin ]] && sudo cachix use jefdaj || cachix use jefdaj

echo "Installing ShortCut from GitHub..."
archive='https://github.com/jefdaj/shortcut/files/3934119/shortcut-v0.9.1.tar.gz'
nix-env -i -f $archive
