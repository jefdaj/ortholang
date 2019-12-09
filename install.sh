#/usr/bin/env bash

export LOG=shortcut.log

case "$(uname)" in
  Linux ) export CHANNEL=nixos-19.09 ;;
  Darwin) export CHANNEL=nixpkgs-19.09-darwin ;;
  *) echo "Sorry, unsupported OS: $(uname). Try manual download + nix-build?"; exit 1 ;;
esac

onfailure() {
  echo "Oh no, something went wrong. See ${LOG} for details."
  echo "And if you have time, please submit a bug report!"
}

onsuccess() {
  echo "Success! Try opening a new terminal and running: shortcut"
  echo "If you get 'command not found', add this line to your .profile or .bashrc and try again:"
  echo "source $HOME/.nix-profile/etc/profile.d/nix.sh"
}

(
  set -e

  echo -n "Installing the Nix package manager..."
  which nix-env &> /dev/null
  if [[ $? != 0 ]]; then
   (curl https://nixos.org/nix/install | sh) >> $LOG 2>&1
    source $HOME/.nix-profile/etc/profile.d/nix.sh
  fi
  echo " OK"
  echo -n "Updating Nix channels..."
  # the options thing works around: https://github.com/NixOS/nix/issues/2733
  nix-channel --add https://nixos.org/channels/${CHANNEL}
  nix-channel --options substituters "" --update >> $LOG 2>&1
  echo " OK"

  # TODO name the cache shortcut?
  echo -n "Adding the jefdaj Cachix cache..."
  which cachix &> /dev/null
  if [[ $? != 0 ]]; then
    nix-env -iA cachix -f https://cachix.org/api/v1/install >> $LOG 2>&1
  fi
  ([[ "$(uname)" == Darwin ]] && sudo cachix use jefdaj || cachix use jefdaj) >> $LOG 2>&1
  echo " OK"

  echo -n "Installing ShortCut from GitHub..."
  archive='https://github.com/jefdaj/shortcut/files/3934119/shortcut-v0.9.1.tar.gz'
  nix-env -i -f $archive >> $LOG 2>&1
  echo " OK"

  echo "Testing that everything works..."
  shortcut --test version
  shortcut --version
  echo
) && onsuccess || onfailure
