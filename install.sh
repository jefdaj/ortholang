#/usr/bin/env bash

export LOG=ortholang.log

case "$(uname)" in
  Linux ) export CHANNEL=nixos-19.09 ;;
  Darwin) export CHANNEL=nixpkgs-19.09-darwin ;;
  *) echo "Sorry, unsupported OS: $(uname). Try git clone + nix-build, or ask Jeff!"; exit 1 ;;
esac

onfailure() {
  echo "Oh no, something went wrong. See ${LOG} for details."
  echo "And if you have time, please submit a bug report!"
}

onsuccess() {
  echo "Success! Try opening a new terminal and running: ortholang"
  echo "If you get 'command not found', add this line to your .profile or .bashrc and try again:"
  echo "source $HOME/.nix-profile/etc/profile.d/nix.sh"
}

(
  set -e
  set -o pipefail

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

  echo -n "Adding the jefdaj Cachix cache..."
  which cachix &> /dev/null
  if [[ $? != 0 ]]; then
    nix-env -iA cachix -f https://cachix.org/api/v1/install >> $LOG 2>&1
  fi
  ([[ "$(uname)" == Darwin ]] && sudo cachix use jefdaj || cachix use jefdaj) >> $LOG 2>&1
  echo " OK"

  echo "Installing OrthoLang..."
  archive='https://ortholang.pmb.berkeley.edu/static/ortholang-v0.9.4.tar.gz'
  nix-env -i -f $archive 2>&1 | tee -a $LOG

  echo "Testing that everything works..."
  ortholang --test version
  ortholang --version
  echo
) && onsuccess || onfailure
