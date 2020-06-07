#/usr/bin/env bash

export LOG=ortholang.log

case "$(uname)" in
  Linux ) export CHANNEL=nixos-20.03 ;;
  Darwin) export CHANNEL=nixpkgs-20.03-darwin ;;
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

install_nix() {
  case "$(uname)" in
    Linux )

      (curl https://nixos.org/nix/install | sh) >> $LOG 2>&1
			. $HOME/.nix-profile/etc/profile.d/nix.sh;;

    Darwin)

      macos_version=$(system_profiler SPSoftwareDataType | egrep -o 'macOS (.*) ' | cut -d'.' -f2);
      # echo "macos_version: $macos_version";
      if [[ $macos_version -ge 14 ]]; then
        (sh <(curl https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume) >> $LOG 2>&1
      else
        (curl https://nixos.org/nix/install | sh) >> $LOG 2>&1
      fi
			. $HOME/.nix-profile/etc/profile.d/nix.sh;;

    *)
      echo "Sorry, unsupported OS: $(uname). Try git clone + nix-build, or ask Jeff!";
      exit 1 ;;

  esac
}

(
  set -E
  # set -x

  echo -n "Installing the Nix package manager..."
  which nix-env &> /dev/null
  if [[ $? != 0 ]]; then
		install_nix
    . $HOME/.nix-profile/etc/profile.d/nix.sh
  fi
  which nix-env &> /dev/null
  if [[ $? != 0 ]]; then
    echo " ERROR $?. See $LOG for details."
    exit 1
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
  which cachix &> /dev/null
  if [[ $? != 0 ]]; then
    echo " ERROR $?. See $LOG for details."
    exit 1
  fi
  echo " OK"

  echo "Installing OrthoLang..."
  # TODO how to work around the git-annex dylib priority bug?
  export VERSION='0.9.5'
  archive="https://ortholang.pmb.berkeley.edu/static/ortholang-v${VERSION}.tar.gz"
  nix-env -i -f $archive 2>&1 | tee -a $LOG
  if [[ $? != 0 ]]; then
    echo "ERROR $?. See $LOG for details."
    exit 1
  fi

  echo "Testing that everything installed..."
  version="$(ortholang --version)"
  [[ "$version" =~ "$VERSION" ]] || exit 1
  ortholang --test version
  echo
)
[[ $? == 0 ]] && onsuccess && . $HOME/.nix-profile/etc/profile.d/nix.sh || onfailure
