# Based on:
#   http://nixos.org/nixpkgs/manual/#sec-language-ruby
#   http://blog.arkency.com/2016/04/packaging-ruby-programs-in-nixos

# Note requires ncbi-blast version 2.2.29
# TODO hardcode that here?
# TODO why doesn't it pick up cat from coreutils? (has to be added to main nix-shell)

{ stdenv, fetchurl, lib, bundlerEnv, ruby, makeWrapper, ncbi-blast, coreutils }:
# with import ../.;

# TODO coreutils needs to be used in env rather than overall derivation?
let
  version = (import ./gemset.nix).crb-blast.version;
  env = bundlerEnv {
    inherit ruby version;
    name = "crb-blast-${version}-env";
    gemdir = ./.;
    meta = with lib; {
      description = "Conditional Reciprocal Best Blast";
      homepage    = https://github.com/cboursnell/crb-blast;
      license     = with licenses; mit;
      maintainers = with maintainers; [ ];
      platforms   = platforms.unix;
    };
  };

# TODO why doesn't cat get in the runtime path? and proper ncbi-blast?
in stdenv.mkDerivation {
  inherit env version;
  name         = "crb-blast-${version}";
  buildInputs  = [ makeWrapper ncbi-blast coreutils ];
  phases       = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    makeWrapper ${env}/bin/crb-blast $out/bin/crb-blast \
      --prefix PATH : "${ncbi-blast}/bin" \
      --prefix PATH : "${coreutils}/bin"
  '';
}
