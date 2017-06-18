# Based on:
#   http://nixos.org/nixpkgs/manual/#sec-language-ruby
#   http://blog.arkency.com/2016/04/packaging-ruby-programs-in-nixos

# TODO get it to recognize and not attempt re-downloading blast+,
#      which is probably caused by this 32bit-related error:
# blastx: error while loading shared libraries: libidn.so.11: cannot open shared object file: No such file or directory

{ stdenv, lib, bundlerEnv, ruby, makeWrapper, ncbi-blast }:
# with import <nixpkgs> {};

let
  version = (import ./gemset.nix).crb-blast.version;
  env = bundlerEnv {
    inherit ruby version;
    name = "crb-blast-${version}-env";
    # expects Gemfile, Gemfile.lock and gemset.nix in the same directory
    gemdir = ./.;
    meta = with lib; {
      description = "Conditional Reciprocal Best Blast";
      homepage    = https://github.com/cboursnell/crb-blast;
      license     = with licenses; mit;
      maintainers = with maintainers; [ ];
      platforms   = platforms.unix;
    };
  };

in stdenv.mkDerivation {
  inherit env version;
  name         = "crb-blast-${version}";
  buildInputs  = [ makeWrapper ncbi-blast ];
  phases       = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    makeWrapper ${env}/bin/crb-blast $out/bin/crb-blast \
      --prefix PATH "${ncbi-blast}/bin"
  '';
}
