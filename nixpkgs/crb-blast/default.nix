# Based on:
#   http://nixos.org/nixpkgs/manual/#sec-language-ruby
#   http://blog.arkency.com/2016/04/packaging-ruby-programs-in-nixos

{ stdenv, lib, bundlerEnv, ruby, makeWrapper, ncbi-blast }:

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
  # TODO prefix path instead of setting the whole thing?
  installPhase = ''
    mkdir -p $out/bin
    makeWrapper ${env}/bin/crb-blast $out/bin/crb-blast \
      --set PATH "${ncbi-blast}/bin:$PATH"
  '';
}
