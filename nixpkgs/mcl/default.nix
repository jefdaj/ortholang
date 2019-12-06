{ stdenv, fetchurl }:

stdenv.mkDerivation {
  name = "mcl";
  src = fetchurl {
    # TODO use github instead?
    url = "http://http.debian.net/debian/pool/main/m/mcl/mcl_14-137.orig.tar.gz";
    sha256 = "15xlax3z31lsn62vlg94hkm75nm40q4679amnfg13jm8m2bnhy5m";
  };
}
