{ stdenv, fetchgit, autoreconfHook, writeScript, patchelf }:

stdenv.mkDerivation {
  name = "fastme";
  src = fetchgit {
    url = "https://gite.lirmm.fr/atgc/FastME";
    rev = "f21e6ab7e05ec10203a6ed8c413f5cafeac06a55";
    sha256 = "0k0ar4hw1v4jpfj6l6xmihzmicdqgnvfkwgbf330wkdahbznb1cg";
  };

  # see: https://nixos.wiki/wiki/Packaging/Quirks_and_Caveats
  nativeBuildInputs = [ autoreconfHook ];

  # see: https://github.com/NixOS/nixpkgs/issues/18995#issuecomment-249829054
  hardeningDisable = [ "all" ];
}
