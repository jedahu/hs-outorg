{ mkDerivation, base, stdenv, text, lib }:
mkDerivation {
  pname = "hs-outorg";
  version = "0.1.0.0";
  src = lib.sourceFilesBySuffices ./. [".nix" ".cabal" ".hs" "LICENSE"];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base text ];
  homepage = "https://github.com/jedahu/hs-outorg";
  description = "Outorg outcommenting without Emacs";
  license = stdenv.lib.licenses.bsd3;
}
