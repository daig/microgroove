{ mkDerivation, base, primitive, stdenv, vector, cabal}:
mkDerivation {
  pname = "microgroove";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base primitive vector ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/daig/microgroove";
  description = "Fast access extensible records";
  license = stdenv.lib.licenses.bsd3;
}
