{ mkDerivation, base, bound, containers, deriving-compat, lens
, stdenv, unification
}:
mkDerivation {
  pname = "linearlambda";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bound containers deriving-compat lens unification
  ];
  license = stdenv.lib.licenses.bsd3;
}
