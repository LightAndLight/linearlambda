{ mkDerivation, base, deriving-compat, equivalence, fetchgit, lens
, mtl, stdenv
}:
mkDerivation {
  pname = "unification";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/LightAndLight/unification";
    sha256 = "04v378wwi3l4jw7jskz723slfqbf1bs7d4jj8cj3l5a69lz0pjjq";
    rev = "27c561b689daaead309b1595530044cd3c45d32f";
  };
  libraryHaskellDepends = [
    base deriving-compat equivalence lens mtl
  ];
  license = stdenv.lib.licenses.bsd3;
}
