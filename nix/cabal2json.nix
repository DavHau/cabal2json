{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, Cabal, hashable, hpack, lib, pretty, pretty-show
, sydtest, sydtest-aeson, sydtest-discover, text
, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "cabal2json";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring Cabal hashable pretty pretty-show
    text unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec autodocodec-yaml base Cabal sydtest sydtest-aeson
    utf8-string
  ];
  testToolDepends = [ sydtest-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/NorfairKing/cabal2json#readme";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
