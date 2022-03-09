{-# LANGUAGE TypeApplications #-}

module Cabal2JSONSpec (spec) where

import Autodocodec.Yaml
import Cabal2JSON ()
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Verbosity as Cabal
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  it "outputs the same json for the cabal2json cabal file" $
    goldenJSONValueFile
      "test_resources/cabal2json.cabal.json"
      (Cabal.readGenericPackageDescription Cabal.silent "cabal2json.cabal")
  it "outputs the same yaml schema" $
    pureGoldenByteStringFile
      "schema.txt"
      (renderColouredSchemaViaCodec @Cabal.GenericPackageDescription)
