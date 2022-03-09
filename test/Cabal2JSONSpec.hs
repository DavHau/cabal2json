{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cabal2JSONSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Cabal2JSON ()
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Types.Flag as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Types.PackageDescription as Cabal
import Distribution.Verbosity as Cabal
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  it "outputs the same json for the cabal2json cabal file" $
    goldenJSONValueFile
      "test_resources/json/cabal2json.cabal.json"
      (Cabal.readGenericPackageDescription Cabal.silent "cabal2json.cabal")

  schemaSpec @Cabal.Flag "flag"
  schemaSpec @Cabal.PackageDescription "package-description"
  schemaSpec @Cabal.GenericPackageDescription "generic-package-description"

schemaSpec :: forall a. HasCodec a => String -> Spec
schemaSpec name =
  it ("outputs the same yaml file for " <> name) $
    pureGoldenByteStringFile
      ("test_resources/schemas/" <> name <> ".txt")
      (renderColouredSchemaViaCodec @a)
