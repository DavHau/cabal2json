{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cabal2JSONSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Cabal2JSON ()
import Distribution.ModuleName as Cabal
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Types.Benchmark as Cabal
import Distribution.Types.BenchmarkInterface as Cabal
import Distribution.Types.BenchmarkType as Cabal
import Distribution.Types.BuildInfo as Cabal
import Distribution.Types.Executable as Cabal
import Distribution.Types.ExecutableScope as Cabal
import Distribution.Types.Flag as Cabal
import Distribution.Types.ForeignLib as Cabal
import Distribution.Types.ForeignLibOption as Cabal
import Distribution.Types.ForeignLibType as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Types.Library as Cabal
import Distribution.Types.LibraryName as Cabal
import Distribution.Types.LibraryVisibility as Cabal
import Distribution.Types.ModuleReexport as Cabal
import Distribution.Types.PackageDescription as Cabal
import Distribution.Types.TestSuite as Cabal
import Distribution.Types.TestSuiteInterface as Cabal
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Verbosity as Cabal
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  it "outputs the same json for the cabal2json cabal file" $
    goldenJSONValueFile
      "test_resources/json/cabal2json.cabal.json"
      (Cabal.readGenericPackageDescription Cabal.silent "cabal2json.cabal")

  schemaSpec @Cabal.UnqualComponentName "unqual-component-name"
  schemaSpec @Cabal.BuildInfo "build-info"
  schemaSpec @Cabal.ModuleName "module-name"
  schemaSpec @Cabal.ModuleReexport "module-reexport"
  schemaSpec @Cabal.LibraryName "library-name"
  schemaSpec @Cabal.LibraryVisibility "library-visibility"
  schemaSpec @Cabal.Library "library"
  schemaSpec @Cabal.ForeignLibType "foreign-lib-type"
  schemaSpec @Cabal.ForeignLibOption "foreign-lib-option"
  schemaSpec @Cabal.ForeignLib "foreign-lib"
  schemaSpec @Cabal.BenchmarkType "benchmark-type"
  schemaSpec @Cabal.BenchmarkInterface "benchmark-interface"
  schemaSpec @Cabal.Benchmark "benchmark"
  schemaSpec @Cabal.TestSuiteInterface "test-suite-interface"
  schemaSpec @Cabal.TestSuite "test-suite"
  schemaSpec @Cabal.ExecutableScope "executable-scope"
  schemaSpec @Cabal.Executable "executable"
  schemaSpec @Cabal.Flag "flag"
  schemaSpec @Cabal.PackageDescription "package-description"
  schemaSpec @Cabal.GenericPackageDescription "generic-package-description"

schemaSpec :: forall a. HasCodec a => String -> Spec
schemaSpec name =
  it ("outputs the same yaml file for " <> name) $
    pureGoldenByteStringFile
      ("test_resources/schemas/" <> name <> ".txt")
      (renderColouredSchemaViaCodec @a)
