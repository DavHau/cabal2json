{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cabal2JSONSpec (spec) where

import Autodocodec
import Autodocodec.Yaml
import Cabal2JSON ()
import qualified Distribution.Make as Cabal
import Distribution.ModuleName as Cabal
import Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.SPDX as SPDX
import Distribution.Types.Benchmark as Cabal
import Distribution.Types.BenchmarkInterface as Cabal
import Distribution.Types.BenchmarkType as Cabal
import Distribution.Types.BuildInfo as Cabal
import Distribution.Types.BuildType as Cabal
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
import Distribution.Types.PkgconfigVersion as Cabal
import Distribution.Types.PkgconfigVersionRange as Cabal
import Distribution.Types.SourceRepo as Cabal
import Distribution.Types.TestSuite as Cabal
import Distribution.Types.TestSuiteInterface as Cabal
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Types.VersionRange as Cabal
import Distribution.Utils.ShortText as Cabal
import Distribution.Verbosity as Cabal
import Language.Haskell.Extension as Cabal
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  it "outputs the same json for the cabal2json cabal file" $
    goldenJSONValueFile
      "test_resources/json/cabal2json.cabal.json"
      (Cabal.readGenericPackageDescription Cabal.silent "cabal2json.cabal")

  schemaSpec @Cabal.GenericPackageDescription "generic-package-description"
  schemaSpec @Cabal.PackageDescription "package-description"
  schemaSpec @Cabal.PackageIdentifier "package-identified"
  schemaSpec @Cabal.BenchmarkType "benchmark-type"
  schemaSpec @Cabal.BenchmarkInterface "benchmark-interface"
  schemaSpec @Cabal.Benchmark "benchmark"
  schemaSpec @Cabal.BuildInfo "build-info"
  schemaSpec @Cabal.BuildType "build-type"
  schemaSpec @Cabal.Executable "executable"
  schemaSpec @Cabal.ExecutableScope "executable-scope"
  schemaSpec @Cabal.Extension "extension"
  schemaSpec @Cabal.Flag "flag"
  schemaSpec @Cabal.FlagName "flag-name"
  schemaSpec @Cabal.ForeignLibType "foreign-lib-type"
  schemaSpec @Cabal.ForeignLibOption "foreign-lib-option"
  schemaSpec @Cabal.ForeignLib "foreign-lib"
  schemaSpec @Cabal.KnownExtension "KnownExtension"
  schemaSpec @Cabal.Language "language"
  schemaSpec @Cabal.Library "library"
  schemaSpec @Cabal.LibraryName "library-name"
  schemaSpec @Cabal.LibraryVisibility "library-visibility"
  schemaSpec @Cabal.License "license"
  schemaSpec @Cabal.ModuleName "module-name"
  schemaSpec @Cabal.ModuleReexport "module-reexport"
  schemaSpec @Cabal.PackageIdentifier "package-identifier"
  schemaSpec @Cabal.PkgconfigVersion "pkgconfig-version"
  schemaSpec @Cabal.PkgconfigVersionRange "pkgconfig-version-range"
  schemaSpec @Cabal.ShortText "short-text"
  schemaSpec @Cabal.SourceRepo "source-repo"
  schemaSpec @Cabal.TestSuiteInterface "test-suite-interface"
  schemaSpec @Cabal.TestSuite "test-suite"
  schemaSpec @Cabal.UnqualComponentName "unqual-component-name"
  schemaSpec @Cabal.Version "version"
  schemaSpec @Cabal.VersionRange "version-range"
  schemaSpec @SPDX.License "spdx-license"
  schemaSpec @SPDX.LicenseExpression "spdx-license-expression"
  schemaSpec @SPDX.SimpleLicenseExpression "spdx-simple-license-expression"
  schemaSpec @SPDX.LicenseExceptionId "spdx-license-exception-id"
  schemaSpec @SPDX.LicenseRef "spdx-license-ref"
  schemaSpec @SPDX.LicenseId "spdx-license-id"

schemaSpec :: forall a. HasCodec a => String -> Spec
schemaSpec name =
  it ("outputs the same yaml file for " <> name) $
    pureGoldenByteStringFile
      ("test_resources/schemas/" <> name <> ".txt")
      (renderColouredSchemaViaCodec @a)
