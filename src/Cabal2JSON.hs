{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cabal2JSON
  ( cabal2JSON,
  )
where

import Autodocodec
import Control.Arrow (first)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Pretty as Pretty
import Distribution.Types.Benchmark as Cabal
import Distribution.Types.BenchmarkInterface as Cabal
import Distribution.Types.BenchmarkType as Cabal
import Distribution.Types.BuildInfo as Cabal
import Distribution.Types.CondTree as Cabal
import Distribution.Types.ConfVar as Cabal
import Distribution.Types.Dependency as Cabal
import Distribution.Types.Executable as Cabal
import Distribution.Types.ExecutableScope as Cabal
import Distribution.Types.Flag as Cabal
import Distribution.Types.ForeignLib as Cabal
import Distribution.Types.ForeignLibOption as Cabal
import Distribution.Types.ForeignLibType as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Types.Library as Cabal
import Distribution.Types.PackageDescription as Cabal
import Distribution.Types.TestSuite as Cabal
import Distribution.Types.TestSuiteInterface as Cabal
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Types.Version as Cabal
import Distribution.Verbosity as Cabal
import System.Environment
import Text.Show.Pretty

cabal2JSON :: IO ()
cabal2JSON = do
  arg : _ <- getArgs
  cabalFile <- Cabal.readGenericPackageDescription Cabal.deafening arg
  pPrint cabalFile

instance HasCodec GenericPackageDescription where
  codec =
    object "GenericPackageDescription" $
      GenericPackageDescription
        <$> requiredField' "description" .= packageDescription
        <*> optionalFieldWithOmittedDefault' "package-flags" [] .= genPackageFlags
        <*> optionalField' "library" .= condLibrary
        <*> optionalFieldWithOmittedDefaultWith' "sublibraries" unqualComponentNameCodec [] .= condSubLibraries
        <*> optionalFieldWithOmittedDefaultWith' "foreign-libs" unqualComponentNameCodec [] .= condForeignLibs
        <*> optionalFieldWithOmittedDefaultWith' "executables" unqualComponentNameCodec [] .= condExecutables
        <*> optionalFieldWithOmittedDefaultWith' "test-suites" unqualComponentNameCodec [] .= condTestSuites
        <*> optionalFieldWithOmittedDefaultWith' "benchmarks" unqualComponentNameCodec [] .= condBenchmarks

deriving via (Autodocodec GenericPackageDescription) instance (FromJSON GenericPackageDescription)

deriving via (Autodocodec GenericPackageDescription) instance (ToJSON GenericPackageDescription)

unqualComponentNameCodec :: HasCodec b => JSONCodec [(UnqualComponentName, b)]
unqualComponentNameCodec = mapInListForCodec mkUnqualComponentName unUnqualComponentName

instance HasCodec UnqualComponentName where
  codec = dimapCodec mkUnqualComponentName unUnqualComponentName codec

mapInListForCodec ::
  HasCodec b =>
  (String -> a) ->
  (a -> String) ->
  JSONCodec [(a, b)]
mapInListForCodec fromText toText =
  dimapCodec
    (f . HM.toList)
    (HM.fromList . g)
    codec
  where
    f = map (first fromText)
    g = map (first toText)

instance Hashable UnqualComponentName where
  hashWithSalt s = hashWithSalt s . unUnqualComponentName

instance HasCodec PackageDescription where
  codec = undefined

instance HasCodec Flag where
  codec =
    object "Flag" $
      MkFlag
        <$> requiredField' "name" .= flagName
        <*> requiredField' "description" .= flagDescription
        <*> requiredField' "default" .= flagDefault
        <*> requiredField' "manual" .= flagManual

instance HasCodec FlagName where
  codec = dimapCodec mkFlagName unFlagName codec

instance HasCodec a => HasCodec (CondTree ConfVar [Dependency] a) where
  codec = undefined

instance HasCodec Library where
  codec = undefined

instance HasCodec ForeignLib where
  codec =
    object "ForeignLib" $
      ForeignLib
        <$> requiredField' "name" .= foreignLibName
        <*> requiredField' "type" .= foreignLibType
        <*> requiredField' "options" .= foreignLibOptions
        <*> requiredField' "build-info" .= foreignLibBuildInfo
        <*> requiredField' "version-info" .= foreignLibVersionInfo
        <*> requiredField' "linux-version" .= foreignLibVersionLinux
        <*> requiredField' "mod-def-files" .= foreignLibModDefFile

instance HasCodec ForeignLibType where
  codec = undefined

instance HasCodec ForeignLibOption where
  codec = undefined

instance HasCodec LibVersionInfo where
  codec = undefined

instance HasCodec Version where
  codec = undefined

instance HasCodec Executable where
  codec =
    object "Executable" $
      Executable
        <$> requiredField' "name" .= exeName
        <*> requiredField' "module-path" .= modulePath
        <*> requiredField' "scope" .= exeScope
        <*> requiredField' "build-info" .= buildInfo

instance HasCodec ExecutableScope where
  codec = undefined

instance HasCodec TestSuite where
  codec =
    object "TestSuite" $
      TestSuite
        <$> requiredField' "name" .= testName
        <*> requiredField' "interface" .= testInterface
        <*> requiredField' "build-info" .= testBuildInfo

instance HasCodec TestSuiteInterface where
  codec = undefined

instance HasCodec Benchmark where
  codec =
    object "Benchmark" $
      Benchmark
        <$> requiredField' "name" .= benchmarkName
        <*> requiredField' "interface" .= benchmarkInterface
        <*> requiredField' "build-info" .= benchmarkBuildInfo

instance HasCodec BenchmarkType where
  codec = dimapCodec undefined prettyShow codec

instance HasCodec BenchmarkInterface where
  codec = undefined

instance HasCodec BuildInfo where
  codec = undefined
