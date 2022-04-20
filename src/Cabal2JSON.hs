{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Cabal2JSON
  ( cabal2JSON,
  )
where

import Autodocodec
import Control.Arrow (first)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Distribution.Compiler as Cabal
import Distribution.License as Cabal
import Distribution.ModuleName as Cabal
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Pretty as Pretty
import Distribution.SPDX.License as SPDX
import Distribution.Types.Benchmark as Cabal
import Distribution.Types.BenchmarkInterface as Cabal
import Distribution.Types.BenchmarkType as Cabal
import Distribution.Types.BuildInfo as Cabal
import Distribution.Types.BuildType as Cabal
import Distribution.Types.CondTree as Cabal
import Distribution.Types.ConfVar as Cabal
import Distribution.Types.Dependency as Cabal
import Distribution.Types.Executable as Cabal
import Distribution.Types.ExecutableScope as Cabal
import Distribution.Types.ExeDependency as Cabal
import Distribution.Types.Flag as Cabal
import Distribution.Types.ForeignLib as Cabal
import Distribution.Types.ForeignLibOption as Cabal
import Distribution.Types.ForeignLibType as Cabal
import Distribution.Types.GenericPackageDescription as Cabal
import Distribution.Types.IncludeRenaming as Cabal
import Distribution.Types.LegacyExeDependency as Cabal
import Distribution.Types.Library as Cabal
import Distribution.Types.LibraryName as Cabal
import Distribution.Types.LibraryVisibility as Cabal
import Distribution.Types.Mixin as Cabal
import Distribution.Types.ModuleReexport as Cabal
import Distribution.Types.ModuleRenaming as Cabal
import Distribution.Types.PackageDescription as Cabal
import Distribution.Types.PackageId as Cabal
import Distribution.Types.PackageName as Cabal
import Distribution.Types.PkgconfigDependency as Cabal
import Distribution.Types.SetupBuildInfo as Cabal
import Distribution.Types.SourceRepo as Cabal
import Distribution.Types.TestSuite as Cabal
import Distribution.Types.TestSuiteInterface as Cabal
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Types.Version as Cabal
import Distribution.Types.VersionRange as Cabal
import Distribution.Utils.ShortText as Cabal
import Distribution.Verbosity as Cabal
import Language.Haskell.Extension
import System.Environment
import Text.Show.Pretty (pPrint)
import Data.List.NonEmpty (fromList)
import Distribution.Types.PkgconfigName (PkgconfigName, mkPkgconfigName, unPkgconfigName)
import Distribution.Types.PkgconfigVersionRange
import Data.Word (Word8)
import Distribution.Types.PkgconfigVersion
import Data.ByteString.UTF8 as BSU
import Text.PrettyPrint
import Distribution.Parsec (Parsec(parsec), CabalParsing (parsecHaskellString), eitherParsec)
import Distribution.Compat.Lens (_1)

cabal2JSON :: IO ()
cabal2JSON = do
  arg <- getArgs
  case arg of
    [] -> error "Please provide path to cabal file as CLI argument"
    arg -> Cabal.readGenericPackageDescription Cabal.deafening (head arg) >>= pPrint

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
  codec =
    object "PackageDescription" $
      PackageDescription
        <$> requiredField' "spec-version" .= specVersionRaw
        <*> requiredField' "package" .= package
        <*> requiredField' "license" .= licenseRaw
        <*> requiredField' "license-files" .= licenseFiles
        <*> requiredField' "copyright" .= copyright
        <*> requiredField' "maintainer" .= maintainer
        <*> requiredField' "author" .= author
        <*> requiredField' "stability" .= stability
        <*> requiredField' "tested-with" .= testedWith
        <*> requiredField' "homepage" .= homepage
        <*> requiredField' "pkg-url" .= pkgUrl
        <*> requiredField' "bug-reports" .= bugReports
        <*> requiredField' "source-repos" .= sourceRepos
        <*> requiredField' "synopsis" .= synopsis
        <*> requiredField' "description" .= description
        <*> requiredField' "category" .= category
        <*> requiredFieldWith' "custom-fields" undefined .= customFieldsPD
        <*> requiredField' "build-type-raw" .= buildTypeRaw
        <*> requiredField' "custom-setup" .= setupBuildInfo
        <*> requiredField' "library" .= library
        <*> requiredField' "sublibraries" .= subLibraries
        <*> requiredField' "executables" .= executables
        <*> requiredField' "foreign-libs" .= foreignLibs
        <*> requiredField' "test-suites" .= testSuites
        <*> requiredField' "benchmarks" .= benchmarks
        <*> requiredField' "data-files" .= dataFiles
        <*> requiredField' "data-dir" .= dataDir
        <*> requiredField' "extra-source-files" .= extraSrcFiles
        <*> requiredField' "extra-tmp-files" .= extraTmpFiles
        <*> requiredField' "extra-doc-files" .= extraDocFiles

instance HasCodec (CompilerFlavor, VersionRange) where
  -- TODO: ?
  codec = undefined

instance HasCodec (PerCompilerFlavor [String]) where
  codec = dimapCodec (uncurry PerCompilerFlavor) (\(PerCompilerFlavor a b) -> (a,b)) codec

instance (HasCodec a) => HasCodec (a, a) where
  codec = dimapCodec (\(a:[b]) -> (a, b)) (\(a, b) -> [a, b]) codec

instance HasCodec PackageName where
  codec = dimapCodec mkPackageName unPackageName codec

instance HasCodec PackageIdentifier where
  codec =
    object "PackgeIdentifier" $
      PackageIdentifier
        <$> requiredField' "name" .= pkgName
        <*> requiredField' "version" .= pkgVersion

instance HasCodec RepoKind where
  -- TODO: ?
  codec = undefined

instance HasCodec RepoType where
  -- TODO: ?
  codec = undefined

instance HasCodec Dependency where
  codec = object "Dependency" $
      Dependency
        <$> requiredField' "package-name" .= (\(Dependency name _ _ ) -> name)
        <*> requiredField' "version-range" .= (\(Dependency _ version _ ) -> version)
        <*> requiredField' "library" .= (\(Dependency _ _ lib ) -> lib)

instance HasCodec SourceRepo where
  codec =
    object "SourceRepo" $
      SourceRepo
       <$> requiredField' "kind" .= repoKind
       <*> requiredField' "type" .= repoType
       <*> requiredField' "location" .= repoLocation
       <*> requiredField' "module" .= repoModule
       <*> requiredField' "branch" .= repoBranch
       <*> requiredField' "tag" .= repoTag
       <*> requiredField' "subdir" .= repoSubdir

instance HasCodec VersionRange where
  codec = dimapCodec f g codec
    where
      f = \s -> case eitherParsec s of
        Left pe -> error "Invalid version string"
        Right a -> a
      g = prettyShow

instance HasCodec ShortText where
  -- TODO: ?
  codec = undefined

instance HasCodec Cabal.License where
  -- TODO: ?
  codec = undefined

instance HasCodec SPDX.License where
  -- TODO: ?
  codec = undefined

instance HasCodec BuildType where
  -- TODO: ?
  codec = undefined

instance HasCodec SetupBuildInfo where
  codec =
    object "SetupBuildInfo" $
      SetupBuildInfo
        <$> requiredField' "setup-depends" .= setupDepends
        -- TODO: not needed
        <*> requiredField' "__defaultSetupDepends" .= defaultSetupDepends


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
  -- TODO: ?
  codec = undefined

instance HasCodec Library where
  codec =
    object "Library" $
      Library
        <$> requiredField' "name" .= libName
        <*> requiredField' "exposed-modules" .= exposedModules
        <*> requiredField' "re-exported-modules" .= reexportedModules
        <*> requiredField' "signatures" .= signatures
        <*> requiredField' "exposed" .= libExposed
        <*> requiredField' "visibility" .= libVisibility
        <*> requiredField' "build-info" .= libBuildInfo

instance HasCodec LibraryName where
  codec = object "LibraryName" $ dimapCodec f g $
      eitherCodec (pure LMainLibName) $
        requiredField' "sub-lib-name"
    where
      f = \case
        Left _ -> LMainLibName
        Right s -> LSubLibName s
      g = \case
        LMainLibName -> Left ()
        LSubLibName s -> Right s

instance HasCodec ModuleName where
  codec = dimapCodec fromComponents components codec

instance HasCodec ModuleReexport where
  codec =
    object "ModuleReexport" $
      ModuleReexport
        <$> requiredField' "moduleReexportOriginalPackage" .= moduleReexportOriginalPackage
        <*> requiredField' "moduleReexportOriginalName" .= moduleReexportOriginalName
        <*> requiredField' "moduleReexportName" .= moduleReexportName

instance HasCodec LibraryVisibility where
  -- TODO: ?
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
  -- TODO: ?
  codec = undefined

instance HasCodec ForeignLibOption where
  -- TODO: ?
  codec = undefined

instance HasCodec LibVersionInfo where
  -- TODO: ?
  codec = undefined

instance HasCodec Version where
  codec = dimapCodec mkVersion versionNumbers codec

instance HasCodec Executable where
  codec =
    object "Executable" $
      Executable
        <$> requiredField' "name" .= exeName
        <*> requiredField' "module-path" .= modulePath
        <*> requiredField' "scope" .= exeScope
        <*> requiredField' "build-info" .= buildInfo

instance HasCodec ExecutableScope where
  codec = stringConstCodec $ fromList [(ExecutablePublic, "public"), (ExecutablePrivate, "private")]

instance HasCodec TestSuite where
  codec =
    object "TestSuite" $
      TestSuite
        <$> requiredField' "name" .= testName
        <*> requiredField' "interface" .= testInterface
        <*> requiredField' "build-info" .= testBuildInfo

instance HasCodec TestSuiteInterface where
  -- TODO: ?
  codec = undefined

instance HasCodec Benchmark where
  codec =
    object "Benchmark" $
      Benchmark
        <$> requiredField' "name" .= benchmarkName
        <*> requiredField' "interface" .= benchmarkInterface
        <*> requiredField' "build-info" .= benchmarkBuildInfo

instance HasCodec BenchmarkType where
  codec = object "BenchmarkType" $ dimapCodec f g $
    eitherCodec (requiredField' "benchmark-type") $
      (,)
        <$> requiredField' "type" .= fst
        <*> requiredField' "version" .= snd
    where
      f = \case
        Left v -> BenchmarkTypeExe v
        Right (s,v) -> BenchmarkTypeUnknown s v
      g = \case
        BenchmarkTypeExe v -> Left v
        BenchmarkTypeUnknown s v -> Right (s,v)

instance HasCodec BenchmarkInterface where
  codec = dimapCodec f g $ eitherCodec (object "BenchmarkExeV10" $ (,)
        <$> requiredField' "version" .= fst
        <*> requiredField' "filepath" .= snd) $
          object "BenchmarkUnsupported" $ requiredField' "benchmark-type"
    where
      f = \case
        Left (version, file) -> BenchmarkExeV10 version file
        Right t -> BenchmarkUnsupported t
      g = \case
        BenchmarkExeV10 version file -> Left (version, file)
        BenchmarkUnsupported t -> Right t

instance HasCodec ModuleRenaming where
  codec = object "ModuleRenaming" $ dimapCodec f g $
      eitherCodec (pure DefaultRenaming) $
        eitherCodec (requiredField' "name") (requiredField' "name")
    where
      f = \case
        Left _ -> DefaultRenaming
        Right (Left m) -> ModuleRenaming m
        Right (Right m) -> HidingRenaming m
      g = \case
        DefaultRenaming  -> Left ()
        ModuleRenaming m -> Right $ Left m
        HidingRenaming m -> Right $ Right m

instance HasCodec IncludeRenaming where
  codec =
    object "IncludeRenaming" $
      IncludeRenaming
        <$> requiredField' "includeProvidesRn" .= includeProvidesRn
        <*> requiredField' "includeRequiresRn" .= includeRequiresRn

instance HasCodec Mixin where
  codec =
    object "Mixin" $
      Mixin
        <$> requiredField' "package-name" .= mixinPackageName
        <*> requiredField' "include-renaming" .= mixinIncludeRenaming

instance HasCodec LegacyExeDependency where
  codec =
    object "LegacyExeDependency" $
      LegacyExeDependency
        <$> requiredField' "package-name" .= (\(LegacyExeDependency s _) -> s)
        <*> requiredField' "include-renaming" .= (\(LegacyExeDependency _ vr) -> vr)

instance HasCodec ExeDependency where
  codec =
    object "ExeDependency" $
      ExeDependency
        <$> requiredField' "package-name" .= (\(ExeDependency a _ _) -> a)
        <*> requiredField' "executable-component-name" .= (\(ExeDependency _ b _) -> b)
        <*> requiredField' "version-range" .= (\(ExeDependency _ _ c) -> c)

instance HasCodec PkgconfigDependency where
  codec =
    object "PkgconfigDependency" $
      PkgconfigDependency
        <$> requiredField' "pkgconfigName" .= (\(PkgconfigDependency name _) -> name)
        <*> requiredField' "pkgconfigVersionRange" .= (\(PkgconfigDependency _ version) -> version)

instance HasCodec PkgconfigName where
  codec = dimapCodec mkPkgconfigName unPkgconfigName codec

instance HasCodec PkgconfigVersionRange where
  codec = dimapCodec f g codec
    where
      f = \s -> case eitherParsec s of
        Left pe -> error "Invalid version string"
        Right a -> a
      g = prettyShow

instance HasCodec PkgconfigVersion where
  codec = dimapCodec (PkgconfigVersion . BSU.fromString) (\(PkgconfigVersion v) -> show v) codec

instance HasCodec Language where
  codec = object "Language" $ dimapCodec f g $
      eitherCodec (pure Haskell98) $
        eitherCodec (pure Haskell2010) (requiredField' "name")
    where
      f = \case
        Left _ -> Haskell98
        Right (Left _) -> Haskell2010
        Right (Right s) -> UnknownLanguage s
      g = \case
        Haskell98 -> Left ()
        Haskell2010 -> Right $ Left ()
        UnknownLanguage s -> Right $ Right s

instance HasCodec Extension where
  codec = object "Extension" $ dimapCodec f g $
      eitherCodec (requiredField' "enable-extension") $
        eitherCodec (requiredField' "disable-extension")
          (requiredField' "unknown-extension")
    where
      f = \case
        Left e -> EnableExtension e
        Right (Left e) -> DisableExtension e
        Right (Right s) -> UnknownExtension s
      g = \case
        EnableExtension e -> Left e
        DisableExtension e -> Right $ Left e
        UnknownExtension e -> Right $ Right e

instance HasCodec KnownExtension where
  codec = shownBoundedEnumCodec

instance HasCodec BuildInfo where
  codec =
    object "BuildInfo" $
      BuildInfo
        <$> requiredField' "buildable" .= buildable
        <*> requiredField' "buildTools" .= buildTools
        <*> requiredField' "buildToolDepends" .= buildToolDepends
        <*> requiredField' "cppOptions" .= cppOptions
        <*> requiredField' "asmOptions" .= asmOptions
        <*> requiredField' "cmmOptions" .= cmmOptions
        <*> requiredField' "ccOptions" .= ccOptions
        <*> requiredField' "cxxOptions" .= cxxOptions
        <*> requiredField' "ldOptions" .= ldOptions
        <*> requiredField' "pkgconfigDepends" .= pkgconfigDepends
        <*> requiredField' "frameworks" .= frameworks
        <*> requiredField' "extraFrameworkDirs" .= extraFrameworkDirs
        <*> requiredField' "asmSources" .= asmSources
        <*> requiredField' "cmmSources" .= cmmSources
        <*> requiredField' "cSources" .= cSources
        <*> requiredField' "cxxSources" .= cxxSources
        <*> requiredField' "jsSources" .= jsSources
        <*> requiredField' "hsSourceDirs" .= hsSourceDirs
        <*> requiredField' "otherModules" .= otherModules
        <*> requiredField' "virtualModules" .= virtualModules
        <*> requiredField' "autogenModules" .= autogenModules
        <*> requiredField' "defaultLanguage" .= defaultLanguage
        <*> requiredField' "otherLanguages" .= otherLanguages
        <*> requiredField' "defaultExtensions" .= defaultExtensions
        <*> requiredField' "otherExtensions" .= otherExtensions
        <*> requiredField' "oldExtensions" .= oldExtensions
        <*> requiredField' "extraLibs" .= extraLibs
        <*> requiredField' "extraGHCiLibs" .= extraGHCiLibs
        <*> requiredField' "extraBundledLibs" .= extraBundledLibs
        <*> requiredField' "extraLibFlavours" .= extraLibFlavours
        <*> requiredField' "extraDynLibFlavours" .= extraDynLibFlavours
        <*> requiredField' "extraLibDirs" .= extraLibDirs
        <*> requiredField' "includeDirs" .= includeDirs
        <*> requiredField' "includes" .= includes
        <*> requiredField' "autogenIncludes" .= autogenIncludes
        <*> requiredField' "installIncludes" .= installIncludes
        <*> requiredField' "options" .= options
        <*> requiredField' "profOptions" .= profOptions
        <*> requiredField' "sharedOptions" .= sharedOptions
        <*> requiredField' "staticOptions" .= staticOptions
        <*> requiredField' "customFieldsBI" .= customFieldsBI
        <*> requiredField' "targetBuildDepends" .= targetBuildDepends
        <*> requiredField' "mixins" .= mixins
