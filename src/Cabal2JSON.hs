{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
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
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text.Encoding
import Distribution.Compiler as Cabal
import Distribution.License as Cabal
import Distribution.ModuleName as Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty as Pretty
import Distribution.SPDX (LicenseExpression (..), SimpleLicenseExpression (..))
import qualified Distribution.SPDX as SPDX
import Distribution.SPDX.License as SPDX
import Distribution.System
import Distribution.Types.CondTree as Cabal
import Distribution.Types.Dependency as Cabal
import Distribution.Types.ExeDependency as Cabal
import Distribution.Types.ExecutableScope as Cabal
import Distribution.Types.ForeignLib as Cabal
import Distribution.Types.ForeignLibOption as Cabal
import Distribution.Types.ForeignLibType as Cabal
import Distribution.Types.IncludeRenaming as Cabal
import Distribution.Types.LegacyExeDependency as Cabal
import Distribution.Types.LibraryVisibility as Cabal
import Distribution.Types.Mixin as Cabal
import Distribution.Types.PackageId as Cabal
import Distribution.Types.PackageName as Cabal
import Distribution.Types.PkgconfigDependency as Cabal
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Types.Version as Cabal
import Distribution.Types.VersionRange as Cabal
import Distribution.Utils.ShortText as Cabal
import Distribution.Verbosity as Cabal
import Language.Haskell.Extension
import System.Environment

cabal2JSON :: IO ()
cabal2JSON = do
  args <- getArgs
  case args of
    [] -> error "Please provide path to cabal file as CLI argument"
    arg -> do
      genericPackageDescription <- Cabal.readGenericPackageDescription Cabal.deafening (head arg)
      SB8.putStrLn $ LB.toStrict $ encodeJSONViaCodec genericPackageDescription

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

instance HasCodec VersionRange where
  codec = bimapCodec f g codec
    where
      f = \s -> case eitherParsec s of
        Left pe -> Left $ show pe
        Right a -> Right a
      g = prettyShow

instance HasCodec CompilerFlavor where
  codec =
    stringConstCodec
      [ (GHC, "GHC"),
        (GHCJS, "GHCJS"),
        (NHC, "NHC"),
        (YHC, "YHC"),
        (HBC, "HBC"),
        (Helium, "Helium"),
        (JHC, "JHC"),
        (LHC, "LHC"),
        (UHC, "UHC"),
        (Eta, "Eta")
      ]

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
        <*> requiredField' "custom-fields" .= customFieldsPD
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

instance HasCodec (PerCompilerFlavor [String]) where
  codec = dimapCodec (uncurry PerCompilerFlavor) (\(PerCompilerFlavor a b) -> (a, b)) codec

instance (HasCodec a) => HasCodec (a, a) where
  codec = dimapCodec (\(a : [b]) -> (a, b)) (\(a, b) -> [a, b]) codec

instance HasCodec (CompilerFlavor, VersionRange) where
  codec =
    object "(CompilerFlavor, VersionRange)" $
      (,)
        <$> requiredField' "compiler" .= fst
        <*> requiredField' "versionRange" .= snd

instance (HasCodec a) => HasCodec (a, a, a) where
  codec = dimapCodec (\(a : b : [c]) -> (a, b, c)) (\(a, b, c) -> [a, b, c]) codec

instance HasCodec (Maybe String, String) where
  codec =
    dimapCodec
      ( \case
          ("", s) -> (Nothing, s)
          (s1, s2) -> (Just s1, s2)
      )
      ( \case
          (Nothing, s) -> ("", s)
          (Just s1, s2) -> (s1, s2)
      )
      codec

instance HasCodec PackageName where
  codec = dimapCodec mkPackageName unPackageName codec

instance HasCodec PackageIdentifier where
  codec =
    object "PackgeIdentifier" $
      PackageIdentifier
        <$> requiredField' "name" .= pkgName
        <*> requiredField' "version" .= pkgVersion

instance HasCodec RepoKind where
  codec =
    object "RepoKind" $
      dimapCodec f g $
        eitherCodec (pure RepoHead) $
          eitherCodec (pure RepoThis) $
            requiredField' "repo-kind"
    where
      f = \case
        Left _ -> RepoHead
        Right (Left _) -> RepoThis
        Right (Right s) -> RepoKindUnknown s
      g = \case
        RepoHead -> Left ()
        RepoThis -> Right $ Left ()
        RepoKindUnknown s -> Right $ Right s

instance HasCodec RepoType where
  -- TODO add OtherRepoType
  codec =
    stringConstCodec
      [ (Darcs, "Darcs"),
        (Git, "Git"),
        (SVN, "SVN"),
        (CVS, "CVS"),
        (Mercurial, "Mercurial"),
        (GnuArch, "GnuArch"),
        (Bazaar, "Bazaar"),
        (Monotone, "Monotone")
      ]

instance HasCodec Dependency where
  codec =
    object "Dependency" $
      Dependency
        <$> requiredField' "package-name" .= (\(Dependency name _ _) -> name)
        <*> requiredField' "version-range" .= (\(Dependency _ version _) -> version)
        <*> requiredField' "library" .= (\(Dependency _ _ lib) -> lib)

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

instance HasCodec ShortText where
  codec = dimapCodec toShortText fromShortText codec

instance HasCodec Cabal.License where
  -- TODO add others
  codec =
    named "Cabal.License" $
      stringConstCodec
        [ (BSD2, "BSD2"),
          (BSD3, "BSD3"),
          (BSD4, "BSD4"),
          (MIT, "MIT"),
          (ISC, "ISC"),
          (PublicDomain, "PublicDomain"),
          (AllRightsReserved, "AllRightsReserved"),
          (UnspecifiedLicense, "UnspecifiedLicense"),
          (OtherLicense, "OtherLicense")
        ]

instance HasCodec SPDX.License where
  codec =
    named "SPDX.License" $
      object "spdx-license" $
        dimapCodec f g $
          eitherCodec (pure NONE) $
            requiredField' "license-expression"
    where
      f = \case
        Left _ -> NONE
        Right e -> License e
      g = \case
        NONE -> Left ("None" :: String)
        License e -> Right e

instance HasCodec SPDX.LicenseExpression where
  codec =
    named "LicenseExpression" $
      dimapCodec f g $
        eitherCodec (object "ELicense" $ (,) <$> requiredField' "expression" .= fst <*> requiredField' "exception-id" .= snd) $
          eitherCodec (object "EAnd" $ (,) <$> requiredField' "expression-1" .= fst <*> requiredField' "expression-2" .= snd) $
            object "EOr" $ (,) <$> requiredField' "expression-11" .= fst <*> requiredField' "expression-22" .= snd
    where
      f = \case
        Left (e, c) -> ELicense e c
        Right (Left (exp1, exp2)) -> EAnd exp1 exp2
        Right (Right (exp1, exp2)) -> EOr exp1 exp2
      g = \case
        ELicense e c -> Left (e, c)
        EAnd exp1 exp2 -> Right $ Left (exp1, exp2)
        EOr exp1 exp2 -> Right $ Right (exp1, exp2)

instance HasCodec SPDX.SimpleLicenseExpression where
  codec =
    object "SimpleLicenseExpression" $
      dimapCodec f g $
        eitherCodec (requiredField' "id") $
          eitherCodec (requiredField' "id-plus") $
            requiredField' "license-ref"
    where
      f = \case
        Left i -> ELicenseId i
        Right (Left i) -> ELicenseIdPlus i
        Right (Right ref) -> ELicenseRef ref
      g = \case
        ELicenseId i -> Left i
        ELicenseIdPlus i -> Right $ Left i
        ELicenseRef ref -> Right $ Right ref

instance HasCodec SPDX.LicenseId where
  codec = named "SPDX.LicenseId" shownBoundedEnumCodec

instance HasCodec SPDX.LicenseRef where
  codec = dimapCodec f g codec
    where
      f = uncurry SPDX.mkLicenseRef'
      g = \l -> (SPDX.licenseDocumentRef l, SPDX.licenseRef l)

instance HasCodec SPDX.LicenseExceptionId where
  codec = shownBoundedEnumCodec

instance HasCodec BuildType where
  codec = stringConstCodec [(Simple, "Simple"), (Configure, "Configure"), (Make, "Make"), (Custom, "Custom")]

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
  codec =
    named "ConditionalTree" $
      object "CondTree" $
        CondNode
          <$> requiredField' "condTreeData" .= condTreeData
          <*> requiredField' "condTreeConstraints" .= condTreeConstraints
          <*> requiredField' "condTreeComponents" .= condTreeComponents

instance (HasCodec a) => HasCodec (CondBranch ConfVar [Dependency] a) where
  codec =
    object "CondBranch" $
      CondBranch
        <$> requiredField' "condBranchCondition" .= condBranchCondition
        <*> requiredField' "condBranchIfTrue" .= condBranchIfTrue
        <*> optionalField' "condBranchIfFalse" .= condBranchIfFalse

instance HasCodec (Condition ConfVar) where
  codec =
    named "Condition" $
      dimapCodec f g $
        eitherCodec (object "var" $ requiredField' "v") $
          eitherCodec (object "lit" $ requiredField' "bool") $
            eitherCodec (object "cnot" $ requiredField' "cond") $
              eitherCodec (object "cor" $ (,) <$> requiredField' "cond1" .= fst <*> requiredField' "cond2" .= snd) $
                object "cand" $ (,) <$> requiredField' "cond12" .= fst <*> requiredField' "cond22" .= snd
    where
      f = \case
        Left c -> Var c
        Right (Left b) -> Lit b
        Right (Right (Left cond)) -> CNot cond
        Right (Right (Right (Left (cond1, cond2)))) -> COr cond1 cond2
        Right (Right (Right (Right (cond1, cond2)))) -> CAnd cond1 cond2
      g = \case
        Var c -> Left c
        Lit b -> Right $ Left b
        CNot cond -> Right $ Right $ Left cond
        COr cond1 cond2 -> Right $ Right $ Right $ Left (cond1, cond2)
        CAnd cond1 cond2 -> Right $ Right $ Right $ Right (cond1, cond2)

instance HasCodec ConfVar where
  codec =
    object "ConfVar" $
      dimapCodec f g $
        eitherCodec (requiredField' "os") $
          eitherCodec (requiredField' "arch") $
            eitherCodec (requiredField' "flag") $
              (,) <$> requiredField' "compiler" .= fst <*> requiredField' "version" .= snd
    where
      f = \case
        Left o -> OS o
        Right (Left a) -> Arch a
        Right (Right (Left name)) -> Flag name
        Right (Right (Right (compiler, version))) -> Impl compiler version
      g = \case
        OS o -> Left o
        Arch a -> Right $ Left a
        Flag name -> Right $ Right $ Left name
        Impl compiler version -> Right $ Right $ Right (compiler, version)

instance HasCodec Arch where
  -- TODO add OtherArch
  codec =
    stringConstCodec
      [ (I386, "I386"),
        (X86_64, "X86_64"),
        (PPC, "PPC"),
        (PPC64, "PPC64"),
        (Sparc, "Sparc"),
        (Arm, "Arm"),
        (AArch64, "AArch64"),
        (Mips, "Mips"),
        (SH, "SH"),
        (IA64, "IA64"),
        (S390, "S390"),
        (Alpha, "Alpha"),
        (Hppa, "Hppa"),
        (Rs6000, "Rs6000"),
        (M68k, "M68k"),
        (Vax, "Vax"),
        (JavaScript, "JavaScript")
      ]

instance HasCodec OS where
  -- TODO add OtherOS
  codec =
    stringConstCodec
      [ (Linux, "Linux"),
        (Windows, "Windows"),
        (OSX, "OSX"),
        (FreeBSD, "FreeBSD"),
        (OpenBSD, "OpenBSD"),
        (NetBSD, "NetBSD"),
        (DragonFly, "DragonFly"),
        (Solaris, "Solaris"),
        (AIX, "AIX"),
        (HPUX, "HPUX"),
        (IRIX, "IRIX"),
        (HaLVM, "HaLVM"),
        (Hurd, "Hurd"),
        (IOS, "IOS"),
        (Android, "Android"),
        (Ghcjs, "Ghcjs")
      ]

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
  codec =
    object "LibraryName" $
      dimapCodec f g $
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
  codec = stringConstCodec [(LibraryVisibilityPublic, "LibraryVisibilityPublic"), (LibraryVisibilityPrivate, "LibraryVisibilityPrivate")]

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
  codec = stringConstCodec [(ForeignLibNativeShared, "ForeignLibNativeShared"), (ForeignLibNativeStatic, "ForeignLibNativeStatic"), (ForeignLibTypeUnknown, "ForeignLibTypeUnknown")]

instance HasCodec ForeignLibOption where
  codec = literalTextValueCodec ForeignLibStandalone "ForeignLibStandalone"

instance HasCodec LibVersionInfo where
  codec = dimapCodec mkLibVersionInfo libVersionInfoCRA codec

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
  codec =
    stringConstCodec $
      NE.fromList
        [ (ExecutablePublic, "public"),
          (ExecutablePrivate, "private")
        ]

instance HasCodec TestSuite where
  codec =
    object "TestSuite" $
      TestSuite
        <$> requiredField' "name" .= testName
        <*> requiredField' "interface" .= testInterface
        <*> requiredField' "build-info" .= testBuildInfo

instance HasCodec TestSuiteInterface where
  codec =
    object "TestSuiteInterface" $
      dimapCodec f g $
        eitherCodec ((,) <$> requiredField' "version" .= fst <*> requiredField' "filepath" .= snd) $
          eitherCodec ((,) <$> requiredField' "version" .= fst <*> requiredField' "module-name" .= snd) $
            requiredField' "type"
    where
      f = \case
        Left (v, fp) -> TestSuiteExeV10 v fp
        Right (Left (v, mn)) -> TestSuiteLibV09 v mn
        Right (Right tt) -> TestSuiteUnsupported tt
      g = \case
        TestSuiteExeV10 v fp -> Left (v, fp)
        TestSuiteLibV09 v mn -> Right $ Left (v, mn)
        TestSuiteUnsupported tt -> Right $ Right tt

instance HasCodec TestType where
  codec =
    object "TestType" $
      dimapCodec f g $
        eitherCodec (requiredField' "version") $
          eitherCodec (requiredField' "version") $
            (,) <$> requiredField' "name" .= fst <*> requiredField' "version" .= snd
    where
      f = \case
        Left v -> TestTypeExe v
        Right (Left v) -> TestTypeLib v
        Right (Right (s, v)) -> TestTypeUnknown s v
      g = \case
        TestTypeExe v -> Left v
        TestTypeLib v -> Right $ Left v
        TestTypeUnknown s v -> Right $ Right (s, v)

instance HasCodec Benchmark where
  codec =
    object "Benchmark" $
      Benchmark
        <$> requiredField' "name" .= benchmarkName
        <*> requiredField' "interface" .= benchmarkInterface
        <*> requiredField' "build-info" .= benchmarkBuildInfo

instance HasCodec BenchmarkType where
  codec =
    object "BenchmarkType" $
      dimapCodec f g $
        eitherCodec (requiredField' "benchmark-type") $
          (,)
            <$> requiredField' "type" .= fst
            <*> requiredField' "version" .= snd
    where
      f = \case
        Left v -> BenchmarkTypeExe v
        Right (s, v) -> BenchmarkTypeUnknown s v
      g = \case
        BenchmarkTypeExe v -> Left v
        BenchmarkTypeUnknown s v -> Right (s, v)

instance HasCodec BenchmarkInterface where
  codec =
    dimapCodec f g $
      eitherCodec
        ( object "BenchmarkExeV10" $
            (,)
              <$> requiredField' "version" .= fst
              <*> requiredField' "filepath" .= snd
        )
        $ object "BenchmarkUnsupported" $ requiredField' "benchmark-type"
    where
      f = \case
        Left (version, file) -> BenchmarkExeV10 version file
        Right t -> BenchmarkUnsupported t
      g = \case
        BenchmarkExeV10 version file -> Left (version, file)
        BenchmarkUnsupported t -> Right t

instance HasCodec ModuleRenaming where
  codec =
    object "ModuleRenaming" $
      dimapCodec f g $
        eitherCodec (pure DefaultRenaming) $
          eitherCodec (requiredField' "name") (requiredField' "name")
    where
      f = \case
        Left _ -> DefaultRenaming
        Right (Left m) -> ModuleRenaming m
        Right (Right m) -> HidingRenaming m
      g = \case
        DefaultRenaming -> Left ()
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
  codec = bimapCodec f g codec
    where
      f = \s -> case eitherParsec s of
        Left pe -> Left $ show pe
        Right a -> Right a
      g = prettyShow

instance HasCodec PkgconfigVersion where
  codec =
    bimapCodec
      (Right . PkgconfigVersion . encodeUtf8)
      ( \(PkgconfigVersion v) -> case decodeUtf8' v of
          Left ex -> T.pack $ show ex
          Right txt -> txt
      )
      codec

instance HasCodec Language where
  codec =
    dimapCodec f g $
      eitherCodec (literalTextValueCodec Haskell98 "Haskell98") $
        eitherCodec
          (literalTextValueCodec Haskell2010 "Haskell2010")
          codec
    where
      f = \case
        Left l -> l
        Right (Left l) -> l
        Right (Right s) -> UnknownLanguage s
      g = \case
        Haskell98 -> Left Haskell98
        Haskell2010 -> Right $ Left Haskell2010
        UnknownLanguage s -> Right $ Right s

instance HasCodec Extension where
  codec =
    named "Extension" $
      object "Extension" $
        dimapCodec f g $
          eitherCodec (requiredField' "enable-extension") $
            eitherCodec
              (requiredField' "disable-extension")
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
  codec = named "KnownExtension" shownBoundedEnumCodec

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
