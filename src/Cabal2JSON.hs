module Cabal2JSON
  ( cabal2JSON,
  )
where

import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Verbosity as Cabal
import System.Environment

cabal2JSON :: IO ()
cabal2JSON = do
  arg : _ <- getArgs
  packageDescription <- Cabal.readGenericPackageDescription Cabal.deafening arg
  print packageDescription
