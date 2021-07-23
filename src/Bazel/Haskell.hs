{-# LANGUAGE RecordWildCards #-}

module Bazel.Haskell
    ( buildStackSnapshotRule
    , buildHaskellLibraryRule
    ) where

import           RIO
import qualified RIO.Map      as Map

import           Bazel.Cabal  (CabalPackage, HasCabalPackages (..))
import qualified Bazel.Cabal  as Cabal
import           Bazel.Rule   (Rule (..), RuleArg (..))
import qualified Hpack.Config as Hpack

buildStackSnapshotRule :: (HasCabalPackages env, MonadReader env m) => Hpack.Package -> String -> m Rule
buildStackSnapshotRule package localSnapshot = do
  setupDeps <- buildSetupDeps
  let ruleArgs = ruleArgs' ++ setupDeps
  pure $ Rule { .. }
  where
    ruleName = "stack_snapshot"
    ruleDef = "@rules_haskell//haskell:cabal.bzl"
    ruleArgs' =
      [ (Just "name", RuleArgString "stackage")
      , (Just "packages", RuleArgArray $ map RuleArgString dependencies)
      , (Just "local_snapshot", RuleArgString $ "//:" <> localSnapshot)
      ]
    dependencies =
      filter (/= Hpack.packageName package) $ map fst (Hpack.packageDependencies package)

buildSetupDeps :: (HasCabalPackages env, MonadReader env m) => m [(Maybe [Char], RuleArg)]
buildSetupDeps = do
  cabals <- asks (view cabalPackagesL)
  pure $ case cabals of
    Nothing ->
      []
    Just cs ->
      case [(Cabal.toPackageName c, RuleArgArray ds) | c <- cs, let ds = toSetupDepsArg c, not (null ds)] of
        [] ->
          []
        deps ->
          [(Just "setup_deps", RuleArgDict $ Map.fromList deps)]
  where
    toSetupDepsArg =
      fmap RuleArgString . filter (not . (`elem` ghcPkgs)) . Cabal.toSetupDeps

ghcPkgs :: [String]
ghcPkgs =
  [ "Cabal"
  , "array"
  , "base"
  , "bin-package-db"
  , "binary"
  , "bytestring"
  , "containers"
  , "directory"
  , "extensible-exceptions"
  , "ffi"
  , "filepath"
  , "ghc-prim"
  , "haskeline"
  , "haskell98"
  , "hpc"
  , "integer-gmp"
  , "mtl"
  , "old-locale"
  , "old-time"
  , "pretty"
  , "process"
  , "random"
  , "rts"
  , "syb"
  , "template-haskell"
  , "terminfo"
  , "time"
  , "unix"
  ,  "utf8-string"
  ]

-- ToDo: GHC_FLAGS
buildHaskellLibraryRule :: Hpack.Package -> Rule
buildHaskellLibraryRule package = Rule { .. }
  where
    ruleName = "haskell_library"
    ruleDef = "@rules_haskell//haskell:defs.bzl"
    ruleArgs = buildRuleArgs (Hpack.packageLibrary package)
    buildRuleArgs Nothing = []
    buildRuleArgs (Just lib) =
      [ (Just "name", RuleArgString $ Hpack.packageName package <> "-library")
      , (Just "src_strip_prefix", RuleArgString $ toSrcDir lib)
      , (Just "srcs", RuleArgGlob $ toSrcDir lib <> "/**/*.hs")
      , (Just "deps", RuleArgArray $ map RuleArgString (dependencies lib))
      , (Just "compiler_flags", RuleArgConst "GHC_FLAGS")
      ]
    dependencies lib =
      map ("@stackage//:" <>)$ Map.keys (Hpack.unDependencies $ Hpack.sectionDependencies lib)
    toSrcDir = fromMaybe "src" . listToMaybe . Hpack.sectionSourceDirs
