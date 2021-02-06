{-# LANGUAGE RecordWildCards #-}

module Bazel.Haskell
    ( buildStackSnapshotRule
    , buildHaskellLibraryRule
    ) where

import Bazel.Rule (Rule(..), RuleArg(..))
import qualified Data.Map as Map
import qualified Hpack.Config as Hpack

buildStackSnapshotRule :: Hpack.Package -> String -> Rule
buildStackSnapshotRule package localSnapshot = Rule { .. }
  where
    ruleName = "stack_snapshot"
    ruleDef = "@rules_haskell//haskell:cabal.bzl"
    ruleArgs =
      [ (Just "name", RuleArgString "stackage")
      , (Just "packages", RuleArgArray $ map RuleArgString dependencies)
      , (Just "local_snapshot", RuleArgString $ "//:" <> localSnapshot)
      ]
    dependencies =
      filter (/= Hpack.packageName package) $ map fst (Hpack.packageDependencies package)

buildHaskellLibraryRule :: Hpack.Package -> Rule
buildHaskellLibraryRule package = Rule { .. }
  where
    ruleName = "haskell_library"
    ruleDef = "@rules_haskell//haskell:defs.bzl"
    ruleArgs = buildRuleArgs (Hpack.packageLibrary package)
    buildRuleArgs Nothing = []
    buildRuleArgs (Just lib) =
      [ (Just "name", RuleArgString $ Hpack.packageName package <> "-library")
      , (Just "src_strip_prefix", RuleArgString $ head (Hpack.sectionSourceDirs lib))
      , (Just "srcs", RuleArgGlob $ head (Hpack.sectionSourceDirs lib) <> "/**/*.hs")
      , (Just "deps", RuleArgArray $ map RuleArgString (dependencies lib))
      , (Just "compiler_flags", RuleArgConst "GHC_FLAGS")
      ]
    dependencies lib = Map.keys (Hpack.unDependencies $ Hpack.sectionDependencies lib)
