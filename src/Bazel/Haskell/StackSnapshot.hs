{-# LANGUAGE RecordWildCards #-}

module Bazel.Haskell.StackSnapshot
    ( buildStackSnapshotRule
    ) where

import Bazel.Workspace (Rule(..), RuleArg(..))
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
