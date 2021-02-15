{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Bazel.Rule
  ( tests
  ) where

import Bazel.Rule
import Data.String.Here
import Helper (assertPrettyEqual)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Bazel.Rule"
  [ testGroup "isStringArg"
    [ testCase "example" $ do
        True  @=? (RuleArgString "hoge" `isStringArg` "hoge")
        False @=? (RuleArgString "fuga" `isStringArg` "hoge")
        False @=? (RuleArgBool True `isStringArg` "hoge")
        False @=? (RuleArgArray [] `isStringArg` "hoge")
    ]
  , testGroup "pretty (Pretty instance)" 
    [ testCase "Rule type" $ do
        example1 `assertPrettyEqual` Rule "hoge" "@hoge/def.bzl" [(Nothing, RuleArgString "hogege")]
        example2 `assertPrettyEqual` Rule "hoge" "@hoge/def.bzl" []
        example3 `assertPrettyEqual` Rule "hoge" "@hoge/def.bzl" [(Just "hogege", RuleArgString "hogege"), (Just "fugaga", RuleArgString "fugaga")]
    , testCase "RuleArg type (RuleArgString)" $
        "\"hoge\"" `assertPrettyEqual` RuleArgString "hoge"
    , testCase "RuleArg type (RuleArgBool)" $ do
        "True" `assertPrettyEqual` RuleArgBool True
        "False" `assertPrettyEqual` RuleArgBool False
    , testCase "RuleArg type (RuleArgArray)" $ do
        "[]" `assertPrettyEqual` RuleArgArray []
        "[True]" `assertPrettyEqual` RuleArgArray [RuleArgBool True]
        "[\n    True,\n    True,\n]" `assertPrettyEqual` RuleArgArray [RuleArgBool True, RuleArgBool True]
    , testCase "RuleArg type (RuleArgConst)" $
        "HO_GE" `assertPrettyEqual` RuleArgConst "HO_GE"
    , testCase "RuleArg type (RuleArgGlob)" $
        "glob([\"**\"])" `assertPrettyEqual` RuleArgGlob "**"
    ]
  ]

example1, example2, example3 :: String
example1 = filter (/= '\r') [here|
load(
    "@hoge/def.bzl",
    "hoge",
)

hoge("hogege")
|]

example2 = filter (/= '\r') [here|
load(
    "@hoge/def.bzl",
    "hoge",
)

hoge()
|]

example3 = filter (/= '\r') [here|
load(
    "@hoge/def.bzl",
    "hoge",
)

hoge(
    hogege = "hogege",
    fugaga = "fugaga",
)
|]