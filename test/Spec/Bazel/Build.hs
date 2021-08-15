{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Spec.Bazel.Build
  ( tests
  ) where

import           RIO

import           Bazel.Build
import           Bazel.Rule
import           Data.String.Here
import           Helper           (assertPrettyEqual)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Bazel.Build"
  [ testGroup "isRule"
    [ testCase "example" $ do
        True  @=? (BuildRule "hoge" [] `isRule` Rule "hoge" "" [])
        False @=? (BuildRule "hoge" [] `isRule` Rule "fuga" "" [])
        False @=? (BuildComment "" `isRule` Rule "hoge" "" [])
        False @=? (BuildNewline `isRule` Rule "hoge" "" [])
    ]
  , testGroup "isLoadRule" $
    let
      loadRule =
        BuildRule "load" [(Nothing, RuleArgString "@hoge/def.bzl"), (Nothing, RuleArgString "hoge")]
    in
      [ testCase "examples" $ do
          True  @=? (loadRule `isLoadRule` Rule "hoge" "@hoge/def.bzl" [])
          False @=? (loadRule `isLoadRule` Rule "fuga" "@hoge/def.bzl" [])
          False @=? (loadRule `isLoadRule` Rule "hoge" "@hogege/def.bzl" [])
          False @=? (BuildRule "hoge" [] `isLoadRule` undefined)
      ]
  , testGroup "fromRule" $
    let
      rule =
        Rule "hoge" "@hoge/def.bzl" [(Just "hogege", RuleArgString "hogege")]
      loadHogeRule =
        BuildRule "load" [(Nothing, RuleArgString "@hoge/def.bzl"), (Nothing, RuleArgString "hoge")]
      hogeRule =
        BuildRule "hoge" [(Just "hogege", RuleArgString "hogege")]
    in
      [ testCase "example" $
          (loadHogeRule, hogeRule) @=? fromRule rule
      ]
   , testGroup "pretty (Pretty instance)"
    [ testCase "BuildContent type" $ do
        example1 `assertPrettyEqual` BuildRule "hoge" []
        example2 `assertPrettyEqual` BuildRule "hoge" [(Nothing, RuleArgString "hogege1")]
        example3 `assertPrettyEqual` BuildRule "hoge" [(Nothing, RuleArgString "hogege1"), (Just "hogege", RuleArgString "hogege2")]
        "# hoge" `assertPrettyEqual` BuildComment " hoge"
        "" `assertPrettyEqual` BuildNewline
    ]
  , testGroup "mergeRuleArgs" $
    let
      newRuleArgs = [(Just "key1", RuleArgBool True), (Just "key2", RuleArgBool True)]
    in
      [ testCase "context is not rule" $ do
          (BuildNewline `mergeRuleArgs` Rule "" "" newRuleArgs) @?= BuildNewline
          (BuildComment "" `mergeRuleArgs` Rule "" "" newRuleArgs) @?= BuildComment ""
      , testCase "context rule args is empty" $
          (BuildRule "abc" [] `mergeRuleArgs` Rule "" "" newRuleArgs) @?= BuildRule "abc" newRuleArgs
      , testCase "new rule args is empty" $
          (BuildRule "abc" [(Just "key1", RuleArgBool False)] `mergeRuleArgs` Rule "" "" []) @?= BuildRule "abc" [(Just "key1", RuleArgBool False)]
      , testCase "duplicated key1 arg" $
          (BuildRule "abc" [(Just "key1", RuleArgBool False)] `mergeRuleArgs` Rule "" "" newRuleArgs) @?= BuildRule "abc" newRuleArgs
      , testCase "different keys order" $
          (BuildRule "abc" [(Just "key2", RuleArgBool False), (Just "key1", RuleArgBool False)] `mergeRuleArgs` Rule "" "" newRuleArgs) @?= BuildRule "abc" [(Just "key2", RuleArgBool True), (Just "key1", RuleArgBool True)]
      ]
  ]


example1, example2, example3 :: String
example1 = filter (/= '\r') [here|
hoge()
|]
example2 = filter (/= '\r') [here|
hoge("hogege1")
|]
example3 = filter (/= '\r') [here|
hoge(
    "hogege1",
    hogege = "hogege2",
)
|]
