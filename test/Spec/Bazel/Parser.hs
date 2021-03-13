{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Spec.Bazel.Parser
  ( tests
  ) where

import           Bazel.Build
import           Bazel.Parser
import           Bazel.Rule
import           Data.String.Here
import           Data.Text        (Text, pack)
import           Helper           (assertPrettyEqual)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec  (parseMaybe)

tests :: TestTree
tests = testGroup "Bazel.Parser"
  [ testGroup "parseBuildFile"
    [ testCase "example" $
        parseBuildFile example @?= Just
          [ BuildRule "workspace" [(Just "name", RuleArgString "sample")]
          , BuildNewline,BuildComment " Load the repository rule to download an http archive."
          , BuildRule "load" [(Nothing, RuleArgString "@bazel_tools//tools/build_defs/repo:http.bzl"), (Nothing, RuleArgString "http_archive")]
          , BuildNewline
          , BuildComment " Download rules_haskell and make it accessible as \"@rules_haskell\"."
          , BuildRule "http_archive"
              [ (Just "name", RuleArgString "rules_haskell")
              , (Just "strip_prefix", RuleArgString "rules_haskell-0.13")
              , (Just "urls", RuleArgArray [RuleArgString "https://github.com/tweag/rules_haskell/archive/v0.13.tar.gz"])
              , (Just "sha256", RuleArgString "b4e2c00da9bc6668fa0404275fecfdb31beb700abdba0e029e74cacc388d94d6")
              ]
          , BuildNewline
          , BuildRule "load" [(Nothing, RuleArgString "@rules_haskell//haskell:repositories.bzl"), (Nothing, RuleArgString "rules_haskell_dependencies")]
          , BuildNewline
          , BuildComment " Setup all Bazel dependencies required by rules_haskell."
          , BuildRule "rules_haskell_dependencies" []
          , BuildNewline
          , BuildRule "load" [(Nothing, RuleArgString "@rules_haskell//haskell:toolchain.bzl"), (Nothing, RuleArgString "rules_haskell_toolchains")]
          , BuildNewline
          , BuildComment " Download a GHC binary distribution from haskell.org and register it as a toolchain."
          , BuildRule "rules_haskell_toolchains" []
          , BuildNewline
          , BuildRule "load" [(Nothing, RuleArgString "@rules_haskell//haskell:cabal.bzl"), (Nothing, RuleArgString "stack_snapshot")]
          , BuildNewline
          , BuildRule "stack_snapshot"
              [ (Just "name", RuleArgString "stackage")
              , (Just "packages", RuleArgArray
                  [ RuleArgString "base"
                  , RuleArgString "containers"
                  , RuleArgString "filepath"
                  , RuleArgString "hpack"
                  , RuleArgString "megaparsec"
                  , RuleArgString "prettyprinter"
                  , RuleArgString "text"
                  ])
              , (Just "local_snapshot", RuleArgString "//:stack-snapshot.yaml")
              ]
          ]
    ]
  , testGroup "buildCommentParser"
    [ testCase "success" $ do
        parseMaybe buildCommentParser "# abc\n" @?= Just (BuildComment " abc")
        parseMaybe buildCommentParser "#abc\n" @?= Just (BuildComment "abc")
        parseMaybe buildCommentParser "#\n" @?= Just (BuildComment "")
    , testCase "failure" $ do
        parseMaybe buildCommentParser "# abc" @?= Nothing
        parseMaybe buildCommentParser "abc\n" @?= Nothing
    ]
  , testGroup "buildNewlineParser"
    [ testCase "success" $
        parseMaybe buildNewlineParser "\n" @?= Just BuildNewline
    , testCase "failure" $
        parseMaybe buildNewlineParser "abc\n" @?= Nothing
    ]
  , testGroup "buildRuleArgArrayParser"
    [ testCase "success" $ do
        parseMaybe buildRuleArgArrayParser "[]" @?= Just (RuleArgArray [])
        parseMaybe buildRuleArgArrayParser "[\n]" @?= Just (RuleArgArray [])
        parseMaybe buildRuleArgArrayParser "[True]" @?= Just (RuleArgArray [RuleArgBool True])
        parseMaybe buildRuleArgArrayParser "[True,True]" @?= Just (RuleArgArray [RuleArgBool True, RuleArgBool True])
        parseMaybe buildRuleArgArrayParser "[True,True,]" @?= Just (RuleArgArray [RuleArgBool True, RuleArgBool True])
        parseMaybe buildRuleArgArrayParser arrayExample1 @?= Just (RuleArgArray [RuleArgBool True, RuleArgBool True])
        parseMaybe buildRuleArgArrayParser arrayExample2 @?= Just (RuleArgArray [RuleArgBool True, RuleArgBool True])
    , testCase "failure" $ do
        parseMaybe buildRuleArgArrayParser "[" @?= Nothing
        parseMaybe buildRuleArgArrayParser failArrayExample @?= Nothing
    ]
  , testGroup "buildRuleArgBoolParser"
    [ testCase "success" $ do
        parseMaybe buildRuleArgBoolParser "True" @?= Just (RuleArgBool True)
        parseMaybe buildRuleArgBoolParser "False" @?= Just (RuleArgBool False)
    , testCase "failure" $
        parseMaybe buildRuleArgBoolParser "Trua" @?= Nothing
    ]
  , testGroup "buildRuleArgStringParser"
    [ testCase "success" $ do
        parseMaybe buildRuleArgStringParser "\"ab c\"" @?= Just (RuleArgString "ab c")
        parseMaybe buildRuleArgStringParser "\"あいう\"" @?= Just (RuleArgString "あいう")
    , testCase "failure" $ do
        parseMaybe buildRuleArgStringParser "\"\"" @?= Nothing
        parseMaybe buildRuleArgStringParser "\"" @?= Nothing
    ]
  , testGroup "buildRuleArgGlobParser"
    [ testCase "success" $
        parseMaybe buildRuleArgGlobParser "glob([\"src/**/*.hs\"])" @?= Just (RuleArgGlob "src/**/*.hs")
    , testCase "failure" $ do
        parseMaybe buildRuleArgGlobParser "glob([])" @?= Nothing
        parseMaybe buildRuleArgGlobParser "glob([\"\"])" @?= Nothing
        parseMaybe buildRuleArgGlobParser "glob(['src/**/*.hs'])" @?= Nothing
    ]
  , testGroup "buildRuleArgConstParser"
    [ testCase "success" $ do
        parseMaybe buildRuleArgConstParser "HOGE_GE" @?= Just (RuleArgConst "HOGE_GE")
        parseMaybe buildRuleArgConstParser "abc123_" @?= Just (RuleArgConst "abc123_")
    , testCase "failure" $
        parseMaybe buildRuleArgConstParser "ab c" @?= Nothing
    ]
  ]

arrayExample1, arrayExample2, failArrayExample :: Text
arrayExample1 = pack $ filter (/= '\r') [here|
[
    True,
    True
]
|]
arrayExample2 = pack $ filter (/= '\r') [here|
[
    True,
    True,
]
|]
failArrayExample = pack $ filter (/= '\r') [here|
[
    True
    True
]
|]

example :: Text
example = pack $ filter (/= '\r') [here|
workspace(name = "sample")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.13",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.13.tar.gz"],
    sha256 = "b4e2c00da9bc6668fa0404275fecfdb31beb700abdba0e029e74cacc388d94d6",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains()

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "containers",
        "filepath",
        "hpack",
        "megaparsec",
        "prettyprinter",
        "text",
    ],
    local_snapshot = "//:stack-snapshot.yaml",
)
|]
