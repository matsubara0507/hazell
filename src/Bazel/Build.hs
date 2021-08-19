{-# LANGUAGE OverloadedStrings #-}

module Bazel.Build
    ( BuildFile
    , BuildContent (..)
    , RuleName
    , isRule
    , isLoadRule
    , fromRule
    , mergeRuleArgs
    ) where

import           RIO
import qualified RIO.List      as L
import qualified RIO.Map       as Map
import qualified RIO.Text      as Text

import           Bazel.Rule
import           Data.String   (fromString)
import           Prettyprinter

type BuildFile = [BuildContent]

data BuildContent
  = BuildRule RuleName [(Maybe String, RuleArg)]
  | BuildComment Text
  | BuildNewline
  deriving (Show, Eq)

instance Pretty BuildContent where
  pretty (BuildRule name args)  = prettyMethodCall (Text.unpack name) (map prettyMethodArg args)
  pretty (BuildComment comment) = "#" <> fromString (Text.unpack comment)
  pretty BuildNewline           = ""

type RuleName = Text

isRule :: BuildContent -> Rule -> Bool
isRule (BuildRule name _) rule = name == Text.pack (ruleName rule)
isRule _ _                     = False

isLoadRule :: BuildContent -> Rule -> Bool
isLoadRule (BuildRule "load" ((_, RuleArgString load) : args)) rule =
  load == ruleDef rule && any ((`isStringArg` ruleName rule) . snd) args
isLoadRule _ _ =
  False

fromRule :: Rule -> (BuildContent, BuildContent)
fromRule rule =
  ( BuildRule "load"
      [ (Nothing, RuleArgString $ ruleDef rule)
      , (Nothing, RuleArgString $ ruleName rule)
      ]
  , BuildRule (Text.pack $ ruleName rule) $ ruleArgs rule
  )

mergeRuleArgs :: BuildContent -> Rule -> BuildContent
mergeRuleArgs (BuildRule name args) rule = BuildRule name (replaced <> rest')
  where
    replace newArgs (key, old) =
      case Map.lookup key newArgs of
        Just new -> (Map.delete key newArgs, (key, new))
        Nothing  -> (newArgs, (key, old))
    (rest, replaced) = L.mapAccumL replace (Map.fromList $ ruleArgs rule) args
    rest' = filter (\(k, _) -> Map.member k rest) $ ruleArgs rule
mergeRuleArgs content _ = content
