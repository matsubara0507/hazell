{-# LANGUAGE OverloadedStrings #-}

module Bazel.Build
    ( BuildFile
    , BuildContent (..)
    , RuleName
    , isRule
    , isLoadRule
    , fromRule
    ) where

import Bazel.Rule
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Prettyprinter

type BuildFile = [BuildContent]

data BuildContent
  = BuildRule RuleName [(Maybe String, RuleArg)]
  | BuildComment Text
  | BuildNewline
  deriving (Show, Eq)

instance Pretty BuildContent where
  pretty (BuildRule name args)  = prettyMethodCall (unpack name) (map prettyMethodArg args)
  pretty (BuildComment comment) = "#" <> fromString (unpack comment)
  pretty BuildNewline           = ""

type RuleName = Text

isRule :: BuildContent -> Rule -> Bool
isRule (BuildRule name _) rule = name == pack (ruleName rule)
isRule _ _                     = False

isLoadRule :: BuildContent -> Rule -> Bool
isLoadRule (BuildRule "load" ((_, RuleArgString load) : args)) rule =
  load == ruleDef rule && any (isStringArg (ruleName rule) . snd) args
isLoadRule _ _ =
  False

fromRule :: Rule -> (BuildContent, BuildContent)
fromRule rule =
  ( BuildRule "load"
      [ (Nothing, RuleArgString $ ruleDef rule)
      , (Nothing, RuleArgString $ ruleName rule)
      ]
  , BuildRule (pack $ ruleName rule) $ ruleArgs rule
  )
