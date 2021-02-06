module Bazel.Build
    ( BuildFile
    , BuildContent (..)
    , RuleName
    ) where

import Bazel.Rule (RuleArg)
import Data.Text (Text)

type BuildFile = [BuildContent]

data BuildContent
  = BuildRule RuleName [(Maybe String, RuleArg)]
  | BuildComment Text
  | BuildNewline
  deriving (Show, Eq)

type RuleName = Text
