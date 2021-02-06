{-# LANGUAGE OverloadedStrings #-}

module Bazel.Rule
    ( Workspace (..)
    , Rule (..)
    , RuleArg (..)
    ) where

import Prettyprinter
import Data.String (fromString)

data Workspace = Workspace
  { workspaceName :: String
  , workspaceRules :: [Rule]
  } deriving (Show, Eq)

data Rule = Rule
  { ruleName :: String
  , ruleDef :: String
  , ruleArgs :: [(Maybe String, RuleArg)]
  } deriving (Show, Eq)

data RuleArg
  = RuleArgString String
  | RuleArgBool Bool
  | RuleArgArray [RuleArg]
  | RuleArgExp String
  deriving (Show, Eq)

instance Pretty Rule where
  pretty (Rule name def args) = vsep [loadRule, "", callRule]
    where
      loadRule = prettyMethodCall "load" [fromString $ show def, fromString $ show name]
      callRule = prettyMethodCall name (map prettyMethodArg args)

instance Pretty RuleArg where
  pretty (RuleArgString str) = fromString (show str)
  pretty (RuleArgBool True)  = "True"
  pretty (RuleArgBool False) = "False"
  pretty (RuleArgArray args) = vsep [nest 4 $ vsep ("[" : map ((<> ",") . pretty) args), "]"]
  pretty (RuleArgExp expr)   = fromString expr

prettyMethodCall :: String -> [Doc ann] -> Doc ann
prettyMethodCall name args =
  vsep [nest 4 $ vsep (fromString name <> "(" : map (<> ",") args), ")"]

prettyMethodArg :: (Maybe String, RuleArg) -> Doc ann
prettyMethodArg (Nothing, val)  = pretty val
prettyMethodArg (Just key, val) = fromString key <+> "=" <+> pretty val
