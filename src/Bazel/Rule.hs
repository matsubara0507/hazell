{-# LANGUAGE OverloadedStrings #-}

module Bazel.Rule
    ( Workspace (..)
    , Rule (..)
    , RuleArg (..)
    , prettyMethodCall
    , prettyMethodArg
    , isStringArg
    ) where

import           RIO
import qualified RIO.Map       as Map

import           Data.String   (fromString)
import           Prettyprinter

data Workspace = Workspace
  { workspaceName  :: String
  , workspaceRules :: [Rule]
  } deriving (Show, Eq)

data Rule = Rule
  { ruleName :: String
  , ruleDef  :: String
  , ruleArgs :: [(Maybe String, RuleArg)]
  } deriving (Show, Eq)

-- ToDo: Dict type
data RuleArg
  = RuleArgString String
  | RuleArgBool Bool
  | RuleArgArray [RuleArg]
  | RuleArgDict (Map String RuleArg)
  | RuleArgConst String
  | RuleArgGlob String
  | RuleArgAppend RuleArg RuleArg
  deriving (Show, Eq)

instance Pretty Rule where
  pretty (Rule name def args) = vsep [loadRule, "", callRule]
    where
      loadRule = prettyMethodCall "load" [fromString $ show def, fromString $ show name]
      callRule = prettyMethodCall name (map prettyMethodArg args)

instance Pretty RuleArg where
  pretty (RuleArgString str)   = fromString (show str)
  pretty (RuleArgBool True)    = "True"
  pretty (RuleArgBool False)   = "False"
  pretty (RuleArgArray [])     = "[]"
  pretty (RuleArgArray [arg])  = "[" <> pretty arg <> "]"
  pretty (RuleArgArray args)   = vsep [nest 4 $ vsep ("[" : map ((<> ",") . pretty) args), "]"]
  pretty (RuleArgDict dict)    = prettyDict dict
  pretty (RuleArgConst name)   = fromString name
  pretty (RuleArgGlob path)    = "glob([" <> fromString (show path) <> "])"
  pretty (RuleArgAppend a1 a2) = pretty a1 <> " + " <> pretty  a2

prettyDict :: Map String RuleArg -> Doc ann
prettyDict dict =
  if Map.null dict then
    "{}"
  else
    vsep [nest 4 $ vsep ("{" : prettyDict' dict), "}"]
  where
    prettyDict' = map (\(k, v) -> fromString (show k) <> ": " <> pretty v <> ",") . Map.toList

prettyMethodCall :: String -> [Doc ann] -> Doc ann
prettyMethodCall name []    = fromString name <> "()"
prettyMethodCall name [arg] = fromString name <> "(" <> arg <> ")"
prettyMethodCall name args  = vsep [nest 4 $ vsep (fromString name <> "(" : map (<> ",") args), ")"]

prettyMethodArg :: (Maybe String, RuleArg) -> Doc ann
prettyMethodArg (Nothing, val)  = pretty val
prettyMethodArg (Just key, val) = fromString key <+> "=" <+> pretty val

isStringArg :: RuleArg -> String -> Bool
isStringArg (RuleArgString str) str' = str == str'
isStringArg _ _                      = False
