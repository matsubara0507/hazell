module Hazell.Env
    ( Env (..)
    ) where

data Env = Env
  { packageYamlPath :: FilePath
  , stackYamlPath :: FilePath
  , bazelProjectPath :: FilePath
  , bazelBuildPath :: FilePath
  }
