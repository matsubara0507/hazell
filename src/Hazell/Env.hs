module Hazell.Env
    ( Env (..)
    ) where

import           RIO
import           RIO.Process (HasProcessContext (..), ProcessContext)

data Env = Env
  { envLogFunc       :: !LogFunc
  , envProcessCtx    :: ProcessContext
  , packageYamlPath  :: FilePath
  , stackYamlPath    :: FilePath
  , bazelProjectPath :: FilePath
  , bazelBuildPath   :: FilePath
  , recReadCabals    :: Bool
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasProcessContext Env where
  processContextL = lens envProcessCtx (\x y -> x { envProcessCtx = y })
