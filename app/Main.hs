module Main where

import Paths_hazell (version)

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Version (Version)
import qualified Data.Version as Version
import qualified Hazell
import qualified Hazell.Env as Hazell
import System.Environment (getArgs)
import System.Console.GetOpt

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    (o, n, [])      -> cmd o n
    (_, _, (err:_)) -> mapM_ putStrLn (err:usage:[])
  where
    opts =
      [ Option ['V'] ["version"] (NoArg Version) "show version"
      , Option ['h'] ["help"] (NoArg Help) "show usage"
      , filePathOption "package-yaml" PackageYamlPath (Hazell.packageYamlPath defaultEnv) "package.yaml"
      , filePathOption "stack-yaml" StackYamlPath (Hazell.stackYamlPath defaultEnv) "stack.yaml"
      ]
    cmd o n
      | any (== Help) o    = putStrLn usage
      | any (== Version) o = putStrLn $ Version.showVersion version
      | otherwise          = Hazell.generate $ toEnv o
    usage = usageInfo "hazell [OPTIONS] [PROJECT_PATH]\n\nAvailable options:" opts

filePathOption
  :: String          -- ^ long option string
  -> (FilePath -> a) -- ^ argument descriptor (use `OptArg`)
  -> FilePath        -- ^ default file path
  -> FilePath        -- ^ file name for explanation of option
  -> OptDescr a
filePathOption optStr f defaultPath fileName =
  Option [] [optStr] (OptArg (f . fromMaybe defaultPath) "PATH") $ "PATH for " ++ fileName ++ " (default is " ++ defaultPath ++ ")"

data Flag
  = Version
  | Help
  | PackageYamlPath FilePath
  | StackYamlPath FilePath
  deriving (Show, Eq)

defaultEnv :: Hazell.Env
defaultEnv = Hazell.Env
  { Hazell.packageYamlPath = "./package.yaml"
  , Hazell.stackYamlPath = "./stack.yaml"
  }

toEnv :: [Flag] -> Hazell.Env
toEnv = go defaultEnv
  where
    go env ((PackageYamlPath path):fs) = go (env { Hazell.packageYamlPath = path }) fs
    go env ((StackYamlPath path):fs)   = go (env { Hazell.stackYamlPath = path }) fs
    go env (_:fs)                      = go env fs
    go env []                          = env
