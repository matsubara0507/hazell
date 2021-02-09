module Main where

import Paths_hazell (version)

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Version (Version)
import qualified Data.Version as Version
import qualified Hazell
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
      ]
    cmd o n
      | any (== Help) o    = putStrLn usage
      | any (== Version) o = putStrLn $ Version.showVersion version
      | otherwise          = Hazell.generate (fromMaybe "." $ listToMaybe n)
    usage = usageInfo "hazell [OPTIONS] [PROJECT_PATH]\n\nAvailable options:" opts

data Flag
  = Version
  | Help
  deriving (Show, Eq)
