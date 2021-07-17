module Main where

import           Paths_hazell          (version)

import           Data.Maybe            (fromMaybe, listToMaybe)
import           Data.Version          (Version)
import qualified Data.Version          as Version
import qualified Hazell
import qualified Hazell.Env            as Hazell
import           System.Console.GetOpt
import           System.Environment    (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    (o, n, [])    -> cmd o n
    (_, _, err:_) -> mapM_ putStrLn [err, usage]
  where
    opts =
      [ Option ['V'] ["version"] (NoArg Version) "show version"
      , Option ['h'] ["help"] (NoArg Help) "show usage"
      , filePathOption "package-yaml" PackageYamlPath (Hazell.packageYamlPath defaultEnv) "package.yaml"
      , filePathOption "stack-yaml" StackYamlPath (Hazell.stackYamlPath defaultEnv) "stack.yaml"
      , filePathOption' "build" BazelBuildPath (Hazell.bazelBuildPath defaultEnv) "PATH for BUILD.bazel from bazel project root (default is BUILD.bazel)"
      , Option ['r'] ["recursive"] (NoArg Recursive) "Read all dependent cabal files to build files for bazel (e.g. for setup_deps)"
      ]
    cmd o n
      | Help `elem` o    = putStrLn usage
      | Version `elem` o = putStrLn $ Version.showVersion version
      | otherwise        = Hazell.generate $ toEnv (listToMaybe n) o
    usage = usageInfo "hazell [OPTIONS] [BAZEL_PROJECT_ROOT_PATH (default ./)]\n\nAvailable options:" opts

filePathOption
  :: String          -- ^ long option string
  -> (FilePath -> a) -- ^ argument descriptor (use `OptArg`)
  -> FilePath        -- ^ default file path
  -> FilePath        -- ^ file name for explanation of option
  -> OptDescr a
filePathOption optStr f defaultPath fileName =
  filePathOption' optStr f defaultPath $ "PATH for " ++ fileName ++ " (default is " ++ defaultPath ++ ")"

filePathOption'
  :: String          -- ^ long option string
  -> (FilePath -> a) -- ^ argument descriptor (use `OptArg`)
  -> FilePath        -- ^ default file path
  -> String          -- ^ explanation of option
  -> OptDescr a
filePathOption' optStr f defaultPath =
  Option [] [optStr] (OptArg (f . fromMaybe defaultPath) "PATH")


data Flag
  = Version
  | Help
  | PackageYamlPath FilePath
  | StackYamlPath FilePath
  | BazelBuildPath FilePath
  | Recursive
  deriving (Show, Eq)

defaultEnv :: Hazell.Env
defaultEnv = Hazell.Env
  { Hazell.packageYamlPath  = "./package.yaml"
  , Hazell.stackYamlPath    = "./stack.yaml"
  , Hazell.bazelProjectPath = "./"
  , Hazell.bazelBuildPath   = "BUILD.bazel"
  , Hazell.recReadCabals    = False
  }

toEnv :: Maybe FilePath -> [Flag] -> Hazell.Env
toEnv project =
  go defaultEnv { Hazell.bazelProjectPath = fromMaybe (Hazell.bazelProjectPath defaultEnv) project }
  where
    go env ((PackageYamlPath path):fs) = go (env { Hazell.packageYamlPath = path }) fs
    go env ((StackYamlPath path):fs)   = go (env { Hazell.stackYamlPath = path }) fs
    go env ((BazelBuildPath path):fs)  = go (env { Hazell.bazelBuildPath = path }) fs
    go env (Recursive:fs)              = go (env { Hazell.recReadCabals = True }) fs
    go env (_:fs)                      = go env fs
    go env []                          = env
