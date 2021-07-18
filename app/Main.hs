module Main where

import           Paths_hazell          (version)

import           RIO
import qualified RIO.ByteString as B
import RIO.Process (mkDefaultProcessContext)

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
      , filePathOption "package-yaml" PackageYamlPath "./package.yaml" "package.yaml"
      , filePathOption "stack-yaml" StackYamlPath "./stack.yaml" "stack.yaml"
      , filePathOption' "build" BazelBuildPath "BUILD.bazel" "PATH for BUILD.bazel from bazel project root (default is BUILD.bazel)"
      , Option ['r'] ["recursive"] (NoArg Recursive) "Read all dependent cabal files to build files for bazel (e.g. for setup_deps)"
      ]
    cmd o n
      | Help `elem` o    = putStrLn usage
      | Version `elem` o = putStrLn $ Version.showVersion version
      | otherwise        = runCmd (listToMaybe n) o
    usage = usageInfo "hazell [OPTIONS] [BAZEL_PROJECT_ROOT_PATH (default ./)]\n\nAvailable options:" opts
    putStrLn = B.putStr . fromString . (++ "\n")

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

buildEnv :: [Flag] -> Hazell.Env -> Hazell.Env
buildEnv flags defaultEnv = go defaultEnv flags
  where
    go env ((PackageYamlPath path):fs) = go (env { Hazell.packageYamlPath = path }) fs
    go env ((StackYamlPath path):fs)   = go (env { Hazell.stackYamlPath = path }) fs
    go env ((BazelBuildPath path):fs)  = go (env { Hazell.bazelBuildPath = path }) fs
    go env (Recursive:fs)              = go (env { Hazell.recReadCabals = True }) fs
    go env (_:fs)                      = go env fs
    go env []                          = env

runCmd :: Maybe FilePath -> [Flag] -> IO ()
runCmd project flags = do
  logOpt <- logOptionsHandle stdout False
  withLogFunc logOpt $ \lf -> do
    pctx <- mkDefaultProcessContext
    let defaultEnv = Hazell.Env
          { Hazell.envLogFunc       = lf
          , Hazell.envProcessCtx    = pctx
          , Hazell.packageYamlPath  = "./package.yaml"
          , Hazell.stackYamlPath    = "./stack.yaml"
          , Hazell.bazelProjectPath = fromMaybe "./" project
          , Hazell.bazelBuildPath   = "BUILD.bazel"
          , Hazell.recReadCabals    = False
          }
    runRIO (buildEnv flags defaultEnv) Hazell.generate
