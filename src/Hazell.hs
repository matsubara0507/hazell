module Hazell
    ( generate
    ) where

import Bazel.Haskell
import Hazell.Env
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)
import qualified Hpack.Config as Hpack

generate :: Env -> IO ()
generate env = do
  let opts = Hpack.defaultDecodeOptions { Hpack.decodeOptionsTarget = packageYamlPath env }
  result <- Hpack.readPackageConfig opts
  case result of
    Left e  -> fail e
    Right r -> do
      putStrLn "# for WORKSPACE"
      printStackSnapshotRule (Hpack.decodeResultPackage r)
      putStrLn ""
      putStrLn "# for BUILD.bazel"
      printHaskellLibrary (Hpack.decodeResultPackage r)

printStackSnapshotRule :: Hpack.Package -> IO ()
printStackSnapshotRule package = do
  putDoc $ pretty (buildStackSnapshotRule package "stack-snapshot.yaml")
  putStrLn ""

printHaskellLibrary :: Hpack.Package -> IO ()
printHaskellLibrary package = do
  putDoc $ pretty (buildHaskellLibraryRule package)
  putStrLn ""
