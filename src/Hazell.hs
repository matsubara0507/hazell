module Hazell
    ( generate
    ) where

import Bazel.Haskell.StackSnapshot (buildStackSnapshotRule)
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)
import qualified Hpack.Config as Hpack

generate :: FilePath -> IO ()
generate project = do
  let opts = Hpack.defaultDecodeOptions { Hpack.decodeOptionsTarget = project <> "/package.yaml" }
  result <- Hpack.readPackageConfig opts
  case result of
    Left e  -> fail e
    Right r -> printStackSnapshotRule (Hpack.decodeResultPackage r)

printStackSnapshotRule :: Hpack.Package -> IO ()
printStackSnapshotRule package = do
  putDoc $ pretty (buildStackSnapshotRule package "stack-snapshot.yaml")
  putStrLn ""
