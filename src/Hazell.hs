module Hazell
    ( generate
    ) where

import           Bazel.Build               (BuildContent (..), BuildFile,
                                            fromRule, isRule)
import           Bazel.Haskell
import qualified Bazel.Parser              as Bazel
import           Data.Functor              ((<&>))
import qualified Data.Text.IO              as Text
import           Hazell.Env
import qualified Hpack.Config              as Hpack
import           Prettyprinter             (defaultLayoutOptions, layoutPretty,
                                            pretty, vsep)
import           Prettyprinter.Render.Text (putDoc, renderStrict)
import           System.FilePath           ((</>))
import           Text.Megaparsec           (parse)

generate :: Env -> IO ()
generate env = do
  let opts = Hpack.defaultDecodeOptions { Hpack.decodeOptionsTarget = packageYamlPath env }
  result <- Hpack.readPackageConfig opts
  case result of
    Left e ->
      fail e
    Right r -> do
      let package = Hpack.decodeResultPackage r
      generateWorkspaceFile env package "stack-snapshot.yaml"
      generateBuildFile env package

generateWorkspaceFile :: Env -> Hpack.Package -> FilePath -> IO ()
generateWorkspaceFile env package stackSnapshot = do
  let path = bazelProjectPath env </> "WORKSPACE"
  ws <- Text.readFile path
  case parse Bazel.buildFileParser path ws of
    Left err ->
      fail $ show err
    Right w ->
      let ws' = replaceStackSnapshotRule package stackSnapshot w
          pws = vsep $ map pretty (ws' ++ [BuildNewline])
      in Text.writeFile path $ renderStrict (layoutPretty defaultLayoutOptions pws)

replaceStackSnapshotRule :: Hpack.Package -> FilePath -> BuildFile -> BuildFile
replaceStackSnapshotRule package stackSnapshotPath ws =
  if any (`isRule` stackSnapshotRule) ws then
    ws <&> \content ->
      if content `isRule` stackSnapshotRule then
        stackSnapshotContent
      else
        content
  else
    ws ++ [BuildNewline, loadContent, BuildNewline, stackSnapshotContent]
  where
    stackSnapshotRule = buildStackSnapshotRule package stackSnapshotPath
    (loadContent, stackSnapshotContent) = fromRule stackSnapshotRule

generateBuildFile :: Env -> Hpack.Package -> IO ()
generateBuildFile env package = do
  let path = bazelProjectPath env </> bazelBuildPath env
  build <- Text.readFile path
  case parse Bazel.buildFileParser path build of
    Left err ->
      fail $ show err
    Right b ->
      let build' = replaceHaskellLibraryRule package b
          pbuild = vsep $ map pretty (build' ++ [BuildNewline])
      in Text.writeFile path $ renderStrict (layoutPretty defaultLayoutOptions pbuild)

replaceHaskellLibraryRule :: Hpack.Package -> BuildFile -> BuildFile
replaceHaskellLibraryRule package build = do
  if any (`isRule` haskellLibraryRule) build then
    build <&> \content ->
      if content `isRule` haskellLibraryRule then
        haskellLibraryContent
      else
        content
  else
    build ++ [BuildNewline, loadContent, BuildNewline, haskellLibraryContent]
  where
    haskellLibraryRule = buildHaskellLibraryRule package
    (loadContent, haskellLibraryContent) = fromRule haskellLibraryRule
