module Hazell
    ( generate
    ) where

import           RIO
import           RIO.FilePath              (takeDirectory, (</>))
import qualified RIO.Map                   as Map
import qualified RIO.Text                  as Text

import           Bazel.Build               (BuildContent (..), BuildFile,
                                            fromRule, isRule)
import           Bazel.Cabal               (CabalPackage, readAllCabalFiles)
import           Bazel.Haskell
import qualified Bazel.Parser              as Bazel
import           Bazel.Rule                (Rule (..))
import           Data.Functor              ((<&>))
import           Data.List                 (find)
import qualified Data.Map.Merge.Strict     as Map
import           Hazell.Env
import qualified Hpack.Config              as Hpack
import           Prettyprinter             (defaultLayoutOptions, layoutPretty,
                                            pretty, vsep)
import           Prettyprinter.Render.Text (putDoc, renderStrict)
import           Text.Megaparsec           (parse)

generate :: RIO Env ()
generate = do
  path <- asks packageYamlPath
  let opts = Hpack.defaultDecodeOptions { Hpack.decodeOptionsTarget = path }
  result <- liftIO $ Hpack.readPackageConfig opts
  case result of
    Left e ->
      logError (displayShow e)
    Right r -> do
      let package = Hpack.decodeResultPackage r
      generateWorkspaceFile package "stack-snapshot.yaml"
      generateBuildFile package

generateWorkspaceFile :: Hpack.Package -> FilePath -> RIO Env ()
generateWorkspaceFile package stackSnapshot = do
  path <- (</> "WORKSPACE") <$> asks bazelProjectPath
  ws <- readFileUtf8 path
  case parse Bazel.buildFileParser path ws of
    Left err ->
      logError (displayShow err)
    Right w -> do
      cabals <- do
        flag <- asks recReadCabals
        if flag then
          fmap Just . readAllCabalFiles . takeDirectory =<< asks packageYamlPath
        else
          pure Nothing
      let ws' = replaceStackSnapshotRule package stackSnapshot cabals w
          pws = vsep $ map pretty (ws' ++ [BuildNewline])
      writeFileUtf8 path $ renderStrict (layoutPretty defaultLayoutOptions pws)

replaceStackSnapshotRule :: Hpack.Package -> FilePath -> Maybe [CabalPackage] -> BuildFile -> BuildFile
replaceStackSnapshotRule package stackSnapshotPath cabals ws =
  if any (`isRule` stackSnapshotRule) ws then
    ws <&> \content ->
      if content `isRule` stackSnapshotRule then
        content `mergeRuleArgs` stackSnapshotRule
      else
        content
  else
    ws ++ [BuildNewline, loadContent, BuildNewline, stackSnapshotContent]
  where
    stackSnapshotRule = buildStackSnapshotRule package stackSnapshotPath cabals
    (loadContent, stackSnapshotContent) = fromRule stackSnapshotRule

mergeRuleArgs :: BuildContent -> Rule -> BuildContent
mergeRuleArgs (BuildRule name args) rule =
  BuildRule name . Map.toList $ Map.merge
    Map.preserveMissing
    Map.preserveMissing
    (Map.zipWithMatched $ \_ old new -> new)
    (Map.fromList args)
    (Map.fromList $ ruleArgs rule)
mergeRuleArgs content _ = content

generateBuildFile :: Hpack.Package -> RIO Env ()
generateBuildFile package = do
  env <- ask
  let path = bazelProjectPath env </> bazelBuildPath env
  build <- readFileUtf8 path
  case parse Bazel.buildFileParser path build of
    Left err ->
      logError (displayShow err)
    Right b ->
      let build' = replaceHaskellLibraryRule package b
          pbuild = vsep $ map pretty (build' ++ [BuildNewline])
      in writeFileUtf8 path $ renderStrict (layoutPretty defaultLayoutOptions pbuild)

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
