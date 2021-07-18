{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}

module Bazel.Cabal
  ( DotPayload (..)
  , PackageLocation (..)
  , runStackLs
  , CabalPackage
  , readCabalFile
  , readAllCabalFiles
  , toSetupDeps
  , toPackageName
  ) where

import           RIO
import qualified RIO.ByteString.Lazy                    as BL
import           RIO.Process                            (HasProcessContext,
                                                         proc,
                                                         readProcessStdout_,
                                                         withWorkingDir)
import qualified RIO.Text                               as Text

import           Data.Aeson                             as JSON
import           Data.Version                           (Version)
import           Deriving.Aeson
import qualified Distribution.PackageDescription        as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.Dependency          as Cabal
import qualified Distribution.Types.PackageId           as Cabal
import qualified Distribution.Types.PackageName         as Cabal
import           Network.HTTP.Client                    (Response (responseBody, responseStatus),
                                                         httpLbs, newManager,
                                                         parseRequest)
import           Network.HTTP.Client.TLS                (tlsManagerSettings)
import           Network.HTTP.Types                     (Status (Status))

data DotPayload = DotPayload
  { payloadName     :: Text
  , payloadVersion  :: Maybe Version
  , payloadLocation :: Maybe PackageLocation
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "payload", CamelToSnake]] DotPayload

data PackageLocation = PackageLocation
  { locationType :: Text
  , locationUrl  :: Text
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "location", CamelToSnake]] PackageLocation

runStackLs
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => FilePath -> m [DotPayload]
runStackLs path = do
  out <- proc "stack" ["ls", "dependencies", "json", "--test"] (withWorkingDir path . readProcessStdout_)
  case JSON.eitherDecode out of
    Left e  -> logError (displayShow e) >> pure mempty
    Right a -> pure a

type CabalPackage = Cabal.PackageDescription

readCabalFile :: MonadIO m => DotPayload -> m (Maybe (Either String CabalPackage))
readCabalFile payload =
  case payloadLocation payload of
    (Just (PackageLocation "hackage" url)) -> do
      Just . (parsePackageDescription =<<) <$> get (Text.unpack url ++ "/" ++ packageName ++ ".cabal")
    _ ->
      pure Nothing
  where
    packageName = Text.unpack (payloadName payload)
    parsePackageDescription b = case Cabal.parseGenericPackageDescriptionMaybe b of
      Nothing -> Left $ "cannnot parse to cabal file " ++ packageName
      Just p  -> Right $ Cabal.packageDescription p

get :: MonadIO m => String -> m (Either String ByteString)
get url = do
  manager  <- liftIO $ newManager tlsManagerSettings
  request  <- liftIO $ parseRequest url
  response <- liftIO $ httpLbs request manager
  pure $ case responseStatus response of
    Status 200 _ -> do
      Right (BL.toStrict $ responseBody response)
    status ->
      Left $ "Error while downloading " ++ url ++ " (" ++ formatStatus status ++ ")"

formatStatus :: Status -> String
formatStatus (Status code message) = show code ++ " " ++ show message

readAllCabalFiles
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => FilePath -> m [CabalPackage]
readAllCabalFiles path = do
  deps <- runStackLs path
  catMaybes <$> traverse readCabalFile' deps
  where
    readCabalFile' payload = do
      r <- readCabalFile payload
      case r of
        Nothing ->
          pure Nothing
        Just (Right a) ->
          pure (Just a)
        Just (Left e) ->
          logError (displayShow e) >> pure Nothing

toSetupDeps :: CabalPackage -> [String]
toSetupDeps =
  fmap (Cabal.unPackageName . Cabal.depPkgName) . maybe [] Cabal.setupDepends . Cabal.setupBuildInfo

toPackageName :: CabalPackage -> String
toPackageName =
  Cabal.unPackageName . Cabal.pkgName . Cabal.package
