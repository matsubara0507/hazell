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

import           Data.Aeson                             as JSON
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Char8                  as B
import qualified Data.ByteString.Lazy                   as LB
import           Data.Maybe                             (catMaybes)
import           Data.String                            (fromString)
import           Data.Text                              (Text)
import qualified Data.Text                              as Text
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
import           System.Process                         (CreateProcess (cwd),
                                                         proc,
                                                         readCreateProcess)

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

runStackLs :: FilePath -> IO [DotPayload]
runStackLs path = do
  out <- readCreateProcess ((proc "stack" ["ls", "dependencies", "json", "--test"]) { cwd = Just path }) ""
  case JSON.eitherDecode (fromString out) of
    Left e  -> fail (show e)
    Right a -> pure a

type CabalPackage = Cabal.PackageDescription

readCabalFile :: DotPayload -> IO (Maybe (Either String CabalPackage))
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

get :: String -> IO (Either String ByteString)
get url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  pure $ case responseStatus response of
    Status 200 _ -> do
      Right (LB.toStrict $ responseBody response)
    status ->
      Left $ "Error while downloading " ++ url ++ " (" ++ formatStatus status ++ ")"

formatStatus :: Status -> String
formatStatus (Status code message) = show code ++ " " ++ B.unpack message

readAllCabalFiles :: FilePath -> IO [CabalPackage]
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
          fail e

toSetupDeps :: CabalPackage -> [String]
toSetupDeps =
  fmap (Cabal.unPackageName . Cabal.depPkgName) . maybe [] Cabal.setupDepends . Cabal.setupBuildInfo

toPackageName :: CabalPackage -> String
toPackageName =
  Cabal.unPackageName . Cabal.pkgName . Cabal.package
