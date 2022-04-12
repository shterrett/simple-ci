{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Docker where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Char (toUpper)
import Data.Generics.Labels ()
import Network.HTTP.Simple qualified as HTTP
import RIO
import Socket (newManager)

newtype Image = Image {unImage :: Text}
  deriving (Eq, Show, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype ContainerExitCode = ContainerExitCode {unContainerExitCode :: Int}
  deriving (Show, Eq, Generic)

data CreateContainerOptions = CreateContainerOptions
  { image :: Image
  , tty :: Bool
  , labels :: [(Text, Text)]
  , cmd :: Text
  , entrypoint :: Text
  }
  deriving (Show, Eq, Generic)

instance Semigroup CreateContainerOptions where
  o1 <> o2 = o2 {labels = o1 ^. #labels <> o2 ^. #labels}

instance Monoid CreateContainerOptions where
  mempty =
    CreateContainerOptions
      { image = Image "ubuntu"
      , tty = True
      , labels = [("quad", "")]
      , cmd = "echo hello"
      , entrypoint = "/bin/sh"
      }

instance Aeson.ToJSON CreateContainerOptions where
  toEncoding =
    Aeson.genericToEncoding aesonOptions
  toJSON =
    Aeson.genericToJSON aesonOptions

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions {Aeson.fieldLabelModifier = capitalize}

capitalize :: String -> String
capitalize = \case
  "" -> ""
  c : cs -> toUpper c : cs

newtype ContainerId = ContainerId {unContainerId :: Text}
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON ContainerId where
  parseJSON =
    Aeson.genericParseJSON aesonOptions

instance Aeson.ToJSON ContainerId where
  toJSON = Aeson.genericToJSON aesonOptions

parseResponse ::
  (Aeson.Value -> Aeson.Types.Parser a) ->
  HTTP.Response ByteString ->
  IO a
parseResponse p response = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody response)
        Aeson.Types.parseEither p value
  either throwString pure result

createContainer :: CreateContainerOptions -> IO ContainerId
createContainer options = do
  manager <- newManager "/var/run/docker.sock"
  let body = Aeson.toJSON options
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  let parser = Aeson.withObject "create-container" $ \o ->
        o .: "Id"
  HTTP.httpBS req >>= parseResponse parser

startContainer :: ContainerId -> IO ()
startContainer (ContainerId containerId) = do
  manager <- newManager "/var/run/docker.sock"
  let path = "/v1.40/containers/" <> containerId <> "/start"
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestManager manager
          & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req
