{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Docker where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.Char (toUpper)
import Data.Generics.Labels (Field')
import Data.Time.Clock.POSIX qualified as Time
import Network.HTTP.Client qualified as Client
import Network.HTTP.Simple qualified as HTTP
import RIO
import RIO.Map qualified as Map

newtype Image = Image {unImage :: Text}
  deriving (Eq, Show, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype ContainerExitCode = ContainerExitCode {unContainerExitCode :: Int}
  deriving (Show, Eq, Generic)

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Show, Eq, Generic)

data CreateContainerOptions = CreateContainerOptions
  { image :: Image
  , tty :: Bool
  , labels :: [(Text, Text)]
  , cmd :: Text
  , env :: ContainerEnv
  , entrypoint :: Text
  , volume :: Volume
  , workingDir :: Text
  , hostConfig :: HostConfig
  }
  deriving (Show, Eq, Generic)

instance Semigroup CreateContainerOptions where
  o1 <> o2 =
    o2
      { labels = o1 ^. #labels <> o2 ^. #labels
      , env = o1 ^. #env <> o2 ^. #env
      }

instance Monoid CreateContainerOptions where
  mempty =
    CreateContainerOptions
      { image = Image "ubuntu"
      , tty = True
      , labels = [("quad", "")]
      , cmd = "echo \"$QUAD_SCRIPT\" | /bin/sh"
      , env = ContainerEnv []
      , entrypoint = "/bin/sh"
      , volume = Volume ""
      , workingDir = "/app"
      , hostConfig = HostConfig []
      }

newtype ContainerEnv = ContainerEnv {unEnv :: [(Text, Text)]}
  deriving (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance Aeson.ToJSON ContainerEnv where
  toJSON (ContainerEnv envs) =
    Aeson.toJSON $ (\(var, val) -> var <> "=" <> val) <$> envs

newtype HostConfig = HostConfig {unConfig :: [(Text, [Text])]}
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

instance Aeson.ToJSON HostConfig where
  toJSON (HostConfig configs) = Aeson.toJSON $ Map.fromList configs

mkContainerOptions :: Image -> Volume -> Text -> CreateContainerOptions
mkContainerOptions image volume script =
  mempty
    & #image .~ image
    & #env . #unEnv .~ [("QUAD_SCRIPT", script)]
    & #volume .~ volume
    & #hostConfig . #unConfig .~ [("Binds", [volume ^. #unVolume <> ":/app"])]

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

type RequestBuilder = Text -> HTTP.Request

createContainerIO :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainerIO buildReq options = do
  let body = Aeson.toJSON options
  let req =
        buildReq "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  let parser = Aeson.withObject "create-container" $ \o ->
        o .: "Id"
  HTTP.httpBS req >>= parseResponse parser

startContainerIO :: RequestBuilder -> ContainerId -> IO ()
startContainerIO buildReq (ContainerId containerId) = do
  let path = "/v1.40/containers/" <> containerId <> "/start"
  let req =
        buildReq path
          & HTTP.setRequestPath (encodeUtf8 path)
          & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req

containerStatusIO :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatusIO buildReq (ContainerId containerId) = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- o .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other
  let req = buildReq $ "/v1.40/containers/" <> containerId <> "/json"
  HTTP.httpBS req >>= parseResponse parser

newtype Volume = Volume {unVolume :: Text}
  deriving (Eq, Show, Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

createVolumeIO :: RequestBuilder -> IO Volume
createVolumeIO buildReq = do
  let body = Aeson.object [("Labels", Aeson.object [("quad", "")])]
      req =
        buildReq "/volumes/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
      parser = Aeson.withObject "create-volume" $ (.: "Name")

  HTTP.httpBS req >>= parseResponse parser

fetchLogsIO :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogsIO = undefined

data FetchLogsOptions = FetchLogsOptions
  { container :: ContainerId
  , since :: Time.POSIXTime
  , until :: Time.POSIXTime
  }
  deriving (Show, Eq, Generic)

class (Monad m) => Docker m where
  createContainer :: CreateContainerOptions -> m ContainerId
  startContainer :: ContainerId -> m ()
  containerStatus :: ContainerId -> m ContainerStatus
  createVolume :: m Volume
  fetchLogs :: FetchLogsOptions -> m ByteString

instance
  ( Field' "dockerManager" r Client.Manager
  , Monad m
  , MonadReader r m
  , MonadIO m
  ) =>
  Docker m
  where
  createContainer options = do
    builder <- requestBuilder <$> view #dockerManager
    liftIO $ createContainerIO builder options
  startContainer cid = do
    builder <- requestBuilder <$> view #dockerManager
    liftIO $ startContainerIO builder cid
  containerStatus cid = do
    builder <- requestBuilder <$> view #dockerManager
    liftIO $ containerStatusIO builder cid
  createVolume = do
    builder <- requestBuilder <$> view #dockerManager
    liftIO $ createVolumeIO builder
  fetchLogs options = do
    builder <- requestBuilder <$> view #dockerManager
    liftIO $ fetchLogsIO builder options

requestBuilder :: Client.Manager -> RequestBuilder
requestBuilder manager = \path ->
  HTTP.defaultRequest
    & HTTP.setRequestManager manager
    & HTTP.setRequestPath (encodeUtf8 path)
