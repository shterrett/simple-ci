{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Core where

import Control.Lens (at, filtered, folded, (?~))
import Data.Generics.Labels ()
import Data.Time.Clock.POSIX qualified as Time
import Docker (ContainerExitCode (..), ContainerId, ContainerStatus (..), Docker (..), FetchLogsOptions (..), Image, Volume, createContainer, mkContainerOptions, startContainer)
import RIO
import RIO.Map qualified as Map
import RIO.NonEmpty qualified as NonEmpty
import RIO.Text qualified as Text

data Pipeline = Pipeline {steps :: NonEmpty Step}
  deriving (Eq, Show, Generic)

data Step = Step
  { name :: StepName
  , commands :: NonEmpty Text
  , image :: Image
  }
  deriving (Eq, Show, Generic)

data Build = Build
  { pipeline :: Pipeline
  , state :: BuildState
  , -- Why isn't this [StepResult]?
    completedSteps :: Map StepName StepResult
  , -- perhaps
    -- completedSteps :: [StepResult]
    -- pendingStep :: [Step]
    -- currentStep :: Maybe Step
    -- invariant: completedSteps <> pendingStep <> completedSteps == Pipeline?
    volume :: Volume
  }
  deriving (Eq, Show, Generic)

data StepResult
  = StepFailed ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show, Generic)

newtype StepName = StepName {unStepName :: Text}
  deriving (Eq, Show, Generic, Ord)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Show, Eq, Generic)

data BuildRunningState = BuildRunningState
  { currentStep :: StepName
  , containerId :: ContainerId
  }
  deriving (Show, Eq, Generic)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
  deriving (Show, Eq, Generic)

progress :: (Docker m) => Build -> m Build
progress build@Build {..} =
  case state of
    BuildFinished _ -> do
      undefined
    BuildRunning s -> do
      status <- containerStatus (s ^. #containerId)
      case status of
        ContainerRunning -> pure build
        ContainerExited ec ->
          pure $
            build & #state .~ BuildReady
              & #completedSteps . at (s ^. #currentStep) ?~ (exitCodeToStepResult ec)
        ContainerOther t -> pure $ build & #state .~ BuildFinished (BuildUnexpectedState t)
    BuildReady -> case buildHasNextStep build of
      Left result -> pure $ build {state = BuildFinished result}
      Right s -> do
        let options =
              mkContainerOptions
                (s ^. #image)
                volume
                (mkScript $ s ^. #commands)
        containerId <- createContainer options
        startContainer containerId
        pure $ build {state = BuildRunning BuildRunningState {currentStep = s ^. #name, containerId = containerId}}
  where
    mkScript :: NonEmpty Text -> Text
    mkScript = Text.unlines . ("set -ex" :) . NonEmpty.toList

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  let toDo = build ^.. #pipeline . #steps . folded . filtered (flip Map.member (build ^. #completedSteps) . (view #name))
      failed = not . null $ build ^.. #completedSteps . folded . #_StepFailed
   in if failed
        then Left BuildFailed
        else case toDo of
          [] -> Left BuildSucceeded
          (s : _) -> Right s

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult cec@(ContainerExitCode ec) = if ec == 0 then StepSucceeded else StepFailed cec

data Log = Log
  { output :: ByteString
  , step :: StepName
  }

type LogCollection = Map StepName CollectionStatus

{- | Ready when the container hasn't started
 Collecting when the container is running
 Finished when the container is exited
 Why is this a separate sum type? What if it gets out of sync?
 Simple haskell doesn't mean don't encode invariants in the type system!
-}
data CollectionStatus
  = CollectionReady
  | CollectingLogs ContainerId Time.POSIXTime
  | CollectionFinished
  deriving (Eq, Show, Generic)

initLogCollection :: Pipeline -> LogCollection
initLogCollection pipeline =
  Map.fromList $
    pipeline ^.. #steps . folded . #name . to (,CollectionReady)

updateCollection ::
  BuildState ->
  Time.POSIXTime ->
  LogCollection ->
  LogCollection
updateCollection state lastCollection =
  Map.mapWithKey $ \step -> \case
    CollectionReady -> update step 0 CollectionReady
    CollectingLogs _ _ -> update step lastCollection CollectionFinished
    CollectionFinished -> CollectionFinished
  where
    update :: StepName -> Time.POSIXTime -> CollectionStatus -> CollectionStatus
    update step since nextState =
      case state of
        BuildRunning s ->
          if s ^. #currentStep == step
            then CollectingLogs (s ^. #containerId) since
            else nextState
        _ -> nextState

collectLogs ::
  ( Docker m
  ) =>
  Time.POSIXTime ->
  LogCollection ->
  Build ->
  m (LogCollection, [Log])
collectLogs collectUntil collection build = do
  logs <- runCollection collection collectUntil
  let newCollection = updateCollection (build ^. #state) collectUntil collection
  pure (newCollection, logs)

runCollection ::
  forall m.
  ( Docker m
  ) =>
  LogCollection ->
  Time.POSIXTime ->
  m [Log]
runCollection collection collectUntil = do
  logs <- Map.traverseWithKey f collection
  pure $ concat (Map.elems logs)
  where
    f :: StepName -> CollectionStatus -> m [Log]
    f step = \case
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options =
              FetchLogsOptions
                { container = container
                , since = since
                , until = collectUntil
                }
        output <- fetchLogs options
        pure [Log {step = step, output = output}]
