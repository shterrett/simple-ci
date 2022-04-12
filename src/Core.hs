{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Core where

import Control.Lens (filtered, folded)
import Data.Generics.Labels ()
import Docker (ContainerExitCode (..), Docker (..), Image, createContainer, startContainer)
import RIO
import RIO.Map qualified as Map

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
    -- perhaps
    -- completedSteps :: [StepResult]
    -- pendingStep :: [Step]
    -- currentStep :: Maybe Step
    -- invariant: completedSteps <> pendingStep <> completedSteps == Pipeline?
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
  }
  deriving (Show, Eq, Generic)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Show, Eq, Generic)

progress :: (Docker m) => Build -> m Build
progress build@Build {..} =
  case state of
    BuildFinished _ -> do
      undefined
    BuildRunning s -> do
      let exit = ContainerExitCode 0
          result = exitCodeToStepResult exit
      pure
        build
          { state = BuildReady
          , completedSteps = Map.insert (s ^. #currentStep) result (build ^. #completedSteps)
          }
    BuildReady -> case buildHasNextStep build of
      Left result -> pure $ build {state = BuildFinished result}
      Right s -> do
        let options = mempty & #image .~ (s ^. #image)
        createContainer options >>= startContainer
        pure $ build {state = BuildRunning BuildRunningState {currentStep = s ^. #name}}

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
