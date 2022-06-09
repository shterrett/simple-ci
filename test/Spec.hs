{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Core
import Data.Set qualified as Set
import Docker (ContainerExitCode (..), Image (..), Volume (..))
import Network.HTTP.Client qualified as Client
import RIO
import RIO.ByteString qualified as BS
import RIO.Map qualified as Map
import Runner (Hooks (..), Runner (..))
import Socket (newManager)
import System.Process.Typed qualified as Process
import Test.Hspec

main :: IO ()
main = hspec $ do
  beforeAll cleanupDocker $ do
    describe "SimpleCI" $ do
      it "runs a build" $ do
        runIntegrationTest testRunSuccess
      it "fails a build" $ do
        runIntegrationTest testRunFailure
      it "creates a shared workspace" $ do
        runIntegrationTest testSharedWorkspace

testRunSuccess :: ReaderT IntegrationTestEnv IO ()
testRunSuccess = do
  result <- runBuild testBuild
  liftIO $ do
    result ^. #state `shouldBe` BuildFinished BuildSucceeded
    Map.elems (result ^. #completedSteps) `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: ReaderT IntegrationTestEnv IO ()
testRunFailure = do
  build <- prepareBuild $ makePipeline $ singleton $ makeStep "Should fail" "ubuntu" (singleton "exit 1")
  result <- runBuild build
  liftIO $ do
    result ^. #state `shouldBe` BuildFinished BuildFailed
    Map.elems (result ^. #completedSteps) `shouldBe` [StepFailed (ContainerExitCode 1)]

testSharedWorkspace :: ReaderT IntegrationTestEnv IO ()
testSharedWorkspace =
  do
    build <-
      prepareBuild $
        makePipeline $
          (makeStep "Create File" "ubuntu" $ singleton "echo hello > test")
            :| [makeStep "ReadFile" "ubuntu" $ singleton "cat test"]
    result <- runBuild build
    liftIO $ do
      result ^. #state `shouldBe` BuildFinished BuildSucceeded
      Map.elems (result ^. #completedSteps)
        `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: ReaderT IntegrationTestEnv IO ()
testLogCollection = do
  expected <- newMVar $ Set.fromList @ByteString ["hello", "world", "Linux"]
  let onLog :: Log -> IO ()
      onLog l = do
        let words = Set.fromList $ BS.split ' ' l
        withMVar expected $ \remaining ->
          remaining `Set.difference` words
  local (set #onLogCollected onLog) $ do
    build <-
      prepareBuild $
        makePipeline $
          (makeStep "Long step" "ubuntu" ("echo hello" :| ["sleep 2", "echo world"]))
            :| [makeStep "Echo Linux" "ubuntu" (singleton "uname -s")]
    result <- runBuild build
    liftIO $ do
      result ^. #state `shouldBe` BuildFinished BuildSucceeded
      Map.elems (result ^. #completedSteps) `shouldBe` [StepSucceeded, StepSucceeded]
      readMVar expected `shouldReturn` Set.empty

makeStep :: Text -> Text -> NonEmpty Text -> Step
makeStep n img cmds =
  Step
    { name = StepName n
    , image = Image img
    , commands = cmds
    }

makePipeline :: NonEmpty Step -> Pipeline
makePipeline = Pipeline

testPipeline :: Pipeline
testPipeline =
  makePipeline
    ( makeStep "first step" "ubuntu" (singleton "date")
        :| [makeStep "second step" "ubuntu" (singleton "uname -r")]
    )

testBuild :: Build
testBuild =
  Build
    { pipeline = testPipeline
    , state = BuildReady
    , completedSteps = mempty
    , volume = Volume ""
    }

data IntegrationTestEnv = IntegrationTestEnv
  { dockerManager :: Client.Manager
  , onLogCollected :: Log -> IO ()
  }
  deriving (Generic)

instance Hooks (ReaderT IntegrationTestEnv IO) where
  logCollected l = asks onLogCollected >>= liftIO . ($ l)

runIntegrationTest :: ReaderT IntegrationTestEnv IO a -> IO a
runIntegrationTest m =
  do
    let onLogCollected = const (pure ())
    dockerManager <- newManager "/var/run/docker.sock"
    flip runReaderT (IntegrationTestEnv {..}) $ m

cleanupDocker :: IO ()
cleanupDocker = do
  void $ Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"
  void $ Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"

singleton :: a -> NonEmpty a
singleton a = a :| []
