{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core
import Docker (ContainerExitCode (..), Image (..))
import Network.HTTP.Client qualified as Client
import RIO
import RIO.Map qualified as Map
import Runner (Runner (..))
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
    }

data IntegrationTestEnv = IntegrationTestEnv
  { dockerManager :: Client.Manager
  }
  deriving (Generic)

runIntegrationTest :: ReaderT IntegrationTestEnv IO a -> IO a
runIntegrationTest m =
  do
    mgr <- newManager "/var/run/docker.sock"
    flip runReaderT (IntegrationTestEnv mgr) $
      m

cleanupDocker :: IO ()
cleanupDocker = void $ do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"

singleton :: a -> NonEmpty a
singleton a = a :| []
