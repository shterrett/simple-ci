{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core
import Docker (Docker, Image (..))
import Network.HTTP.Client qualified as Client
import RIO
import RIO.Map qualified as Map
import Socket (newManager)
import System.Process.Typed qualified as Process
import Test.Hspec

main :: IO ()
main = hspec $ do
  beforeAll cleanupDocker $ do
    describe "SimpleCI" $ do
      it "runs a build" $ do
        runIntegrationTest testRunSuccess

testRunSuccess :: ReaderT IntegrationTestEnv IO ()
testRunSuccess = do
  result <- runBuild testBuild
  liftIO $ do
    result ^. #state `shouldBe` BuildFinished BuildSucceeded
    Map.elems (result ^. #completedSteps) `shouldBe` [StepSucceeded, StepSucceeded]

runBuild :: (MonadIO m, Docker m) => Build -> m Build
runBuild build = do
  newBuild <- progress build
  case newBuild ^. #state of
    BuildFinished _ -> pure newBuild
    _ -> do
      threadDelay oneSecond
      runBuild newBuild
  where
    oneSecond :: Int
    oneSecond = 1 * 1000 * 1000

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
    ( makeStep "first step" "ubuntu" ("date" :| [])
        :| [makeStep "second step" "ubuntu" ("uname -r" :| [])]
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
