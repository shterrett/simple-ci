{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Core
import Docker (ContainerExitCode (..), Image (..))
import RIO
import RIO.Map qualified as Map

main :: IO ()
main = pure ()

makeStep :: Text -> Text -> NonEmpty Text -> Step
makeStep name image cmds =
  Step
    { name = StepName name
    , image = Image image
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
    , completedSteps = Map.empty
    }
