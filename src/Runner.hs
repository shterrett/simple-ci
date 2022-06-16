{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Runner where

import Core (Build (..), BuildState (..), Log, LogCollection, Pipeline, collectLogs, initLogCollection, progress)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Docker (Docker (..))
import RIO

class (Monad m) => Hooks m where
  logCollected :: Log -> m ()

class (Monad m) => Runner m where
  runBuild :: Build -> m Build
  prepareBuild :: Pipeline -> m Build

instance forall m. (Monad m, MonadIO m, Hooks m, Docker m) => Runner m where
  runBuild b = do
    loop b $ initLogCollection (b ^. #pipeline)
    where
      loop :: Build -> LogCollection -> m Build
      loop build collection = do
        now <- liftIO getPOSIXTime
        (newCollection, logs) <- collectLogs now collection build
        traverse_ logCollected logs
        newBuild <- progress build
        case newBuild ^. #state of
          BuildFinished _ -> pure newBuild
          _ -> do
            threadDelay oneSecond
            loop newBuild newCollection
      oneSecond :: Int
      oneSecond = 1 * 1000 * 1000
  prepareBuild p = do
    v <- createVolume
    pure $
      Build
        { pipeline = p
        , state = BuildReady
        , completedSteps = mempty
        , volume = v
        }
