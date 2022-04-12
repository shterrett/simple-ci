{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Runner where

import Core (Build (..), BuildState (..), Pipeline, progress)
import Docker (Docker)
import RIO

class (Monad m) => Runner m where
  runBuild :: Build -> m Build
  prepareBuild :: Pipeline -> m Build

instance (Monad m, MonadIO m, Docker m) => Runner m where
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
  prepareBuild p =
    pure $
      Build
        { pipeline = p
        , state = BuildReady
        , completedSteps = mempty
        }
