{-# LANGUAGE GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

module Libjit.Jit  where

import Libjit.Raw

import Control.Monad.State

newtype Jit a = Jit {unJit :: (StateT (Maybe Context) IO a)}
                deriving (Monad, MonadIO, MonadState (Maybe Context))
                         
jitSession :: Jit a -> IO a
jitSession jit = evalStateT (unJit prg) Nothing
  where
    prg = do
      ctx <- liftIO contextCreate
      put (Just ctx)
      a <- jit
      liftIO$ contextDestroy ctx
      put (Nothing)
      return a