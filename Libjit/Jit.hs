{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Libjit.Jit  where

import Libjit.Raw

import Control.Monad.State

newtype Jit a = Jit (StateT Context IO a)
                deriving (Monad, MonadIO, MonadState Context)
                         

