
module Libjit.Context (withContext)  where

import Libjit.Raw
import Libjit.Jit

import Control.Monad.State
import Control.Monad.IO.Class

withContext :: (a -> Jit b) -> a -> Jit b 
withContext f x =
  do
    ctx <- liftIO contextCreate
    put ctx
    b <- f x
    liftIO $ contextDestroy ctx
    return b 


