
module Libjit.Context  where

import Libjit.Raw
import Libjit.Jit

import Control.Monad.State
import Control.Monad.IO.Class

{-
withContext :: Jit a -> Jit a 
withContext jit =
  do
    ctx <- contextCreate
    put (Just ctx)
    a <- runJit jit
    contextDestroy ctx
    return a
-}

