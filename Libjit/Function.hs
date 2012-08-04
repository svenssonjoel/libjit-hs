{- Joel Svensson 2012 -} 

{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module Libjit.Function where


-- function building...
{- Function.hs
   - Function building utilities. 
-} 

import Libjit.Raw
import Libjit.Jit

import Control.Monad.State 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

type FunStack = [Function]

newtype FunBuilder a =
  FunBuilder (StateT FunStack Jit a)
   deriving (Monad, MonadIO, MonadState FunStack)

liftJit :: Jit a -> FunBuilder a
liftJit = FunBuilder . lift

getContext :: FunBuilder Context
getContext =
  do (Just ctx) <- liftJit get
     return ctx 

buildFun :: ABI -> Type -> [Type] -> FunBuilder () -> FunBuilder Function
buildFun abi out_t in_t body =
  do
    -- TODO: The last int is really an enum type, fix this ! 
    sig <- liftIO$ createTypeSignature abi out_t in_t nargs 1

    ctx <- getContext
    fun <- liftIO$ createFunction ctx sig
    body 
           
    return fun 
    where
      nargs = length in_t
 
    