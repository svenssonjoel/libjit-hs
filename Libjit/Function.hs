{- Joel Svensson 2012 -} 

{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module Libjit.Function ( ABI(..)   -- Raw
                       , Function  -- Raw
                       , mkFun, runFunBuilder
                       , compile 
                       , getParam
                       , add, mul, ret ) where


-- function building...
{- Function.hs
   - Function building utilities. 
-} 

import qualified Libjit.Raw as Raw
import Libjit.Raw (Function, ABI(..), Value, Context, Type)
import Libjit.Jit

import Control.Monad.State 
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

type FunStack = [Function]

newtype FunBuilder a =
  FunBuilder (StateT FunStack Jit a)
   deriving (Monad, MonadIO, MonadState FunStack)

top :: FunBuilder Function 
top =
  do
    stack <- get
    case stack of
      [] -> error "Empty function stack"
      (x:xs) -> return x

push :: Function -> FunBuilder ()
push f =
  do
    stack <- get
    put (f:stack)

pop :: FunBuilder Function
pop =
  do
    stack <- get
    case stack of
      [] -> error "Empty function stack"
      (x:xs) -> do
        put xs
        return x 

liftJit :: Jit a -> FunBuilder a
liftJit = FunBuilder . lift

getContext :: FunBuilder Context
getContext =
  do (Just ctx) <- liftJit get
     return ctx 

runFunBuilder :: FunBuilder a -> Jit a
runFunBuilder (FunBuilder fb) = evalStateT fb [] 

mkFun :: ABI -> Type -> [Type] -> FunBuilder () -> FunBuilder Function
mkFun abi out_t in_t body =
  do
    -- TODO: The last int is really an enum type, fix this !
    ctx <- getContext

    liftIO$ Raw.startBuild ctx
    
    sig <- liftIO$ Raw.createTypeSignature abi out_t in_t nargs 1

    
    fun <- liftIO$ Raw.createFunction ctx sig
    push fun
    body

    liftIO$ Raw.endBuild ctx 
    
    fun <- pop
    return fun 
    where
      nargs = length in_t
 
-- Lift instructions into FunBuilder.

compile :: Function -> Jit ()
compile f =
  do
    (Just ctx) <- get 
    liftIO$
      do
        Raw.startBuild ctx
        Raw.compile f
        Raw.endBuild ctx 

lift1 :: (Function -> a -> IO b) -> a -> FunBuilder b
lift1 f a = do {fun <- top; liftJit $ liftIO $ f fun a}

lift2 :: (Function -> a -> b -> IO c) -> a -> b -> FunBuilder c
lift2 f a b = do {fun <- top; liftJit $ liftIO $ f fun a b} 

getParam :: Int -> FunBuilder Value
getParam i = do { fun <- top; liftJit $ liftIO $ Raw.getParam fun i}

ret = lift1 Raw.ret
add = lift2 Raw.add
mul = lift2 Raw.mul


