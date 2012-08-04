{- Tests -}

-- Compile:  ghc --make test.hs ./Libjit/cbits/o/extra.o -ljit 
import Libjit.Context
import Libjit.Jit
import Libjit.Raw
import Libjit.Function

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Control.Monad.State

import Data.Int

main =
  jitSession $ do 
    -- ctx <- contextCreate
    (Just ctx) <- get 
    int_type <- liftIO$ getIntType 
    liftIO$ startBuild(ctx)
    sig <- liftIO$ createTypeSignature CDECL
                               int_type
                               [int_type,int_type,int_type]
                               3 1
    fun <- liftIO$ createFunction ctx sig
    a <- liftIO$ getParam fun 0
    b <- liftIO$ getParam fun 1
    c <- liftIO$ getParam fun 2
    tmp1 <- liftIO$ mul fun a b
    tmp2 <- liftIO$ add fun tmp1 c
    liftIO$ ret fun tmp2
    liftIO$ compile fun 
    liftIO$ endBuild ctx 

    -- Hackity
    liftIO$ withArray [3,5,2,0 :: Int] $ \ ptr ->
      do 
        let ptr' = castPtr ptr 
        apply fun [ptr'
                  ,(plusPtr ptr' 8) 
                  ,(plusPtr ptr' 16)]
                  (plusPtr  ptr' 24)
        val <- peek (castPtr (plusPtr ptr' 24))    
        putStrLn $ "result: " ++ show (val :: Int)

    liftIO$ putStrLn "Ok"