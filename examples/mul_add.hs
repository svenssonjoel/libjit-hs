{- Tests
   Joel Svensson 2012 
-}

-- Compile:  ghc --make test.hs ./Libjit/cbits/o/extra.o -ljit 
import Libjit
import qualified Libjit.Raw as Raw
import Libjit.Raw (int_type) -- export all these in Libjit.hs

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Control.Monad.State

import Data.Int

main =
  jitSession $ do 
    -- change "getIntType" to not being in IO monad.
    -- maybe also change the name. 
    -- int_type <- liftIO$ Raw.getIntType 
  
    fun <- runFunBuilder $ mkFun CDECL
                                    int_type
                                    [int_type, int_type,int_type] $
      do
        a <- getParam 0
        b <- getParam 1
        c <- getParam 2
        tmp1 <- mul a b
        tmp2 <- add tmp1 c
        ret tmp2

    compile fun 
    
    -- Hackity
    liftIO$ withArray [3,5,2,0 :: Int] $ \ ptr ->
      do 
        let ptr' = castPtr ptr 
        Raw.apply fun [ptr'
                      ,(plusPtr ptr' 8) 
                      ,(plusPtr ptr' 16)]
                      (plusPtr  ptr' 24)
        val <- peek (castPtr (plusPtr ptr' 24))    
        putStrLn $ "result: " ++ show (val :: Int)

    liftIO$ putStrLn "Ok"