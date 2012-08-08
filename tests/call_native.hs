{-# LANGUAGE ForeignFunctionInterface #-} 
{- Tests -}

-- Compile:  ghc --make test.hs ./Libjit/cbits/o/extra.o -ljit

import Libjit.Raw 

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.C.Types

import Data.Int


foreign import ccall "stdio.h &putchar"
  putc :: FunPtr (CInt -> IO CInt)
  
main =
  do
    ctx <- contextCreate 
    -- int_type <- getIntType 
    startBuild(ctx)
    funsig <- createTypeSignature CDECL
                                  int_type
                                  [int_type]
                                  1 1 
    sig <- createTypeSignature CDECL
                               int_type
                               [int_type]
                               1 1
    fun <- createFunction ctx sig
    a <- getParam fun 0
    callNativeFunction fun "putc" putc
                       funsig [a] 1 CallNothrow
 
    ret fun a
    compile fun 
    endBuild(ctx)

    -- Hackity
    withArray [43,0 :: Int] $ \ ptr ->
      do 
        let ptr' = castPtr ptr 
        apply fun [ptr']
                  (plusPtr  ptr' 8)
        val <- peek (castPtr (plusPtr ptr' 8))    
        putStrLn $ "result: " ++ show (val :: Int)
       

    
    putStrLn "Ok"