{- Tests -}

-- Compile:  ghc --make test.hs ./Libjit/cbits/o/extra.o -ljit 
import Libjit 

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import Data.Int

main =
  do
    ctx <- contextCreate 
    int_type <- getIntType 
    startBuild(ctx)
    sig <- createTypeSignature CDECL
                               int_type
                               [int_type,int_type,int_type]
                               3 1
    fun <- createFunction ctx sig
    a <- getParam fun 0
    b <- getParam fun 1
    c <- getParam fun 2
    tmp1 <- mul fun a b
    tmp2 <- add fun tmp1 c
    ret fun tmp2
    compile fun 
    endBuild(ctx)

    -- Hackity
    withArray [3,5,2,0 :: Int] $ \ ptr ->
      do 
        let ptr' = castPtr ptr 
        apply fun [ptr'
                  ,(plusPtr ptr' 8) 
                  ,(plusPtr ptr' 16)]
                  (plusPtr  ptr' 24)
        val <- peek (castPtr (plusPtr ptr' 24))    
        putStrLn $ "result: " ++ show (val :: Int)

    putStrLn "Ok"