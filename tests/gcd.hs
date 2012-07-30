
import Libjit

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


import Data.Int


main =
  do
    ctx <- contextCreate
    uint_type <- getUIntType
    startBuild(ctx)
    lab1 <- getUndefinedLabel
    lab2 <- getUndefinedLabel

    sig <- createTypeSignature CDECL
                               uint_type
                               [uint_type,uint_type]
                               2 1
                               
    fun <- createFunction ctx sig
    x <- getParam fun 0
    y <- getParam fun 1
    t1 <- eq fun x y
    branchIfNot fun t1 lab1 
    ret fun x
    setLabel fun lab1

    t2 <- lt fun x y
    branchIfNot fun t2 lab2

    a1 <- sub fun y x
    t3 <- callFunction fun
                       "gcd"
                       fun
                       (Type nullPtr) 
                       [x,a1]
                       2 0
    ret fun t3 
    setLabel fun lab2                       
  
    a0 <- sub fun x y
    t4 <- callFunction fun
                       "gcd"
                       fun
                       (Type nullPtr)
                       [a0,y]
                       2 0
    ret fun t4 

    compile fun
    endBuild(ctx)
    -- Hackity
    withArray [15,18,0 :: Int] $ \ ptr ->
      do 
        let ptr' = castPtr ptr 
        apply fun [ptr'
                  ,(plusPtr ptr' 8)]
                  (plusPtr  ptr' 16)                
        val <- peek (castPtr (plusPtr ptr' 16))    
        putStrLn $ "result: " ++ show (val :: Int)


    putStrLn "Ok"