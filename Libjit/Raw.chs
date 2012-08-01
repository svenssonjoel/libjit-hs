

{-# LANGUAGE ForeignFunctionInterface,
             DeriveDataTypeable #-} 


module Libjit.Raw  where 

import Foreign.Ptr
import Foreign.ForeignPtr 
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

import Data.Word
import Data.Typeable

import Control.Exception
import Control.Monad


#include <jit/jit.h>
#include "cbits/src/extra.h"

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------
newtype Context = Context {fromContext :: Ptr ()} 
newtype Type = Type {fromType :: Ptr ()} 
newtype Function = Function {fromFunction :: Ptr ()}
newtype Value    = Value {fromValue :: Ptr ()}

-- Label is different from most other libjit types.
--  Label is an unsigned long  in C.
--  Labels seems to be passed by reference to most functions.
--  TODO: Will this be correct or do I need something else
--        Top ensure that the label is not "freed" while
--        still needed by libjit 
newtype Label    = Label (ForeignPtr CULong)
fromLabel (Label fptr) = unsafeForeignPtrToPtr fptr
----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
withTypeArray  inp = withArray (fmap fromType inp)
withValueArray inp = withArray (fmap fromValue inp)

cFromEnum :: (Enum a, Integral i) => a -> i
cFromEnum = fromIntegral . fromEnum

cFromInt :: (Integral a, Integral b) => a -> b 
cFromInt = fromIntegral

fromFunPtr = castFunPtrToPtr 

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------
data LIBJITException = LIBJITException (Maybe Int) String
                    deriving (Eq, Show,Typeable) 

instance Exception LIBJITException

throwRaw :: String -> Ptr () -> IO ()
throwRaw str ptr =
  do
    when (nullPtr == ptr)
      $ throwIO (LIBJITException Nothing str)

throwOnBadValue :: Value -> IO Value 
throwOnBadValue v = throwRaw "Value is null" (fromValue v) >> return v 

throwOnBadContext c = throwRaw "Context is null" (fromContext c) >> return c 
                    

throwOnError :: Int -> IO ()
throwOnError i =
  do
    when (i /= 0)
      $ throwIO (LIBJITException (Just i) "Returned error code")  
----------------------------------------------------------------------------
-- Enums 
----------------------------------------------------------------------------


{# enum jit_abi_t as ABI { jit_abi_cdecl as CDECL
                         , jit_abi_vararg as VARARG
                         , jit_abi_stdcall as STDCALL
                         , jit_abi_fastcall as FASTCALL } #}

-- These are just defines in the C library.
#c 
enum CALL_FLAG_ENUM {
  call_nothrow = JIT_CALL_NOTHROW,
  call_noreturn = JIT_CALL_NORETURN,
  call_tail = JIT_CALL_TAIL
}; 

#endc 

{# enum CALL_FLAG_ENUM as CallFlag {underscoreToCase} deriving (Eq,Show) #} 


----------------------------------------------------------------------------
-- Functions 
----------------------------------------------------------------------------
contextCreate = contextCreate' >>= throwOnBadContext 

{# fun unsafe jit_context_create as contextCreate' 
   { } -> `Context' Context #} 

{# fun unsafe jit_context_build_start as startBuild 
   { fromContext `Context' } -> `()' #}

{# fun unsafe jit_context_build_end as endBuild 
   { fromContext `Context' } -> `()' #}

{# fun unsafe jit_type_create_signature as createTypeSignature
   { cFromEnum `ABI' ,
     fromType  `Type' ,
     withTypeArray* `[Type]' ,
     cFromInt  `Int' ,
     cFromInt  `Int' } -> `Type' Type #} 
     
{# fun unsafe jit_function_create as createFunction
   { fromContext `Context' ,
     fromType    `Type' } -> `Function' Function #} 

{# fun unsafe jit_value_get_param as getParam 
   { fromFunction `Function' ,
     cFromInt     `Int' } -> `Value' Value #}

{# fun unsafe jit_function_compile as compile 
   { fromFunction `Function' } -> `()' #} 

{# fun unsafe jit_function_apply as apply 
   { fromFunction `Function' ,
     withArray*   `[Ptr ()]' ,
            id    `Ptr ()' } -> `()' #}  

----------------------------------------------------------------------------
-- Labels
----------------------------------------------------------------------------
getUndefinedLabel =
 do
   value <- getUndefLab
   lab <- mallocForeignPtr 
   poke  (unsafeForeignPtrToPtr lab) value
   return (Label lab)

{# fun unsafe getUndefinedLabel as getUndefLab 
    { } -> `CULong' cFromInt #} 

----------------------------------------------------------------------------
-- Instructions 
----------------------------------------------------------------------------

-- TODO: functions that do not return "Value" returns an Int
--       check this int for error conditions ?
-- TODO: The return value can probably be NULL for failed 
--       attempt to generate a instr. Check if Value ptr is null. 

-- Arithmetic 
{# fun unsafe jit_insn_mul as mul
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' Value #} 

{# fun unsafe jit_insn_add as add
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' Value #} 

{# fun unsafe jit_insn_sub as sub
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' Value #} 

-- Comparisons
{# fun jit_insn_lt as lt
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' Value #} 

{# fun jit_insn_eq as eq
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' Value #} 

-- Branch
{# fun jit_insn_branch_if_not as branchIfNot
   { fromFunction `Function'  ,
     fromValue    `Value'     ,
     fromLabel    `Label' } -> `()' #}

-- Return 
{# fun unsafe jit_insn_return as ret 
   { fromFunction `Function' ,
     fromValue    `Value' } -> `()' #}  

-- Function calls
-- TODO: Figure this one out (how to use it) 
{# fun unsafe jit_insn_call_native as callNativeFunction 
   { fromFunction `Function'   ,
     withCString* `String'     ,
     fromFunPtr   `FunPtr a'   ,
     fromType     `Type'       ,
     withValueArray* `[Value]' ,
     cFromInt     `Int'        ,
     cFromEnum     `CallFlag' } -> `Value' Value #}

{# fun unsafe jit_insn_call as callFunction 
   { fromFunction `Function'   ,
     withCString* `String'     ,
     fromFunction `Function'   ,
     fromType     `Type'       ,
     withValueArray* `[Value]' ,
     cFromInt     `Int'        ,
     cFromEnum    `CallFlag'  } -> `Value' Value #} 
     

-- Labels
{# fun unsafe jit_insn_label as setLabel 
   { fromFunction `Function'   ,
     fromLabel    `Label'  } -> `()' #}

----------------------------------------------------------------------------
-- TODO: One of these for each type 
{# fun unsafe get_void_type as getVoidType 
   {  } -> `Type' Type #}  

{# fun unsafe get_sbyte_type as getSByteType 
   {  } -> `Type' Type #}  

{# fun unsafe get_ubyte_type as getUByteType 
   {  } -> `Type' Type #}  

{# fun unsafe get_int_type as getIntType 
   {  } -> `Type' Type #}  

{# fun unsafe get_uint_type as getUIntType 
   {  } -> `Type' Type #}  

{# fun unsafe get_long_type as getLongType 
   {  } -> `Type' Type #}  

{# fun unsafe get_ulong_type as getULongType 
   {  } -> `Type' Type #}  

{# fun unsafe get_float32_type as getFloat32Type 
   {  } -> `Type' Type #}  

{# fun unsafe get_float64_type as getFloat64Type 
   {  } -> `Type' Type #}  

