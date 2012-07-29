

{-# LANGUAGE ForeignFunctionInterface #-} 


module Libjit.Raw  where 

import Foreign.Ptr
import Foreign.ForeignPtr 
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

import Data.Word



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

----------------------------------------------------------------------------
-- Enums 
----------------------------------------------------------------------------


{# enum jit_abi_t as ABI { jit_abi_cdecl as CDECL
                         , jit_abi_vararg as VARARG
                         , jit_abi_stdcall as STDCALL
                         , jit_abi_fastcall as FASTCALL } #}


----------------------------------------------------------------------------
-- Functions 
----------------------------------------------------------------------------
{# fun unsafe jit_context_create as contextCreate 
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

{# fun unsafe getUndefinedLabel as getUndefLab 
    { } -> `Word64' cFromInt #} 

----------------------------------------------------------------------------
-- Instructions 
----------------------------------------------------------------------------
{# fun unsafe jit_insn_mul as mul
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' Value #} 

{# fun unsafe jit_insn_add as add
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' Value #} 

{# fun unsafe jit_insn_return as ret 
   { fromFunction `Function' ,
     fromValue    `Value' } -> `()' #}  

-- TODO: Figure this one out (how to use it) 
{# fun unsafe jit_insn_call_native as callNative 
   { fromFunction `Function'   ,
     withCString* `String'     ,
     id           `Ptr ()'     ,
     fromType     `Type'       ,
     withValueArray* `[Value]' ,
     cFromInt     `Int'        ,
     cFromInt     `Int'   } -> `Value' Value #}


{# fun unsafe jit_insn_label as setLabel 
   { fromFunction `Function'   ,
     fromLabel    `Label'  } -> `()' #}

----------------------------------------------------------------------------
-- TODO: One of these for each type 

{# fun unsafe get_int_type as getIntType 
   {  } -> `Type' Type #}  

{# fun unsafe get_long_type as getLongType 
   {  } -> `Type' Type #}  


{# fun unsafe get_float32_type as getFloat32Type 
   {  } -> `Type' Type #}  

{# fun unsafe get_float64_type as getFloat64Type 
   {  } -> `Type' Type #}  

