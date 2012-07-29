

{-# LANGUAGE ForeignFunctionInterface #-} 


module Libjit.Raw  where 

import Foreign.Ptr 
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

-- TODO: Figure this one out
{# fun unsafe jit_insn_call_native as callNative 
   { fromFunction `Function'   ,
     withCString* `String'     ,
     id           `Ptr ()'     ,
     fromType     `Type'       ,
     withValueArray* `[Value]' ,
     cFromInt     `Int'        ,
     cFromInt     `Int'   } -> `Value' Value #}

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

