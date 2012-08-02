

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

throwOnBadFunction :: Function -> IO Function
throwOnBadFunction f = throwRaw "Function is null" (fromFunction f) >> return f 

throwOnBadContext c = throwRaw "Context is null" (fromContext c) >> return c 
                    
-- Libjit uses the dotNET,dotGNU,windows tradition
-- of having 1 = Success 
--           0 = Failure
throwOnError :: Int -> IO ()
throwOnError i =
  do
    when (i /= 1)
      $ throwIO (LIBJITException (Just i) "Returned error code")

checkedValue v = throwOnBadValue (Value v) 
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
-- Context  (jit-context.h) 
----------------------------------------------------------------------------
contextCreate = contextCreate' >>= throwOnBadContext 

{# fun unsafe jit_context_create as contextCreate' 
   { } -> `Context' Context #} 

{# fun unsafe jit_context_destroy as contextDestroy 
   { fromContext `Context' } -> `()' #} 

-- TODO: figure out what to do with this one
{# fun unsafe jit_context_supports_threads as contextSupportsThreads 
   { fromContext `Context' } -> `Int' cFromInt #} 

{# fun unsafe jit_context_build_start as startBuild 
   { fromContext `Context' } -> `()' #}

{# fun unsafe jit_context_build_end as endBuild 
   { fromContext `Context' } -> `()' #}

{- 
DONE: jit_context_t jit_context_create(void) JIT_NOTHROW;
DONE: void jit_context_destroy(jit_context_t context) JIT_NOTHROW;
IN PROGRESS: int jit_context_supports_threads(jit_context_t context) JIT_NOTHROW;
DONE: void jit_context_build_start(jit_context_t context) JIT_NOTHROW;
DONE: void jit_context_build_end(jit_context_t context) JIT_NOTHROW;
void jit_context_set_on_demand_driver(
	jit_context_t context,
	jit_on_demand_driver_func driver) JIT_NOTHROW;
int jit_context_set_meta
	(jit_context_t context, int type, void *data,
	 jit_meta_free_func free_data) JIT_NOTHROW;
int jit_context_set_meta_numeric
	(jit_context_t context, int type, jit_nuint data) JIT_NOTHROW;
void *jit_context_get_meta(jit_context_t context, int type) JIT_NOTHROW;
jit_nuint jit_context_get_meta_numeric
	(jit_context_t context, int type) JIT_NOTHROW;
void jit_context_free_meta(jit_context_t context, int type) JIT_NOTHROW;
-} 

----------------------------------------------------------------------------
-- Functions (jit-function.h) 
----------------------------------------------------------------------------
{# fun unsafe jit_type_create_signature as createTypeSignature
   { cFromEnum `ABI' ,
     fromType  `Type' ,
     withTypeArray* `[Type]' ,
     cFromInt  `Int' ,
     cFromInt  `Int' } -> `Type' Type #} 
     
createFunction c t = createFunction' c t >>= throwOnBadFunction 

{# fun unsafe jit_function_create as createFunction'
   { fromContext `Context' ,
     fromType    `Type' } -> `Function' Function #} 

createNestedFunction c t f = createNestedFunction' c t f >>= throwOnBadFunction 

{# fun unsafe jit_function_create_nested as createNestedFunction'
   { fromContext  `Context' ,
     fromType     `Type'    ,
     fromFunction `Function' } -> `Function' Function #} 

{#fun unsafe jit_function_abandon as abandonFunction 
   { fromFunction `Function' } -> `()' #} 

{# fun unsafe jit_value_get_param as getParam 
   { fromFunction `Function' ,
     cFromInt     `Int' } -> `Value' Value #}

{# fun unsafe jit_function_compile as compile 
   { fromFunction `Function' } -> `()' #} 

{# fun unsafe jit_function_apply as apply 
   { fromFunction `Function' ,
     withArray*   `[Ptr ()]' ,
            id    `Ptr ()' } -> `()' #} 
{-  
DONE: jit_function_t jit_function_create
	(jit_context_t context, jit_type_t signature) JIT_NOTHROW;
DONE: jit_function_t jit_function_create_nested
	(jit_context_t context, jit_type_t signature,
	 jit_function_t parent) JIT_NOTHROW;
DONE: void jit_function_abandon(jit_function_t func) JIT_NOTHROW;
jit_context_t jit_function_get_context(jit_function_t func) JIT_NOTHROW;
jit_type_t jit_function_get_signature(jit_function_t func) JIT_NOTHROW;
int jit_function_set_meta
	(jit_function_t func, int type, void *data,
	 jit_meta_free_func free_data, int build_only) JIT_NOTHROW;
void *jit_function_get_meta(jit_function_t func, int type) JIT_NOTHROW;
void jit_function_free_meta(jit_function_t func, int type) JIT_NOTHROW;
jit_function_t jit_function_next
	(jit_context_t context, jit_function_t prev) JIT_NOTHROW;
jit_function_t jit_function_previous
	(jit_context_t context, jit_function_t prev) JIT_NOTHROW;
jit_block_t jit_function_get_entry(jit_function_t func) JIT_NOTHROW;
jit_block_t jit_function_get_current(jit_function_t func) JIT_NOTHROW;
jit_function_t jit_function_get_nested_parent(jit_function_t func) JIT_NOTHROW;
int jit_function_compile(jit_function_t func) JIT_NOTHROW;
int jit_function_is_compiled(jit_function_t func) JIT_NOTHROW;
void jit_function_set_recompilable(jit_function_t func) JIT_NOTHROW;
void jit_function_clear_recompilable(jit_function_t func) JIT_NOTHROW;
int jit_function_is_recompilable(jit_function_t func) JIT_NOTHROW;
int jit_function_compile_entry(jit_function_t func, void **entry_point) JIT_NOTHROW;
void jit_function_setup_entry(jit_function_t func, void *entry_point) JIT_NOTHROW;
void *jit_function_to_closure(jit_function_t func) JIT_NOTHROW;
jit_function_t jit_function_from_closure
	(jit_context_t context, void *closure) JIT_NOTHROW;
jit_function_t jit_function_from_pc
	(jit_context_t context, void *pc, void **handler) JIT_NOTHROW;
void *jit_function_to_vtable_pointer(jit_function_t func) JIT_NOTHROW;
jit_function_t jit_function_from_vtable_pointer
	(jit_context_t context, void *vtable_pointer) JIT_NOTHROW;
void jit_function_set_on_demand_compiler
	(jit_function_t func, jit_on_demand_func on_demand) JIT_NOTHROW;
jit_on_demand_func jit_function_get_on_demand_compiler(jit_function_t func) JIT_NOTHROW;
int jit_function_apply
	(jit_function_t func, void **args, void *return_area);
int jit_function_apply_vararg
	(jit_function_t func, jit_type_t signature, void **args, void *return_area);
void jit_function_set_optimization_level
	(jit_function_t func, unsigned int level) JIT_NOTHROW;
unsigned int jit_function_get_optimization_level
	(jit_function_t func) JIT_NOTHROW;
unsigned int jit_function_get_max_optimization_level(void) JIT_NOTHROW;
jit_label_t jit_function_reserve_label(jit_function_t func) JIT_NOTHROW;
-} 
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

-- Arithmetic 
{# fun unsafe jit_insn_mul as mul
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_add as add
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_sub as sub
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

-- Comparisons
{# fun jit_insn_lt as lt
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_eq as eq
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

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
     cFromEnum     `CallFlag' } -> `Value' checkedValue* #}

{# fun unsafe jit_insn_call as callFunction 
   { fromFunction `Function'   ,
     withCString* `String'     ,
     fromFunction `Function'   ,
     fromType     `Type'       ,
     withValueArray* `[Value]' ,
     cFromInt     `Int'        ,
     cFromEnum    `CallFlag'  } -> `Value' checkedValue* #} 
     

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

