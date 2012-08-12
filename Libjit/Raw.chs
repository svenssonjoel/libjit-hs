

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
import Data.Bits

import Control.Exception
import Control.Monad

import System.IO.Unsafe

#include <jit/jit.h>

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------
newtype Context  = Context {fromContext :: Ptr ()} 
newtype Type     = Type {fromType :: Ptr ()} 
newtype Function = Function {fromFunction :: Ptr ()}
newtype Value    = Value {fromValue :: Ptr ()}
newtype Block    = Block {fromBlock :: Ptr ()} 

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

intToBool i =  if i == 1 then True else False 
  
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
-- BaseTypes (these are constants defined in jit-type.c) 
----------------------------------------------------------------------------
void_type = Type (unsafePerformIO (peek voidT'))
foreign import ccall "jit/jit.h &jit_type_void" voidT' :: Ptr (Ptr ())  

sbyte_type = Type (unsafePerformIO (peek sbyteT'))
foreign import ccall "jit/jit.h &jit_type_sbyte" sbyteT' :: Ptr (Ptr ()) 

ubyte_type = Type (unsafePerformIO (peek ubyteT'))
foreign import ccall "jit/jit.h &jit_type_ubyte" ubyteT' :: Ptr (Ptr ()) 

short_type = Type (unsafePerformIO (peek shortT'))
foreign import ccall "jit/jit.h &jit_type_short" shortT' :: Ptr (Ptr ()) 

ushort_type = Type (unsafePerformIO (peek ushortT'))
foreign import ccall "jit/jit.h &jit_type_ushort" ushortT' :: Ptr (Ptr ()) 

int_type = Type (unsafePerformIO (peek intT'))
foreign import ccall "jit/jit.h &jit_type_int" intT' :: Ptr (Ptr ()) 

uint_type = Type (unsafePerformIO (peek uintT'))
foreign import ccall "jit/jit.h &jit_type_uint" uintT' :: Ptr (Ptr ()) 

nint_type = Type (unsafePerformIO (peek nintT'))
foreign import ccall "jit/jit.h &jit_type_nint" nintT' :: Ptr (Ptr ()) 

nuint_type = Type (unsafePerformIO (peek nuintT'))
foreign import ccall "jit/jit.h &jit_type_nuint" nuintT' :: Ptr (Ptr ()) 

long_type = Type (unsafePerformIO (peek longT'))
foreign import ccall "jit/jit.h &jit_type_long" longT' :: Ptr (Ptr ()) 

ulong_type = Type (unsafePerformIO (peek ulongT'))
foreign import ccall "jit/jit.h &jit_type_ulong" ulongT' :: Ptr (Ptr ()) 

float32_type = Type (unsafePerformIO (peek float32T'))
foreign import ccall "jit/jit.h &jit_type_float32" float32T' :: Ptr (Ptr ()) 

float64_type = Type (unsafePerformIO (peek float64T'))
foreign import ccall "jit/jit.h &jit_type_float64" float64T' :: Ptr (Ptr ()) 

void_ptr_type = Type (unsafePerformIO (peek void_ptrT'))
foreign import ccall "jit/jit.h &jit_type_void_ptr" void_ptrT' :: Ptr (Ptr ()) 

----------------------------------------------------------------------------
-- Undefined label 
----------------------------------------------------------------------------

-- TODO: very skeptic to these labels.
--       I do not think I am doing it right. 
getUndefinedLabel :: IO Label
getUndefinedLabel =
  do 
    fptr <- mallocForeignPtr
    let ptr = unsafeForeignPtrToPtr fptr
    poke ptr (0xFFFFFFFF :: CULong)
    return $ Label fptr

makeLabel :: CULong -> IO Label
makeLabel l =
  do 
    fptr <- mallocForeignPtr
    let ptr = unsafeForeignPtrToPtr fptr
    poke ptr l
    return $ Label fptr

----------------------------------------------------------------------------
-- Context  (jit-context.h) 
----------------------------------------------------------------------------
contextCreate = contextCreate' >>= throwOnBadContext 

{# fun unsafe jit_context_create as contextCreate' 
   { } -> `Context' Context #} 

{# fun unsafe jit_context_destroy as contextDestroy 
   { fromContext `Context' } -> `()' #} 

-- DONE: figure out what to do with this one (1 means True, 0 means false) 
{# fun unsafe jit_context_supports_threads as contextSupportsThreads 
   { fromContext `Context' } -> `Bool' intToBool #} 

{# fun unsafe jit_context_build_start as startBuild 
   { fromContext `Context' } -> `()' #}

{# fun unsafe jit_context_build_end as endBuild 
   { fromContext `Context' } -> `()' #}

{- 
DONE: jit_context_t jit_context_create(void) JIT_NOTHROW;
DONE: void jit_context_destroy(jit_context_t context) JIT_NOTHROW;
DONE: int jit_context_supports_threads(jit_context_t context) JIT_NOTHROW;
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

{# fun unsafe jit_function_get_context as getFunctionContext
   { fromFunction `Function' } -> `Context' Context #} 

{# fun unsafe jit_function_get_signature as getFunctionSignature
   { fromFunction `Function' } -> `Type' Type #} 

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

{# fun unsafe jit_function_get_entry as getFunctionEntry 
   { fromFunction `Function' } -> `Block' Block #} 

{# fun unsafe jit_function_get_current as getFunctionCurrent 
   { fromFunction `Function' } -> `Block' Block #} 

{# fun unsafe jit_function_is_compiled as isCompiled 
   { fromFunction `Function' } -> `Bool' intToBool #} 

{# fun unsafe jit_function_set_optimization_level as setOptLevel 
   { fromFunction `Function' ,
     cFromInt     `Int'  } -> `()' id #} 

{# fun unsafe jit_function_get_optimization_level as getOptLevel 
   { fromFunction `Function' } -> `Int' cFromInt #} 


{# fun pure unsafe jit_function_get_max_optimization_level as maxOptLevel 
   { } -> `Int' cFromInt #} 

{# fun unsafe jit_function_reserve_label as reserveLabel 
   { fromFunction `Function' } -> `Label' makeLabel* #} 

{-  
DONE: jit_function_t jit_function_create
	(jit_context_t context, jit_type_t signature) JIT_NOTHROW;
DONE: jit_function_t jit_function_create_nested
	(jit_context_t context, jit_type_t signature,
	 jit_function_t parent) JIT_NOTHROW;
DONE: void jit_function_abandon(jit_function_t func) JIT_NOTHROW;
DONE: jit_context_t jit_function_get_context(jit_function_t func) JIT_NOTHROW;
DONE: jit_type_t jit_function_get_signature(jit_function_t func) JIT_NOTHROW;
int jit_function_set_meta
	(jit_function_t func, int type, void *data,
	 jit_meta_free_func free_data, int build_only) JIT_NOTHROW;
void *jit_function_get_meta(jit_function_t func, int type) JIT_NOTHROW;
void jit_function_free_meta(jit_function_t func, int type) JIT_NOTHROW;
jit_function_t jit_function_next
	(jit_context_t context, jit_function_t prev) JIT_NOTHROW;
jit_function_t jit_function_previous
	(jit_context_t context, jit_function_t prev) JIT_NOTHROW;
DONE: jit_block_t jit_function_get_entry(jit_function_t func) JIT_NOTHROW;
DONE: jit_block_t jit_function_get_current(jit_function_t func) JIT_NOTHROW;
jit_function_t jit_function_get_nested_parent(jit_function_t func) JIT_NOTHROW;
DONE: int jit_function_compile(jit_function_t func) JIT_NOTHROW;
DONE: int jit_function_is_compiled(jit_function_t func) JIT_NOTHROW;
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
DONE: int jit_function_apply
	(jit_function_t func, void **args, void *return_area);
int jit_function_apply_vararg
	(jit_function_t func, jit_type_t signature, void **args, void *return_area);
DONE: void jit_function_set_optimization_level
	(jit_function_t func, unsigned int level) JIT_NOTHROW;
DONE: unsigned int jit_function_get_optimization_level
	(jit_function_t func) JIT_NOTHROW;
DONE: unsigned int jit_function_get_max_optimization_level(void) JIT_NOTHROW;
DONE: jit_label_t jit_function_reserve_label(jit_function_t func) JIT_NOTHROW;
-} 

----------------------------------------------------------------------------
-- Instructions (jit-insn.h)  
----------------------------------------------------------------------------

-- Math 
{# fun unsafe jit_insn_mul as mul
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_mul_ovf as mul_ovf
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_add as add
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_add_ovf as add_ovf
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_sub as sub
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_sub_ovf as sub_ovf
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_div as div
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_rem as rem
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_rem_ieee as rem_ieee
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_neg as neg
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_acos as acos
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_asin as asin
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_atan as atan
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_atan2 as atan2
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_ceil as ceil
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_cos as cosh
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_exp as exp
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_floor as floor
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_log as log
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_log10 as log10
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_pow as pow
   { fromFunction `Function' ,
     fromValue    `Value' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_rint as rint
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_round as round
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_sin as sin
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_sinh as sinh
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_sqrt as sqrt
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_tan as tan
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 

{# fun unsafe jit_insn_tanh as tanh
   { fromFunction `Function' ,
     fromValue    `Value' } -> `Value' checkedValue* #} 


-- Comparisons and boolean 
{# fun jit_insn_lt as lt
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_le as le
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_gt as gt
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_ge as ge
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_cmpl as cmpl
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_cmpg as cmpg
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 


{# fun jit_insn_eq as eq
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_ne as ne
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 


{# fun jit_insn_and as and
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_or as or
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_xor as xor
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_not as not
   { fromFunction  `Function' ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_shl as shl
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_shr as shr
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_sshr as sshr
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 

{# fun jit_insn_ushr as ushr
   { fromFunction  `Function' ,
     fromValue     `Value'    ,
     fromValue     `Value' } -> `Value' checkedValue* #} 


-- Branch
{# fun jit_insn_branch as branch
   { fromFunction `Function'  ,
     fromLabel    `Label' } -> `()' #}

{# fun jit_insn_branch_if as branchIf
   { fromFunction `Function'  ,
     fromValue    `Value'     ,
     fromLabel    `Label' } -> `()' #}

{# fun jit_insn_branch_if_not as branchIfNot
   { fromFunction `Function'  ,
     fromValue    `Value'     ,
     fromLabel    `Label' } -> `()' #}

-- Return 
{# fun unsafe jit_insn_return as ret 
   { fromFunction `Function' ,
     fromValue    `Value' } -> `()' #}  

-- Function calls

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

{- 
int jit_insn_get_opcode(jit_insn_t insn) JIT_NOTHROW;
jit_value_t jit_insn_get_dest(jit_insn_t insn) JIT_NOTHROW;
jit_value_t jit_insn_get_value1(jit_insn_t insn) JIT_NOTHROW;
jit_value_t jit_insn_get_value2(jit_insn_t insn) JIT_NOTHROW;
jit_label_t jit_insn_get_label(jit_insn_t insn) JIT_NOTHROW;
jit_function_t jit_insn_get_function(jit_insn_t insn) JIT_NOTHROW;
void *jit_insn_get_native(jit_insn_t insn) JIT_NOTHROW;
const char *jit_insn_get_name(jit_insn_t insn) JIT_NOTHROW;
jit_type_t jit_insn_get_signature(jit_insn_t insn) JIT_NOTHROW;
int jit_insn_dest_is_value(jit_insn_t insn) JIT_NOTHROW;

DONE: int jit_insn_label(jit_function_t func, jit_label_t *label) JIT_NOTHROW;
int jit_insn_new_block(jit_function_t func) JIT_NOTHROW;
jit_value_t jit_insn_load(jit_function_t func, jit_value_t value) JIT_NOTHROW;
jit_value_t jit_insn_dup(jit_function_t func, jit_value_t value) JIT_NOTHROW;
jit_value_t jit_insn_load_small
	(jit_function_t func, jit_value_t value) JIT_NOTHROW;
int jit_insn_store
	(jit_function_t func, jit_value_t dest, jit_value_t value) JIT_NOTHROW;
jit_value_t jit_insn_load_relative
	(jit_function_t func, jit_value_t value,
	 jit_nint offset, jit_type_t type) JIT_NOTHROW;
int jit_insn_store_relative
	(jit_function_t func, jit_value_t dest,
	 jit_nint offset, jit_value_t value) JIT_NOTHROW;
jit_value_t jit_insn_add_relative
	(jit_function_t func, jit_value_t value, jit_nint offset) JIT_NOTHROW;
jit_value_t jit_insn_load_elem
	(jit_function_t func, jit_value_t base_addr,
	 jit_value_t index, jit_type_t elem_type) JIT_NOTHROW;
jit_value_t jit_insn_load_elem_address
	(jit_function_t func, jit_value_t base_addr,
	 jit_value_t index, jit_type_t elem_type) JIT_NOTHROW;
int jit_insn_store_elem
	(jit_function_t func, jit_value_t base_addr,
	 jit_value_t index, jit_value_t value) JIT_NOTHROW;
int jit_insn_check_null(jit_function_t func, jit_value_t value) JIT_NOTHROW;

DONE: jit_value_t jit_insn_add
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_add_ovf
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_sub
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_sub_ovf
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_mul
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_mul_ovf
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_div
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_rem
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_rem_ieee
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_neg
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_and
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_or
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_xor
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_not
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_shl
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DOME: jit_value_t jit_insn_shr
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_ushr
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_sshr
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_eq
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_ne
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_lt
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_le
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_gt
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_ge
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_cmpl
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_cmpg
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
jit_value_t jit_insn_to_bool
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
jit_value_t jit_insn_to_not_bool
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_acos
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_asin
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_atan
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_atan2
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_ceil
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_cos
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_cosh
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_exp
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_floor
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_log
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_log10
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_pow
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
DONE: jit_value_t jit_insn_rint
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_round
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_sin
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_sinh
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_sqrt
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_tan
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: jit_value_t jit_insn_tanh
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
jit_value_t jit_insn_is_nan
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
jit_value_t jit_insn_is_finite
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
jit_value_t jit_insn_is_inf
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
jit_value_t jit_insn_abs
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
jit_value_t jit_insn_min
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
jit_value_t jit_insn_max
	(jit_function_t func, jit_value_t value1, jit_value_t value2) JIT_NOTHROW;
jit_value_t jit_insn_sign
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
DONE: int jit_insn_branch
	(jit_function_t func, jit_label_t *label) JIT_NOTHROW;
DONE: int jit_insn_branch_if
	(jit_function_t func, jit_value_t value, jit_label_t *label) JIT_NOTHROW;
DONE: int jit_insn_branch_if_not
	(jit_function_t func, jit_value_t value, jit_label_t *label) JIT_NOTHROW;
int jit_insn_jump_table
	(jit_function_t func, jit_value_t value,
	 jit_label_t *labels, unsigned int num_labels) JIT_NOTHROW;
jit_value_t jit_insn_address_of
	(jit_function_t func, jit_value_t value1) JIT_NOTHROW;
jit_value_t jit_insn_address_of_label
	(jit_function_t func, jit_label_t *label) JIT_NOTHROW;
jit_value_t jit_insn_convert
	(jit_function_t func, jit_value_t value,
	 jit_type_t type, int overflow_check) JIT_NOTHROW;

DONE: jit_value_t jit_insn_call
	(jit_function_t func, const char *name,
	 jit_function_t jit_func, jit_type_t signature,
	 jit_value_t *args, unsigned int num_args, int flags) JIT_NOTHROW;
jit_value_t jit_insn_call_indirect
	(jit_function_t func, jit_value_t value, jit_type_t signature,
	 jit_value_t *args, unsigned int num_args, int flags) JIT_NOTHROW;
jit_value_t jit_insn_call_indirect_vtable
	(jit_function_t func, jit_value_t value, jit_type_t signature,
	 jit_value_t *args, unsigned int num_args, int flags) JIT_NOTHROW;
DONE: jit_value_t jit_insn_call_native
	(jit_function_t func, const char *name,
	 void *native_func, jit_type_t signature,
	 jit_value_t *args, unsigned int num_args, int flags) JIT_NOTHROW;
jit_value_t jit_insn_call_intrinsic
	(jit_function_t func, const char *name, void *intrinsic_func,
	 const jit_intrinsic_descr_t *descriptor,
	 jit_value_t arg1, jit_value_t arg2) JIT_NOTHROW;
int jit_insn_incoming_reg
	(jit_function_t func, jit_value_t value, int reg) JIT_NOTHROW;
int jit_insn_incoming_frame_posn
	(jit_function_t func, jit_value_t value, jit_nint frame_offset) JIT_NOTHROW;
int jit_insn_outgoing_reg
	(jit_function_t func, jit_value_t value, int reg) JIT_NOTHROW;
int jit_insn_outgoing_frame_posn
	(jit_function_t func, jit_value_t value, jit_nint frame_offset) JIT_NOTHROW;
int jit_insn_return_reg
	(jit_function_t func, jit_value_t value, int reg) JIT_NOTHROW;
int jit_insn_setup_for_nested
	(jit_function_t func, int nested_level, int reg) JIT_NOTHROW;
int jit_insn_flush_struct(jit_function_t func, jit_value_t value) JIT_NOTHROW;
jit_value_t jit_insn_import
	(jit_function_t func, jit_value_t value) JIT_NOTHROW;
int jit_insn_push(jit_function_t func, jit_value_t value) JIT_NOTHROW;
int jit_insn_push_ptr
	(jit_function_t func, jit_value_t value, jit_type_t type) JIT_NOTHROW;
int jit_insn_set_param
	(jit_function_t func, jit_value_t value, jit_nint offset) JIT_NOTHROW;
int jit_insn_set_param_ptr
	(jit_function_t func, jit_value_t value, jit_type_t type,
	 jit_nint offset) JIT_NOTHROW;
int jit_insn_push_return_area_ptr(jit_function_t func) JIT_NOTHROW;
int jit_insn_pop_stack(jit_function_t func, jit_nint num_items) JIT_NOTHROW;
int jit_insn_defer_pop_stack
	(jit_function_t func, jit_nint num_items) JIT_NOTHROW;
int jit_insn_flush_defer_pop
	(jit_function_t func, jit_nint num_items) JIT_NOTHROW;
DONE: int jit_insn_return(jit_function_t func, jit_value_t value) JIT_NOTHROW;
int jit_insn_return_ptr
	(jit_function_t func, jit_value_t value, jit_type_t type) JIT_NOTHROW;
int jit_insn_default_return(jit_function_t func) JIT_NOTHROW;
int jit_insn_throw(jit_function_t func, jit_value_t value) JIT_NOTHROW;
jit_value_t jit_insn_get_call_stack(jit_function_t func) JIT_NOTHROW;

jit_value_t jit_insn_thrown_exception(jit_function_t func) JIT_NOTHROW;
int jit_insn_uses_catcher(jit_function_t func) JIT_NOTHROW;
jit_value_t jit_insn_start_catcher(jit_function_t func) JIT_NOTHROW;
int jit_insn_branch_if_pc_not_in_range
	(jit_function_t func, jit_label_t start_label,
	 jit_label_t end_label, jit_label_t *label) JIT_NOTHROW;
int jit_insn_rethrow_unhandled(jit_function_t func) JIT_NOTHROW;
int jit_insn_start_finally
	(jit_function_t func, jit_label_t *finally_label) JIT_NOTHROW;
int jit_insn_return_from_finally(jit_function_t func) JIT_NOTHROW;
int jit_insn_call_finally
	(jit_function_t func, jit_label_t *finally_label) JIT_NOTHROW;
jit_value_t jit_insn_start_filter
	(jit_function_t func, jit_label_t *label, jit_type_t type) JIT_NOTHROW;
int jit_insn_return_from_filter
	(jit_function_t func, jit_value_t value) JIT_NOTHROW;
jit_value_t jit_insn_call_filter
	(jit_function_t func, jit_label_t *label,
	 jit_value_t value, jit_type_t type) JIT_NOTHROW;

int jit_insn_memcpy
	(jit_function_t func, jit_value_t dest,
	 jit_value_t src, jit_value_t size) JIT_NOTHROW;
int jit_insn_memmove
	(jit_function_t func, jit_value_t dest,
	 jit_value_t src, jit_value_t size) JIT_NOTHROW;
int jit_insn_memset
	(jit_function_t func, jit_value_t dest,
	 jit_value_t value, jit_value_t size) JIT_NOTHROW;
jit_value_t jit_insn_alloca
	(jit_function_t func, jit_value_t size) JIT_NOTHROW;

int jit_insn_move_blocks_to_end
	(jit_function_t func, jit_label_t from_label, jit_label_t to_label)
		JIT_NOTHROW;
int jit_insn_move_blocks_to_start
	(jit_function_t func, jit_label_t from_label, jit_label_t to_label)
		JIT_NOTHROW;

int jit_insn_mark_offset
	(jit_function_t func, jit_int offset) JIT_NOTHROW;
int jit_insn_mark_breakpoint
	(jit_function_t func, jit_nint data1, jit_nint data2) JIT_NOTHROW;
int jit_insn_mark_breakpoint_variable
	(jit_function_t func, jit_value_t data1, jit_value_t data2) JIT_NOTHROW;

void jit_insn_iter_init(jit_insn_iter_t *iter, jit_block_t block) JIT_NOTHROW;
void jit_insn_iter_init_last
	(jit_insn_iter_t *iter, jit_block_t block) JIT_NOTHROW;
jit_insn_t jit_insn_iter_next(jit_insn_iter_t *iter) JIT_NOTHROW;
jit_insn_t jit_insn_iter_previous(jit_insn_iter_t *iter) JIT_NOTHROW;
-} 
