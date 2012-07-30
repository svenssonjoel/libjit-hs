#include <jit/jit.h> 


/* -- Types -- */ 

#define JIT_TYPE(x)  \
  jit_type_t get_ ## x ## _type(){ \
  return jit_type_ ## x; \
  }

JIT_TYPE(void);
JIT_TYPE(sbyte);
JIT_TYPE(ubyte);
JIT_TYPE(int);
JIT_TYPE(uint);
JIT_TYPE(long);
JIT_TYPE(ulong);
JIT_TYPE(float32);
JIT_TYPE(float64);




/* -- LABELS -- */ 

jit_label_t getUndefinedLabel() { 
  return jit_label_undefined;
} 

