#include <jit/jit.h> 


#define JIT_TYPE(x)  \
  jit_type_t get_ ## x ## _type(){ \
  return jit_type_ ## x; \
  }

JIT_TYPE(int);
JIT_TYPE(long);
JIT_TYPE(float32);
JIT_TYPE(float64);


