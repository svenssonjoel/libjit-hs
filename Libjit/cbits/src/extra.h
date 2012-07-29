
#ifndef LIBJIT_HS_EXTRA_H_ 
#define LIBJIT_HS_EXTRA_H_ 


#define JIT_TYPE_PROTO(x) extern jit_type_t get_ ## x ## _type(); 

JIT_TYPE_PROTO(int);
JIT_TYPE_PROTO(long);
JIT_TYPE_PROTO(float32);
JIT_TYPE_PROTO(float64);

/* -- LABELS -- */

jit_label_t getUndefinedLabel();


#endif
