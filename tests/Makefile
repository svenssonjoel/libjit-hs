

all: mul_add gcd call_native

mul_add: mul_add.hs 
	ghc --make mul_add.hs ../Libjit/cbits/o/extra.o -ljit -i../

gcd: gcd.hs
	ghc --make gcd.hs ../Libjit/cbits/o/extra.o -ljit -i../


call_native: call_native.hs
	ghc --make call_native.hs ../Libjit/cbits/o/extra.o -ljit -i../


