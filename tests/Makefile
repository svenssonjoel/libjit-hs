

all: mul_add gcd call_native

mul_add: mul_add.hs 
	ghc --make mul_add.hs -ljit -i../

gcd: gcd.hs
	ghc --make gcd.hs -ljit -i../


call_native: call_native.hs
	ghc --make call_native.hs -ljit -i../



clean:
	rm *.o
	rm *.hi 
	rm mul_add
	rm gcd
	rm call_native