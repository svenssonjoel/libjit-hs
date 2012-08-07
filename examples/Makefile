

all: mul_add 

mul_add: mul_add.hs 
	ghc --make mul_add.hs ../Libjit/cbits/o/extra.o -ljit -i../

clean:
	rm *.o
	rm *.hi 
	rm mul_add