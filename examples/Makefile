

all: mul_add 

mul_add: mul_add.hs 
	ghc --make mul_add.hs -ljit -i../

clean:
	rm *.o
	rm *.hi 
	rm mul_add