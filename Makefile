


HSFILES := $(wildcard Libjit/*.hs) 
OFILES := $(HSFILES:%.hs=%.o)


all: Libjit/Raw.hs $(OFILES)


Libjit/%.o : Libjit/%.hs 
	ghc --make $<



Libjit/Raw.hs: Libjit/Raw.chs
	c2hs Libjit/Raw.chs

