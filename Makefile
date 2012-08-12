

HSFILES := $(wildcard Libjit/*.hs) 
OFILES := $(HSFILES:%.hs=%.o)

exhs := $(wildcard examples/*.hs) 
exexe := $(exhs:%hs=%exe)

all: Libjit/Raw.hs $(OFILES) Libjit.o

Libjit.o: Libjit.hs 
	ghc --make $<

Libjit/%.o : Libjit/%.hs 
	ghc --make $<

Libjit/Raw.hs: Libjit/Raw.chs
	c2hs Libjit/Raw.chs


examples: $(exexe)

examples/%.exe:examples/%.hs
	ghc -o $@ --make $< -ljit

clean: 
	rm -f *.o
	rm -f *.hi 
	rm -f Libjit/*.o
	rm -f Libjit/*hi
	rm -f Libjit/Raw.hs 
	rm -f $(exexe)