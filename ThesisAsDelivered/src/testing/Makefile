GTKLHS := $(wildcard ../Simulator/*.lhs) $(wildcard ../QSM/*.lhs) $(wildcard ../Assembler/*.lhs) $(wildcard ../Assembler/*.hs) $(wildcard ../Data/*.lhs)


HS := ../Assembler/AssemParser.hs  ../Assembler/AssemLexer.hs
GRAMMER := ../Assembler/AssemParser.ly

GTKGHCPARMS:=-fglasgow-exts -fwarn-incomplete-patterns
GTKGHCIPARMS:=-fglasgow-exts -i..
GHCGENSRCPARMS:=-fglasgow-exts 

GHC:=ghc
GHCI:=ghci

HAPPY:=happy
HAPPYOPTS:=-g -a -c

all: tsf

tsf:	TestStackFunctions.lhs
	$(GHCI)  $(GTKGHCIPARMS) TestStackFunctions.lhs 

%.hs: %.ly
	$(HAPPY) $(HAPPYOPTS) $< -o $@

../Assembler/AssemLexer.hs: ../Assembler/AssemLexer.x
	alex --ghc ../Assembler/AssemLexer.x

clean:
	@echo "Cleaning..."
	rm -f *~ *.o *.hi tags TAGS core 

