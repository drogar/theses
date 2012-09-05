EXECUTABLE = qsgtk

WXEXECUTABLE = qstack

WXLHS :=  $(wildcard WxSimulator/*.lhs) $(wildcard QSM/*.lhs) $(wildcard Assembler/*.lhs) $(wildcard Assembler/*.hs) $(wildcard Data/*.lhs)

GTKLHS := $(wildcard Simulator/*.lhs) $(wildcard Simulator/Dialogs/*.lhs) $(wildcard QSM/*.lhs) $(wildcard Assembler/*.lhs) $(wildcard Assembler/*.hs) $(wildcard Data/*.lhs)  $(wildcard Shared/*.lhs) 


HS := Assembler/AssemParser.hs  Assembler/AssemLexer.hs
GRAMMER := Assembler/AssemParser.ly

WXGHCPARMS:=-fglasgow-exts -fwarn-incomplete-patterns -package wx
WXGHCIPARMS:=-fglasgow-exts -package wx
GTKGHCPARMS:=-fglasgow-exts -fwarn-incomplete-patterns
GTKGHCIPARMS:=-fglasgow-exts 
GHCGENSRCPARMS:=-fglasgow-exts 

GHC:=ghc
GHCI:=ghci

HAPPY:=happy
HAPPYOPTS:=-g -a -c

all: $(EXECUTABLE)

$(EXECUTABLE): $(GTKLHS) $(HS)
	$(GHC) -c --make $(GHCGENSRCPARMS) $(HS) 
	$(GHC) --make  $(GTKGHCPARMS) -o $(EXECUTABLE) Simulator/QstackGTK.lhs


$(WXEXECUTABLE): $(WXLHS) $(HS)
	$(GHC) -c --make $(GHCGENSRCPARMS) $(HS) 
	$(GHC) --make  $(WXGHCPARMS) -o $(WXEXECUTABLE) WxSimulator/Qmachine.lhs

doc:	$(LHS)
	lhs2TeX --poly driver.lhs > Qstack.tex
	latex Qstack
	makeindex Qstack
	bibtex Qstack
	latex Qstack

%.hs: %.ly
	$(HAPPY) $(HAPPYOPTS) $< -o $@

Assembler/AssemLexer.hs: Assembler/AssemLexer.x
	alex --ghc Assembler/AssemLexer.x

tags: $(LHS) 
	hasktags $(LHS)

clean:
	@echo "Cleaning..."
	rm -f *~ *.o *.hi tags TAGS $(EXECUTABLE) core 
	(cd Assembler ;	rm -f *~ *.o *.hi AssemLexer.hs AssemParser.hs tags TAGS  core )
	(cd Data; rm -f *~ *.o *.hi tags TAGS core )
	(cd QSM ;	rm -f *~ *.o *.hi tags TAGS core )
	(cd Simulaor/Dialogs ;	rm -f *~ *.o *.hi tags TAGS core )
	(cd Simulator ;	rm -f *~ *.o *.hi tags TAGS core )
	(cd WxSimulator ;rm -f *~ *.o *.hi tags TAGS core )

