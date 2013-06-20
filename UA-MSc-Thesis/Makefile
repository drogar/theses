
TEX=latex

all:	Thesis.ps

Thesis.ps:	Thesis.tex thesisMacros.tex

%.gz:	%.ps
	gzip $<

%.bbl:	%.tex *.bib
	bibtex $*


#%.aux:	%.tex
#	latex thesis

%.ps: 	%.dvi
	$(TEX) $*.tex
	dvips -o $@ $<

#%.dvi:	%.tex


clean:
	rm -f *.blg *.dvi *.tmp *.log *.aux *.toc *.tof Thesis.ps

realclean: clean
