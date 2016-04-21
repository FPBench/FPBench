main.pdf: main.tex fpbench.bib
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main

clean:
	rm -rf main.pdf main.aux main.log main.bbl main.blg

.PHONY: clean
