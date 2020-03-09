all: final

ps: *.tex
	latex main.tex && dvips -t letter -o main.ps  main.dvi

dvi: *.tex
	latex main.tex

pdf: *.tex
	bibtex -min-crossrefs=10 main
	pdflatex -shell-escape main.tex main.pdf

bib:
	bibtex -min-crossrefs=10 main

final: *.tex
	pdflatex -shell-escape main.tex main.pdf
	bibtex -min-crossrefs=10 main
	pdflatex -shell-escape main.tex main.pdf
	pdflatex -shell-escape main.tex main.pdf

quick: *.tex
	pdflatex -shell-escape main.tex main.pdf
