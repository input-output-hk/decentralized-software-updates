main.pdf: *.tex alg/*.tex
	pdflatex main.tex

clean:
	rm -Rf latex.out
	rm -f *.aux *.log *.out *.cfg *.glo *.idx *.toc *.ilg *.ind *.lof *.lot *.bbl *.blg *.gls *.cut *.hd *.dvi *.ps *.thm *.rpi *.d *.fls *.pyc *.fdb_latexmk
