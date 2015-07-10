

all:
	pdflatex strat

arch.pdf: arch.dot
	dot -Tpdf arch.dot -o arch.pdf

strat.pdf: strat.tex
	pdflatex strat

clean:
	rm -f arch.pdf
	rm -f strat.pdf strat.aux