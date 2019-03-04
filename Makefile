.PHONY: openpaper all


all: build openpaper

static/paper.html: src/Main.lhs
	pandoc --smart -f markdown+lhs -t html+lhs -s  --highlight-style pygments src/Main.lhs -o static/index.html

paper/paper.tex: src/Main.lhs
	pandoc src/Main.lhs -o paper/paper.tex

paper/paper.pdf: paper/paper.tex
	cd paper && pdflatex  -shell-escape paper.tex 

openpaper: paper/paper.pdf
	rifle paper/paper.pdf

build: src/Main.lhs deltas.cabal 
	cabal v2-build

