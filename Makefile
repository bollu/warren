.PHONY: openpaper all html


all: build html

html: src/Main.lhs src/LZero.lhs
	mkdir -p docs
	pandoc --smart -f markdown+lhs -t html+lhs -s  --highlight-style pygments src/Main.lhs -o docs/index.html
	pandoc --smart -f markdown+lhs -t html+lhs -s  --highlight-style pygments src/LZero.lhs -o docs/lzero.html

paper/paper.tex: src/Main.lhs
	pandoc src/Main.lhs -o paper/paper.tex

paper/paper.pdf: paper/paper.tex
	cd paper && pdflatex  -shell-escape paper.tex 

openpaper: paper/paper.pdf
	rifle paper/paper.pdf

build: src/*.lhs warren.cabal 
	cabal v2-build

