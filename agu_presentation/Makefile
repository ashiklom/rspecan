ALL_RNW := $(wildcard *.Rnw)
RNW := $(filter-out main.Rnw, $(ALL_RNW))
SCRIPTS := $(wildcard *.R)

LMKFLAGS := -outdir=texfiles -jobname=agutalk

.PHONY: all clean

all: agutalk.pdf

agutalk.pdf: main.tex
	latexmk -pdf $(LMKFLAGS) main.tex

clean:
	latexmk -C $(LMKFLAGS) main.tex
	rm -rf main.tex

main.tex: main.Rnw $(RNW) $(SCRIPTS)

%.tex: %.Rnw
	Rscript -e "knitr::knit('$<', '$@')"
	rm -rf Rplots.*
