#!/usr/bin/env sh

Rscript --verbose -e "require(knitr); knitr::knit('compInd_vignette.Rnw')"

latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -bibtex compInd_vignette.tex

latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -bibtex compInd_vignette.tex

latexmk -c

rm compInd_vignette.tex

