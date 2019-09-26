#!/bin/bash
rm -r "./$1"
rm "./latex/$1"*
mkdir "./$1"
mkdir "./$1/figures"
mkdir "./$1/tables"
Rscript ./IncreaseRateRatios.R $1 
Rscript ./LowSLG.R $1
Rscript ./MultipleRegression.R $1
Rscript ./RegressionTrees.R $1
Rscript ./SensitivityAnalysis.R $1
Rscript ./SummaryStats.R $1
Rscript ./OATplots.R $1
sed "s/REPLACEME/$1/g" ./latex/addendum.tex > "./latex/$1.tex"
cd latex
pdflatex -synctex=1 -interaction=nonstopmode $1.tex
bibtex $1.aux
pdflatex -synctex=1 -interaction=nonstopmode $1.tex
