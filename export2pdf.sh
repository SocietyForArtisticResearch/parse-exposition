#!/bin/bash
stack exec -- parse-exposition -d -latex $1
cd media/
for i in *.gif ; do 
    convert -coalesce "$i" "$(basename "${i/.gif}")".png
done
cd ..
xelatex -interaction=nonstopmode export.tex
#rm *.tex
#rm -r media/
