#!/bin/bash
stack exec -- parse-exposition -d $2 -latex $1
cd $2
for i in *.gif ; do 
    convert -coalesce "$i" "$(basename "${i/.gif}")".png
done
for i in *.svg ; do 
    convert -coalesce "$i" "$(basename "${i/.svg}")".png
done
xelatex -interaction=nonstopmode export.tex
#rm *.tex
#rm -r media/
