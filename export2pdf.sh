#!/bin/bash
stack exec -- parse-exposition -d -latex $1
xelatex -interaction=nonstopmode export.tex
rm *.tex
rm -r media/
