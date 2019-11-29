#!/bin/bash

echo "Start rendering the manuscript"
R --quite -e "rmarkdown::render_site(encoding = 'UTF-8')"

echo "Start changing the equations format"
python format_equations.py -i _book/manuscript.docx -o _book/manuscript.docx -f Calibri

echo "Opening the manuscript"
open _book/manuscript.docx