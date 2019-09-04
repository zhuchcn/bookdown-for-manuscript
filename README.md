This is an example of academic manuscript written based on R Markdown and **bookdown** (https://github.com/rstudio/bookdown).

The manuscript is published on the *Metabolomics*.

The HDL lipidome is widely remodeled by fast food versus Mediterranean diet in 4 days. Zhu, et al. *Metabolomics* August 2019 

[![DOI:10.1007/s11306-019-1579-1](https://zenodo.org/badge/DOI/10.1007/s11306-019-1579-1.svg)](https://doi.org/10.1007/s11306-019-1579-1)

## Title page

The `index.Rmd` defines the title page of the manuscript, including the title, authors, emails, addresses, and abbreviations. The `bibliography` in the yml header defines the reference file(s). The references **must** be stored in `.bib` file(s). The `csl` in the yaml header defines the citation stylesheets.

## Chapters

Each chapter should be written as a separate Rmd file and labeled in the designated order. All Rmd files will be sourced and merged by [**bookdown**](https://bookdown.org/).

## Reference word document

In order to tell the [**bookdown**](https://bookdown.org/) to render it into word document, you need to specify it in the `_output.yml` file. The `reference_doc.doc` is a valid word document, in which the content is ignored and only the stylesheet is kept. Use the **Styles Pane** in Word to modify the stylesheet.

## Equations Font

Unfortunately there is not way to specify the font in the equations in the word style pane, and by default the font of any equations will be **Cambria Math**. The script `format_equations.py` changes the font of all equations in the word document at once. 

## Reference

References are stored in one or more .bib file and needs to be specified in the yaml header of the `index.Rmd`. The references must be in valid BibTex format. The [pubmed-bib](www.github.com/zhuchcn/pubmed-bib) is a command line tool that queries reference from PubMed and print out to the command line in a valid BibTex format.

The citation style is determined by a .csl file and needs to be specified also in the yaml header of the `index.Rmd`. The journal specific csl file can be downloaded from the [Zotero Style Repository](https://www.zotero.org/styles)

## Build

To build the book, either click the **build** button in RStudio, or run the `build.sh` script. The latter will also run the `format_equation.py`. However Windows users must figure something out. The rendered book can be found under the `_book` folder

------------
Please see the page "Get Started" at https://bookdown.org/ for how to compile this example.
