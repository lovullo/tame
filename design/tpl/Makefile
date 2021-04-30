
inputs := tpl.tex tpl.sty glossary.tex \
          $(wildcard sec/*.tex)


.DEFAULT: pdf

.PHONY: pdf
pdf: tpl.pdf
tpl.pdf: tpl.tex $(inputs)
	latexmk --pdf $<

clean:
	latexmk -c
