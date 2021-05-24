
inputs := tpl.tex tpl.sty \
          $(wildcard sec/*.tex)


.DEFAULT: pdf

.PHONY: pdf
pdf: tpl.pdf
tpl.pdf: tpl.tex $(inputs)
	latexmk --pdf $<

clean:
	latexmk -c
	rm -f tpl.pdf
