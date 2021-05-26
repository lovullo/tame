# The TAME Programming Language: Design and Implementation

This is a living document providing a formal definition of the TAME
programming language.

## Dependencies
See [`tpl.sty`](tpl.sty) for the specific LaTeX packages that are
needed.  If you use a Debian-based system, the following command should be
sufficient to install all necessary dependencies:

```
$ apt install --no-recommends \
    make latexmk biber \
    texlive-latex-extra texlive-fonts-extra texlive-bibtex-extra \
    texlive-science
```

## Building
Simply run `make`.  The output is `tpl.pdf`.
