TEXI = xslt-process.texi
IMAGES = xslt-process-mode.png

INFO = $(TEXI:.texi=.info)
DVI = $(TEXI:.texi=.dvi)
PS = $(TEXI:.texi=.ps)
PDF = $(TEXI:.texi=.pdf)
PDFIMAGES = $(IMAGES:.png=.pdf)
HTML = $(TEXI:.texi=.html)

.SUFFIXES: .texi .info .dvi .ps .pdf .html

.texi.info:
	makeinfo $<
	info --output=../README --subnodes --file $@
	sed -e '/^File:.*/d' -e '/[ 	]*,Top[ 	]*/d' <../README >../README.tmp
	info --file=$@ --node=Installation --node='Setting up PSGML with XSLT-process' --output ../INSTALL
	mv ../README.tmp ../README

.texi.dvi:
	texi2dvi $<
	texi2dvi $<

.dvi.ps:
	dvips -o $@ $<

.texi.html:
	texi2html -nosec_nav -nomenu $<

.texi.pdf:
	texi2pdf $<

all: version.texi info dvi ps pdf html

version.texi:
	(cd ..; make doc/version.texi)

info: $(INFO)
dvi: $(DVI)
ps: $(PS)
pdf: $(PDFIMAGES) $(PDF)
html: $(HTML)

xslt-process-mode.pdf: xslt-process-mode.png
	convert -page 256x120 $< $@

homepage: html
	sed -e 's/<BODY[^>]*>/<BODY>/g' \
	  -e 's^</BODY>^<p><a HREF=../index.html><img src=../../images/back.gif border=0%> Other (X)Emacs hacks</a><br><a href=../../index.html><img src=../../images/home.gif border=0%> Home</a></body>^g' -e 's/xslt-process.html/index.html/g' <$(HTML) >$(HTML).tmp
	mv $(HTML).tmp index.html

clean:
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr version.texi

distclean: clean
	rm -f *.dvi *.html *.pdf *.ps *.info
