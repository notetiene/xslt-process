TEXI = xslt-process.texi
IMAGES = xslt-process-mode.png

INFO = $(TEXI:.texi=.info)
DVI = $(TEXI:.texi=.dvi)
PS = $(TEXI:.texi=.ps)
PDF = $(TEXI:.texi=.pdf)
PDFIMAGES = $(IMAGES:.png=.pdf)
EPSIMAGES = $(IMAGES:.png=.eps)
JPGIMAGES = $(IMAGES:.png=.jpg)
HTML = $(TEXI:.texi=.html)

.SUFFIXES: .texi .png .info .dvi .ps .pdf .eps .jpg .html

.texi.info:
	makeinfo $<

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
dvi: $(EPSIMAGES) $(DVI)
ps: $(EPSIMAGES) $(PS)
pdf: $(PDFIMAGES) $(PDF)
html: $(JPGIMAGES) $(HTML)

.png.pdf:
	@(size=`identify $< | awk '{print $$2}'`; \
	  echo convert -page $$size $< $@; \
	  convert -page $$size $< $@)

.png.jpg:
	@(size=`identify $< | awk '{print $$2}'`; \
	  echo convert -page $$size $< $@; \
	  convert -page $$size $< $@)

.png.eps:
	@(size=`identify $< | awk '{print $$2}'`; \
	  echo convert -page $$size $< $@; \
	  convert -page $$size $< $@)

homepage: html
	sed -e 's/<BODY[^>]*>/<BODY>/g' \
	  -e 's^</BODY>^<p><a HREF=../index.html><img src=../../images/back.gif border=0%> Other (X)Emacs hacks</a><br><a href=../../index.html><img src=../../images/home.gif border=0%> Home</a></body>^g' -e 's/xslt-process.html/index.html/g' <$(HTML) >$(HTML).tmp
	mv $(HTML).tmp index.html

clean:
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr version.texi \
	    $(PDFIMAGES)

distclean: clean
	rm -f *.dvi *.html *.pdf *.ps *.eps *.jpg *.info *.cps
