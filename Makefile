VERSION = $(shell egrep 'defconst[ ]*xslt-process-version' lisp/xslt-process.el | awk '{print $$NF}')

all clean distclean: doc/version.texi
	for d in java etc doc; do \
	  (cd $$d; make $@); \
	done

doc/version.texi:
	@echo "@set version $(VERSION)" >doc/version.texi
	@echo "@set update-month `date +'%B %Y'`" >> doc/version.texi

dist:
	@(repository=`cat CVS/Root`; \
	 mkdir -p /tmp/xslt-process-$(VERSION); \
	 cp -rf . /tmp/xslt-process-$(VERSION); \
	 cd /tmp/xslt-process-$(VERSION); \
	 make distclean; make all; make clean; \
	 find . -name 'CVS' -prune -exec rm -rf {} \; ;\
	 cd /tmp; \
	 tar zcf xslt-process-$(VERSION).tar.gz xslt-process-$(VERSION); \
	 rm -rf /tmp/xslt-process-$(VERSION); \
	 tag=release_`echo $(VERSION) | sed 's/\./_/g'`; \
	 echo "Tagging the repository with tag '$$tag'"; \
	 cvs -d $$repository rtag -F $$tag xslt; \
	 echo "The distribution is in /tmp/xslt-process-$(VERSION).tar.gz")
