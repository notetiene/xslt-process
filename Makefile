VERSION = $(shell grep '@set'  doc/xslt-process.texi | grep version | awk '{print $$NF}')

JAR = xslt.jar

all: jar doc elc

.PHONY: doc

jar: java/$(JAR)

java/xslt.jar: $(wildcard java/xslt/*.java java/xslt/*/*.java java/xslt/*/*/*.java)
	(cd java; \
	 javac -classpath `pwd`/java:$$CLASSPATH `find . -name '*.java'`; \
	 jar cf $(JAR) `find . -name '*.class'`)

doc:
	(cd doc; make)

elc:

clean:
	(cd doc; make $@)
	rm -f lisp/*.elc
	rm -f java/xslt/*.class

distclean: clean
	(cd doc; make $@)
	rm -f java/xslt.jar

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
