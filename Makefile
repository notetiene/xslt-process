VERSION = $(shell grep '@set'  doc/xslt-process.texi | grep version | awk '{print $$NF}')

JAR = xslt.jar

all: jar doc elc

.PHONY: doc

JAVA = $(shell find java/xslt/debugger -name '*.java' -type f -print)
CLASSES = $(JAVA:.java=.class)

jar: java/$(JAR)

java/$(JAR): $(CLASSES)
	jar cf java/$(JAR) $(CLASSES)

$(CLASSES): $(JAVA)
	javac -classpath `pwd`/java:$$CLASSPATH $(JAVA)

doc:
	(cd doc; make)

elc:

clean:
	(cd doc; make $@)
	rm -f lisp/*.elc
	find java -name '*.class' -exec rm -f {} \;

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
