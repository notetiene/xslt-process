JAR = xslt.jar

# The following definition assumes you have Cygwin installed on your
# computer: uname returns something like: CYGWIN...
ON_WINDOWS = $(shell uname | tr A-Z a-z | awk '{if ($$0 ~ "win") print 1; else print 0;}')

ifeq ($(ON_WINDOWS), 1)
SEP = ;
else
SEP = :
endif

JAVA = $(shell find xslt/debugger -name '*.java' -type f -print)
CLASSES = $(JAVA:.java=.class)
CLASSPATH = saxon-6.5.2.jar$(SEP)xalan-2.4.D1.jar$(SEP)xercesImpl-2.0.1.jar$(SEP)bsf.jar$(SEP)batik.jar$(SEP)jimi-1.0.jar$(SEP)fop.jar$(SEP)avalon-framework-cvs-20020315.jar

all: $(JAR)

$(JAR): $(CLASSES)
	jar cf $(JAR) xslt

$(CLASSES): $(JAVA)
	javac -classpath '$(CLASSPATH)' $(JAVA)

clean distclean:
	find . -name '*.class' -exec rm -f {} \;

