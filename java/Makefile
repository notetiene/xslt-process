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
CLASSPATH = xalan-2.0.1.jar$(SEP)xalanj1compat.jar$(SEP)xerces.jar$(SEP)bsf.jar$(SEP)saxon-6.2.2-fix.jar

jar: $(JAR)

$(JAR): $(CLASSES)
	jar cf $(JAR) xslt

$(CLASSES): $(JAVA)
	javac -classpath '$(CLASSPATH)' $(JAVA)

clean:
	find . -name '*.class' -exec rm -f {} \;
	rm -f $(JAR)
