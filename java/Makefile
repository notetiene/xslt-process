JAR = xslt.jar

SEP = :

JAVA = $(shell find xslt/debugger -name '*.java' -type f -print)
CLASSES = $(JAVA:.java=.class)
CLASSPATH = xalan-2.0.1.jar$(SEP)xalanj1compat.jar$(SEP)xerces.jar$(SEP)bsf.jar$(SEP)saxon-6.2.2-fix.jar

jar: $(JAR)

$(JAR): $(CLASSES)
	jar cf $(JAR) xslt

$(CLASSES): $(JAVA)
	javac -classpath $(CLASSPATH) $(JAVA)

clean:
	find . -name '*.class' -exec rm -f {} \;
	rm -f $(JAR)
