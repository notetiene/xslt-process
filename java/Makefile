JAR = xslt.jar

JAVA = $(shell find xslt/debugger -name '*.java' -type f -print)
CLASSES = $(JAVA:.java=.class)

jar: java/$(JAR)

java/$(JAR): $(CLASSES)
	jar cf $(JAR) $(CLASSES)

$(CLASSES): $(JAVA)
	javac -classpath `pwd`:$$CLASSPATH $(JAVA)

