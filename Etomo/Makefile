# $Author$
#
# $Revision$

JAVAC_EXE = javac

# CLASSPATH and SOURCEPATH definitions match Eclipse structure
SOURCEPATH = src
CLASSPATH = bin

CLASS_FILES = $(CLASSPATH)/etomo/ApplicationManager.class
JAR_FILE = jar/etomo.jar

PACKAGES = etomo etomo.comscript etomo.process etomo.storage etomo.type etomo.ui etomo.util

JAVAC_OPTIONS = -verbose
JAVAC = $(JAVAC_EXE) -d $(CLASSPATH) -sourcepath $(SOURCEPATH) $(JAVAC_OPTIONS)

JAVADOC_OPTIONS = -author -version -private -use -breakiterator -doctitle "Etomo API" -windowtitle "Etomo API"

TEST_BIN = \
$(OUTPUT)/test/test_CircularBuffer.class \
$(OUTPUT)/test/test_DataSource.class \


MainTarget: $(JAR_FILE)
	echo "Make completed."

$(JAR_FILE): cpimages compile
	mkdir -p jar
	jar  -cvmf MANIFEST.MF $(JAR_FILE) -C $(CLASSPATH) etomo -C $(CLASSPATH) images

cpimages:
	cp -a $(SOURCEPATH)/images $(CLASSPATH)
	rm -rf  $(CLASSPATH)/images/CVS

compile: 
	$(JAVAC)  $(SOURCEPATH)/etomo/ApplicationManager.java

tests: $(CLASS_FILES) $(TEST_BIN)
	echo "Make tests completed"

javadocs:
	javadoc -classpath $(CLASSPATH) $(SOURCEPATH) $(JAVADOC_OPTIONS) -d doc/javadoc \
	$(PACKAGES)

.PHONY: clean
clean:
	rm -rf bin/etomo/*
	rm -rf doc/javadocs
	rm $(JAR_FILE)

.PHONY: linecount
linecount:
	echo "Actual line count:"
	wc -l src/etomo/*.java
	wc -l src/etomo/comscript/*.java | grep total
	wc -l src/etomo/process/*.java | grep total
	wc -l src/etomo/storage/*.java | grep total
	wc -l src/etomo/type/*.java | grep total
	wc -l src/etomo/ui/*.java | grep total
	wc -l src/etomo/util/*.java | grep total
	echo "Semicolon count:"
	grep ";" src/etomo/ApplicationManager.java | wc -l
	grep ";" src/etomo/comscript/*.java | wc -l
	grep ";" src/etomo/process/*.java | wc -l
	grep ";" src/etomo/storage/*.java | wc -l
	grep ";" src/etomo/type/*.java | wc -l
	grep ";" src/etomo/ui/*.java | wc -l
	grep ";" src/etomo/util/*.java | wc -l
	echo "Blank lines:"
	grep "^\s*$$" src/etomo/*.java | wc -l
	grep "^\s*$$" src/etomo/comscript/*.java | wc -l
	grep "^\s*$$" src/etomo/process/*.java | wc -l
	grep "^\s*$$" src/etomo/storage/*.java | wc -l
	grep "^\s*$$" src/etomo/type/*.java | wc -l
	grep "^\s*$$" src/etomo/ui/*.java | wc -l
	grep "^\s*$$" src/etomo/util/*.java | wc -l

