JAVAC_EXE = javac
#JAVAC_EXE = /usr/local/IBMJava2-13/bin/javac

#CLASSPATH = -classpath /home/rickg/Src/Java/Etomo
VPATH = src:src/etomo:src/etomo/comscript:src/etomo/process:src/etomo/storage:src/etomo/ui:src/etomo/utils
SOURCEPATH = -sourcepath src
CLASSPATH = -classpath classes
OUTPUT = -d classes

PACKAGES = etomo etomo.comscript etomo.process etomo.storage etomo.type etomo.ui etomo.util

JAVAC_OPTIONS =
JAVAC = $(JAVAC_EXE) $(CLASSPATH) $(OUTPUT) $(JAVAC_OPTIONS)

JAVADOC_OPTIONS = -author -version -private -use -breakiterator

CLASS_FILES = \
bin/etomo/ui/FixedDim.class \
bin/etomo/ui/AlignmentEstimationDialog.class \
bin/etomo/ui/BeveledBorder.class \
bin/etomo/ui/BoundaryRotationDialog.class \
bin/etomo/ui/CCDEraserPanel.class \
bin/etomo/ui/CoarseAlignDialog.class \
bin/etomo/ui/ContextPopup.class \
bin/etomo/ui/CrossCorrelationPanel.class \
bin/etomo/ui/DataSetIDPanel.class \
bin/etomo/ui/DataSetTypePanel.class \
bin/etomo/ui/FiducialModelDialog.class \
bin/etomo/ui/GenericProcessingDialog.class \
bin/etomo/ui/HighlightList.class \
bin/etomo/ui/HTMLPageWindow.class \
bin/etomo/ui/LabeledTextField.class \
bin/etomo/ui/MainFrame_AboutBox.class \
bin/etomo/ui/MainFrame.class \
bin/etomo/ui/PostProcessingDialog.class \
bin/etomo/ui/PreProcessingDialog.class \
bin/etomo/ui/ProcessControlPanel.class \
bin/etomo/ui/ProcessDialog.class \
bin/etomo/ui/SetupDialog.class \
bin/etomo/ui/TiltAngleDialogPanel.class \
bin/etomo/ui/TomogramCombinationDialog.class \
bin/etomo/ui/TomogramGenerationDialog.class \
bin/etomo/storage/Storable.class \
bin/etomo/storage/ParameterStore.class \
bin/etomo/AxisType.class \
bin/etomo/CopyTomoComs.class \
bin/etomo/SectionType.class \
bin/etomo/TiltAngleType.class \
bin/etomo/ViewType.class \
bin/etomo/DataSource.class \
bin/etomo/DialogExitState.class \
bin/etomo/ConstSetupData.class \
bin/etomo/SetupData.class \
bin/etomo/ProcessManager.class \
bin/etomo/ProcessState.class \
bin/etomo/RunComScript.class \
bin/etomo/SystemProgram.class \
bin/etomo/TiltAngleSpec.class \

TEST_BIN = \
bin/test/test_CircularBuffer.class \
bin/test/test_DataSource.class \


MainTarget : $(CLASS_FILES)
	echo "Make completed."

tests: $(CLASS_FILES) $(TEST_BIN)
	echo "Make tests completed"

javadocs:
	javadoc $(CLASSPATH) $(SOURCEPATH) $(JAVADOC_OPTIONS) -d doc/javadoc \
	$(PACKAGES)


%.class: %.java
	$(JAVAC) $<

jar: $(CLASS_FILES)
	jar -cvmf MANIFEST.MF etomo.jar etomo images

.PHONY: linecount
linecount:
	echo "Actual line count:"
	wc -l src/etomo/*.java
	wc -l src/etomo/comscript/*.java | grep total
	wc -l src/etomo/process/*.java | grep total
	wc -l src/etomo/storage/*.java | grep total
	wc -l src/etomo/type/*.java | grep total
	wc -l src/etomo/ui/*.java | grep total
	wc -l src/etomo/util/*.java
	echo "Semicolon count:"
	grep ";" src/etomo/*.java | wc
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

.PHONY: clean
clean:
	rm -rf bin/etomo/*.class
	rm -rf bin/etomo/comscript/*.class
	rm -rf bin/etomo/process/*.class
	rm -rf bin/etomo/storage/*.class
	rm -rf bin/etomo/ui/*.class
	rm -rf bin/etomo/util/*.class
	rm -rf doc/javadocs
