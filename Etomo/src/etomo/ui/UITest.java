package etomo.ui;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.AbstractButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import etomo.EtomoDirector;
import etomo.JfcUnitTests;
import etomo.process.SystemProgram;
import etomo.storage.AutodocFilter;
import etomo.type.AxisID;
import etomo.type.DialogType;
import junit.extensions.jfcunit.JFCTestCase;
import junit.extensions.jfcunit.JFCTestHelper;
import junit.extensions.jfcunit.eventdata.MouseEventData;
import junit.extensions.jfcunit.eventdata.StringEventData;
import junit.extensions.jfcunit.finder.NamedComponentFinder;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class UITest extends JFCTestCase {
  public static final String rcsid = "$Id$";

  private static final File TEST_DIR = new File(new File(new File(
      JfcUnitTests.TEST_ROOT_DIR, "etomo"), "ui"), "UITest");

  private final JFCTestHelper helper = new JFCTestHelper();
  private NamedComponentFinder finder = null;

  private Autodoc autodoc = null;
  private File testDir = null;
  private long sleep = 0;

  public final void test() throws IOException {
    //set jfcunit helper
    setHelper(helper);
    //get the vector directory
    File vectorDir = new File(JfcUnitTests.TEST_ROOT_DIR, "vectors");
    //get a list of autodoc files
    AutodocFilter filter = new AutodocFilter();
    filter.setAutodocType(Autodoc.UITEST);
    File[] autodocs = vectorDir.listFiles(filter);
    if (autodocs == null) {
      return;
    }
    //set up Autodoc
    Autodoc.setTest(true);
    Autodoc.setDir_test(vectorDir.getAbsolutePath());
    //run etomo with each autodoc file
    for (int i = 0; i < autodocs.length; i++) {
      runEtomo(autodocs[i]);
    }
  }

  public final void runEtomo(File autodocFile) throws IOException {
    //delete test directory
    testDir = new File(TEST_DIR, "dataset");
    SystemProgram remove = new SystemProgram(System.getProperty("user.dir"),
        new String[] { "rm", "-fr", testDir.getAbsolutePath() }, AxisID.ONLY);
    remove.run();
    //make test directory
    testDir.mkdirs();
    //go to test directory in case the autodoc doesn't specific an absolute
    //path for the stack
    System.setProperty("user.dir", testDir.getAbsolutePath());
    //get the autodoc
    Autodoc.resetInstance_test(Autodoc.UITEST);
    try {
      autodoc = Autodoc.getInstance_test(Autodoc.UITEST, autodocFile,
          AxisID.ONLY);
    }
    catch (FileNotFoundException e) {
    }
    assertNotNull(autodoc);
    //copy stacks into test directory
    /*SystemProgram copy = new SystemProgram(testDir.getAbsolutePath(), new String[] {
        "cp",
        new File(vectorDir, DatasetFiles.getStackName(dataset, axisType,
            AxisID.SECOND)).toString(), "." }, AxisID.ONLY);
    copy.run();*/
    //get the sleep time from the autodoc
    String sleepValue = null;
    try {
      sleepValue = autodoc.getAttribute(JfcUnitTests.SLEEP_ATTRIB_NAME)
          .getUnformattedValue();
    }
    catch (NullPointerException e) {
    }
    if (sleepValue != null) {
      sleep = Long.parseLong(sleepValue);
    }
    //get etomo file name from the autodoc
    String etomoFileName = null;
    try {
      etomoFileName = autodoc.getAttribute(JfcUnitTests.ETOMO_FILE_ATTRIB_NAME)
          .getUnformattedValue();
    }
    catch (NullPointerException e) {
    }
    //build the argument list
    String[] args;
    if (etomoFileName == null) {
      args = new String[JfcUnitTests.ETOMO_ARGUMENTS.length];
    }
    else {
      args = new String[JfcUnitTests.ETOMO_ARGUMENTS.length + 1];
    }
    for (int i = 0; i < JfcUnitTests.ETOMO_ARGUMENTS.length; i++) {
      args[i] = JfcUnitTests.ETOMO_ARGUMENTS[i];
    }
    if (etomoFileName != null) {
      args[args.length - 1] = etomoFileName;
    }
    //run etomo
    EtomoDirector.createInstance_test(args);
    //get the dialog sections from the autodoc;
    SectionLocation sectionLocation = autodoc
        .getSectionLocation(JfcUnitTests.SECTION_TYPE);
    Section section = autodoc.nextSection(sectionLocation);
    while (section != null) {
      getDialog(section);
      section = autodoc.nextSection(sectionLocation);
    }
  }

  private final void getDialog(Section section) {
    //check for a dialog that comes up automatically
    if (section.getName().equals(DialogType.SETUP.getStorableName())) {
      useDialog(section);
      return;
    }
    //press dialog button
  }

  private final void setFinder(Class componentClass, String name) {
    if (finder == null) {
      finder = new NamedComponentFinder(componentClass, name, true);
    }
    else {
      finder.setComponentClass(componentClass);
      finder.setName(name);
    }
  }

  private final void useDialog(Section section) {
    //get the root panel of the dialog
    setFinder(JPanel.class, section.getName());
    JPanel dialogRootPanel = (JPanel) finder.find();
    //get list of attributes
    AttributeLocation attribLoc = section.getAttributeLocation();
    Attribute attrib = section.nextAttribute(attribLoc);
    while (attrib != null) {
      //apply attribute
      uiAction(dialogRootPanel, attrib);
      attrib = section.nextAttribute(attribLoc);
    }
  }

  private final void uiAction(JPanel dialogRootPanel, Attribute attribute) {
    //sleep if this is a sleep attribute
    String name = attribute.getName();
    if (name.equals(JfcUnitTests.SLEEP_ATTRIB_NAME)) {
      sleep(attribute);
      return;
    }
    //get the index of the component
    int index = 0;
    Attribute indexAttrib = attribute.getAttributeByIndex(0);
    String indexName = indexAttrib.getUnformattedValue();
    if (indexName != null) {
      index = Integer.parseInt(indexName);
    }
    String value = attribute.getUnformattedValue();
    //perform the action on the component
    if (value == null) {
      uiButtonAction(dialogRootPanel, name, index);
    }
    else {
      uiFieldAction(dialogRootPanel, name, value, index);
    }
  }
  
  private final void sleep(Attribute attrib) {
    String sleepValue = null;
    try {
      sleepValue = attrib.getUnformattedValue();
    }
    catch (NullPointerException e) {
    }
    try {
      if (sleepValue != null) {
        Thread.sleep(Long.parseLong(sleepValue));
      }
      else {
        Thread.sleep(sleep);
      }
    }
    catch (InterruptedException e) {
    }
  }

  private final void uiButtonAction(JPanel dialogRootPanel, String buttonName,
      int index) {
    setFinder(AbstractButton.class, buttonName);
    AbstractButton button = (AbstractButton) finder
        .find(dialogRootPanel, index);
    helper.enterClickAndLeave(new MouseEventData(this, button));
  }

  private final void uiFieldAction(JPanel dialogRootPanel, String fieldName,
      String value, int index) {
    setFinder(JTextField.class, fieldName);
    JTextField textField = (JTextField) finder.find(dialogRootPanel, index);
    helper.sendString(new StringEventData(this, textField, value));
  }
/*
  void setup() {
    //get Axis Type from autodoc setup section
    if (setupSection != null
        && setupSection.getAttribute(JfcUnitTests.SINGLE_AXIS_ATTRIB_NAME) != null) {
      axisType = AxisType.SINGLE_AXIS;
    }
    //delete test directory
    testDir = new File(TEST_DIR, "dataset");
    SystemProgram remove = new SystemProgram(System.getProperty("user.dir"),
        new String[] { "rm", "-fr", testDir.getAbsolutePath() }, AxisID.ONLY);
    remove.run();
    //make test directory
    testDir.mkdirs();
    //copy vectors into test directory
    //a.st
    firstStack = new File(vectorDir, DatasetFiles.getStackName(dataset,
        axisType, AxisID.FIRST));
    SystemProgram copy = new SystemProgram(testDir.getAbsolutePath(),
        new String[] { "cp", firstStack.toString(), "." }, AxisID.ONLY);
    copy.run();
    if (axisType == AxisType.DUAL_AXIS) {
      //b.st
      copy = new SystemProgram(testDir.getAbsolutePath(), new String[] {
          "cp",
          new File(vectorDir, DatasetFiles.getStackName(dataset, axisType,
              AxisID.SECOND)).toString(), "." }, AxisID.ONLY);
      copy.run();
    }
    //a.seed
    copy = new SystemProgram(testDir.getAbsolutePath(), new String[] {
        "cp",
        new File(vectorDir, DatasetFiles.getSeedName(dataset, axisType,
            AxisID.FIRST)).toString(), "." }, AxisID.ONLY);
    copy.run();
    //create helper
    setHelper(helper);
    //use setup dialog    
    useSetupDialog();
  }

  private final void useSetupDialog() {
    //get setup root panel
    NamedComponentFinder finder = new NamedComponentFinder(JPanel.class,
        setupDialogName);
    Container setupRootPanel = (Container) finder.find();
    //Dataset name
    finder.setComponentClass(JTextField.class);
    finder.setName(UIUtilities
        .convertLabelToName(SetupDialog.LTF_DATASET_LABEL));
    JTextField tfDataset = (JTextField) finder.find(setupRootPanel, 0);
    helper.sendString(new StringEventData(this, tfDataset, firstStack
        .getAbsolutePath()));
    //Axis Type
    if (axisType == AxisType.SINGLE_AXIS) {
      //set single axis
      finder.setComponentClass(JRadioButton.class);
      finder.setName(UIUtilities
          .convertLabelToName(SetupDialog.RB_SINGLE_AXIS_LABEL));
      JRadioButton rbSingleAxis = (JRadioButton) finder.find(setupRootPanel, 0);
      helper.enterClickAndLeave(new MouseEventData(this, rbSingleAxis));
    }
    //Frame Type
    //get montage from autodoc
    String montageName = UIUtilities
        .convertLabelToName(SetupDialog.RB_MONTAGE_LABEL);
    if (setupSection.getAttribute(montageName) != null) {
      //set montage
      finder.setComponentClass(JRadioButton.class);
      finder.setName(montageName);
      JRadioButton rbMontage = (JRadioButton) finder.find(setupRootPanel, 0);
      helper.enterClickAndLeave(new MouseEventData(this, rbMontage));
    }
    //get scan header button
    finder.setComponentClass(AbstractButton.class);
    finder.setName(UIUtilities
        .convertLabelToName(SetupDialog.BTN_SCAN_HEADER_LABEL));
    AbstractButton bScanHeader = (AbstractButton) finder
        .find(setupRootPanel, 0);
    //press scan header button
    helper.enterClickAndLeave(new MouseEventData(this, bScanHeader));
    //get fiducial diameter text field
    finder.setComponentClass(JTextField.class);
    finder.setName(UIUtilities
        .convertLabelToName(SetupDialog.LTF_FIDUCIAL_DIAMETER_LABEL));
    JTextField tfFiducialDiameter = (JTextField) finder.find(setupRootPanel, 0);
    //fill fiducial diameter text field
    helper.sendString(new StringEventData(this, tfFiducialDiameter, "10"));
    //get create com scripts button
    finder.setComponentClass(AbstractButton.class);
    finder.setName(UIUtilities
        .convertLabelToName(SetupDialog.BTN_CREATE_COM_SCRIPTS_LABEL));
    AbstractButton bCreateComScripts = (AbstractButton) finder.find(
        setupRootPanel, 0);
    //press create com scripts button
    helper.enterClickAndLeave(new MouseEventData(this, bCreateComScripts));
    try {
      Thread.sleep(1000);
    }
    catch (InterruptedException e) {
    }
  }*/
}
/**
 * <p> $Log$ </p>
 */