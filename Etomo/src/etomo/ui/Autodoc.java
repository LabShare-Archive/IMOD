package etomo.ui;

import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

import java.awt.geom.IllegalPathStateException;
import java.io.File;
import java.io.FileNotFoundException;
import java.lang.IllegalArgumentException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;
import java.util.Iterator;

/**
 * <p>Description:  Data storage for an autodoc file.
 * 
 * To Use:
 * 
 * Set up environement variables:  make sure that either the AUTODOC_DIR or
 * IMOD_DIR/autodoc points to an autodoc directory (AUTODOC_DIR) is checked
 * first.  An autodoc file can also be placed in the current working directory.
 * 
 * Call Autodoc.get(String) with the constant refering to the autodoc file you wish to
 * load.
 * 
 * Use the get... and next... functions to retrieve sections and attributes.
 * 
 * 
 * To Test:
 * Call the print() function to print everything in the autodoc.
 * Test the parser of the autodoc file by modifying the code.  Set testMode to
 * true.  Uncomment the test function you wish to use.
 * 
 * 
 * Possible Upgrades:
 * 
 * Required:  The Autodoc file names need to be added to this object.
 * 
 * The ability to open an unknown autodoc file.
 * 
 * Make testing accessible without modifying the code.
 * 
 * After parsing is finished the Autodoc should be readOnly.  Allow Autodoc to be
 * set to ReadOnly. 
 * 
 * </p>
 *
 * <p>Copyright: Copyright © 2002, 2003</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 */

public class Autodoc implements AttributeCollection {
  public static final String rcsid = "$$Id$$";

  public static final String AUTODOC_DIR = "AUTODOC_DIR";
  public static final String IMOD_DIR = "IMOD_DIR";
  public static final String DEFAULT_AUTODOC_DIR = "autodoc";
  public static final String TILTXCORR = "tiltxcorr";
  public static final String MTF_FILTER = "mtffilter";
  public static final String COMBINE_FFT = "combinefft";
  public static final String TILTALIGN = "tiltalign";
  public static final String CCDERASER = "ccderaser";
  public static final String SOLVEMATCH = "solvematch";
  public static final String BEADTRACK = "beadtrack";
  public static final String TEST = "test";
  public static final String CPU = "cpu";

  private static Autodoc TILTXCORR_INSTANCE = null;
  private static Autodoc TEST_INSTANCE = null;
  private static Autodoc MTF_FILTER_INSTANCE = null;
  private static Autodoc COMBINE_FFT_INSTANCE = null;
  private static Autodoc TILTALIGN_INSTANCE = null;
  private static Autodoc CCDERASER_INSTANCE = null;
  private static Autodoc SOLVEMATCH_INSTANCE = null;
  private static Autodoc BEADTRACK_INSTANCE = null;
  private static Autodoc CPU_INSTANCE = null;

  private static String testDir = null;

  private File autodocFile = null;
  private AutodocParser parser = null;
  private boolean internalTest = false;

  //data
  private HashMap metaData = null;
  private Vector sectionList = null;
  private HashMap sectionMap = null;

  //Cache of sets of data.  Hold on to sets of data requested by other
  //objects.  Autodocs are never refreshed so the set will remain valid.
  //And, like the autodoc itself, the set will probably be requested more
  //then once.
  private HashMap attributeValuesCache = null;

  public static Autodoc getInstance(String name, AxisID axisID)
      throws FileNotFoundException, IOException {
    if (name == null) {
      throw new IllegalStateException("name is null");
    }
    Autodoc autodoc = getExistingAutodoc(name);
    if (autodoc != null) {
      return autodoc;
    }
    autodoc = new Autodoc();
    if (name.equals(CPU)) {
      autodoc.initialize(name, axisID, "IMOD_CALIB_DIR");
    }
    else {
      autodoc.initialize(name, axisID);
    }
    return autodoc;
  }
  
  /**
   * for testing
   * @param name
   */
  public static final void resetInstance_test(String name) {
    if (!EtomoDirector.getInstance().isTest()) {
      throw new IllegalStateException();
    }
    else if (name.equals(TILTXCORR)) {
      TILTXCORR_INSTANCE = null;
    }
    else if (name.equals(TEST)) {
      TEST_INSTANCE = null;
    }
    else if (name.equals(MTF_FILTER)) {
      MTF_FILTER_INSTANCE = null;
    }
    else if (name.equals(COMBINE_FFT)) {
      COMBINE_FFT_INSTANCE = null;
    }
    else if (name.equals(TILTALIGN)) {
      TILTALIGN_INSTANCE = null;
    }
    else if (name.equals(CCDERASER)) {
      CCDERASER_INSTANCE = null;
    }
    else if (name.equals(SOLVEMATCH)) {
      SOLVEMATCH_INSTANCE = null;
    }
    else if (name.equals(BEADTRACK)) {
      BEADTRACK_INSTANCE = null;
    }
    else if (name.equals(CPU)) {
      CPU_INSTANCE = null;
    }
    else {
      throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
    }
  }

  private static final Autodoc getExistingAutodoc(String name) {
    if (name.equals(TILTXCORR)) {
      return TILTXCORR_INSTANCE;
    }
    if (name.equals(TEST)) {
      return TEST_INSTANCE;
    }
    if (name.equals(MTF_FILTER)) {
      return MTF_FILTER_INSTANCE;
    }
    if (name.equals(COMBINE_FFT)) {
      return COMBINE_FFT_INSTANCE;
    }
    if (name.equals(TILTALIGN)) {
      return TILTALIGN_INSTANCE;
    }
    if (name.equals(CCDERASER)) {
      return CCDERASER_INSTANCE;
    }
    if (name.equals(SOLVEMATCH)) {
      return SOLVEMATCH_INSTANCE;
    }
    if (name.equals(BEADTRACK)) {
      return BEADTRACK_INSTANCE;
    }
    if (name.equals(CPU)) {
      return CPU_INSTANCE;
    }
    throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
  }

  private Autodoc() {
  }

  /**
   * for test
   * @param testDirAbsolutePath
   */
  public static void setTestDir(String testDirAbsolutePath) {
    if (!EtomoDirector.getInstance().isTest()) {
      throw new IllegalStateException();
    }
    testDir = testDirAbsolutePath;
  }

  final String getName() {
    return autodocFile.getName();
  }

  final File getAutodocFile() {
    return autodocFile;
  }

  public Section addSection(Token type, Token name) {
    if (sectionList == null) {
      sectionList = new Vector();
      sectionMap = new HashMap();
    }
    Section existingSection = null;
    String key = Section.getKey(type, name);
    existingSection = (Section) sectionMap.get(key);
    if (existingSection == null) {
      Section newSection = new Section(type, name);
      sectionList.add(newSection);
      sectionMap.put(newSection.getKey(), newSection);
      return newSection;
    }
    return existingSection;
  }

  public AttributeCollection addAttribute(Token name) {
    if (metaData == null) {
      metaData = new HashMap();
    }
    Attribute existingMetaDataElement = null;
    String key = Attribute.getKey(name);
    existingMetaDataElement = (Attribute) metaData.get(key);
    if (existingMetaDataElement == null) {
      Attribute newMetaDataElement = new Attribute(name);
      metaData.put(key, newMetaDataElement);
      return newMetaDataElement;
    }
    return existingMetaDataElement;
  }

  public final Attribute getAttribute(String name) {
    if (metaData == null) {
      return null;
    }
    String key = Attribute.getKey(name);
    Attribute attribute = (Attribute) metaData.get(key);
    return attribute;
  }

  public final Section getSection(String type, String name) {
    if (sectionMap == null) {
      return null;
    }
    String key = Section.getKey(type, name);
    Section section = (Section) sectionMap.get(key);
    return section;
  }

  final boolean isSectionExists(String type) {
    return getFirstSectionLocation(type) != null;
  }

  public final SectionLocation getFirstSectionLocation(String type) {
    if (sectionList == null) {
      return null;
    }
    Section section = null;
    for (int i = 0; i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      if (section.equalsType(type)) {
        return new SectionLocation(type, i);
      }
    }
    return null;
  }

  public final Section getSection(SectionLocation location) {
    if (location == null || sectionList == null) {
      return null;
    }
    return (Section) sectionList.get(location.getIndex());
  }

  public final Section nextSection(SectionLocation location) {
    if (location == null) {
      return null;
    }
    Section section = null;
    for (int i = location.getIndex() + 1; i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      if (section.equalsType(location.getType())) {
        location.setIndex(i);
        return section;
      }
    }
    return null;
  }

  /**
   * Returns a HashMap containing a list of attribute values, keyed by
   * sectionName.  The elements in each list is using section type and attribute
   * name.  Each list is saved in attributeValuesCache.  If a saved list is
   * requested, then the cached HashMap is returned.
   * @param sectionType
   * @param attributeName
   * @return
   */
  public final HashMap getAttributeValues(String sectionType,
      String attributeName) {
    if (sectionType == null || attributeName == null || sectionList == null) {
      return null;
    }
    HashMap attributeValues = null;
    //See if an attributeValues with this sectionType and attributeName has
    //already been created.
    String cacheKey = sectionType + attributeName;
    if (attributeValuesCache != null) {
      attributeValues = (HashMap) attributeValuesCache.get(cacheKey);
      if (attributeValues != null) {
        return attributeValues;
      }
    }
    //Create attributeValues
    attributeValues = new HashMap();
    SectionLocation sectionLocation = getFirstSectionLocation(sectionType);
    Section section = getSection(sectionLocation);
    while (section != null) {
      try {
        String sectionName = section.getName();
        String attributeValue = section.getAttribute(attributeName)
            .getUnformattedValue();
        attributeValues.put(sectionName, attributeValue);
      }
      catch (NullPointerException e) {
        //An attribute with attributeName doesn't exist
      }
      //Go to next section
      section = nextSection(sectionLocation);
    }
    //Cache attributeValues.
    if (attributeValuesCache == null) {
      attributeValuesCache = new HashMap();
    }
    attributeValuesCache.put(cacheKey, attributeValues);
    return attributeValues;
  }

  public void print() {
    System.out.println("Printing stored data:");
    if (metaData != null) {
      Attribute metaDataElement = null;
      Iterator iterator = metaData.values().iterator();
      while (iterator.hasNext()) {
        metaDataElement = (Attribute) iterator.next();
        metaDataElement.print();
      }
    }
    System.out.println("metaData is null");
    if (sectionList == null) {
      System.out.println("sectionList is null");
      return;
    }
    Section section = null;
    for (int i = 0; i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      section.print();
    }
  }

  /**
   * sets the autodoc file
   * @param name
   * @param axisID
   * @param envVariable
   */
  private final File setAutodocFile(String name, AxisID axisID,
      String envVariable) {
    File dir = getTestAutodocDir();
    if (dir != null) {
      return getAutodocFile(dir, name);
    }
    if (envVariable != null && !envVariable.matches("\\s*+")) {
      //if envVariable is set, then it points to the only valid directory for this
      //autodoc
      dir = getDir(envVariable, axisID);
      if (dir == null) {
        System.err.println("Warning:  can't open the " + name
            + " autodoc file.\nThis autodoc should be stored in $"
            + envVariable + ".\n");
        return null;
      }
      return getAutodocFile(dir, name);
    }
    dir = getDir(AUTODOC_DIR, axisID);
    if (dir != null) {
      return getAutodocFile(dir, name);
    }
    dir = getDir(IMOD_DIR, DEFAULT_AUTODOC_DIR, axisID);
    if (dir != null) {
      return getAutodocFile(dir, name);
    }
    System.err.println("Warning:  can't open the " + name
        + " autodoc file.\nThis autodoc should be stored in either $"
        + IMOD_DIR + "/" + DEFAULT_AUTODOC_DIR + " or $" + AUTODOC_DIR + ".\n");
    return null;
  }

  private final File getAutodocFile(File autodocDir, String autodocName) {
    File file = DatasetFiles.getAutodoc(autodocDir, autodocName);
    if (!file.exists()) {
      System.err.println("Warning:  the autodoc file," + file.getAbsolutePath()
          + ", does not exist.");
      return null;
    }
    if (file.isDirectory()) {
      System.err.println("Warning:  the autodoc file," + file.getAbsolutePath()
          + ", is a directory.");
      return null;
    }
    if (!file.canRead()) {
      System.err.println("Warning:  cannot read the autodoc file,"
          + file.getAbsolutePath() + ".");
      return null;
    }
    return file;
  }

  private final File getTestAutodocDir() {
    if (!EtomoDirector.getInstance().isTest()) {
      return null;
    }
    if (testDir == null) {
      return null;
    }
    File dir = new File(testDir);
    if (!dir.isAbsolute()) {
      throw new IllegalPathStateException(testDir + " is not an absolute path");
    }
    if (!dir.exists()) {
      throw new IllegalPathStateException(dir.getAbsolutePath()
          + " does not exist");
    }
    if (!dir.canRead()) {
      throw new IllegalPathStateException("cannot read "
          + dir.getAbsolutePath());
    }
    return dir;
  }

  private final File getDir(String envVariable, AxisID axisID) {
    if (envVariable == null || envVariable.matches("\\s*+")) {
      return null;
    }
    String dirName = new String(Utilities.getEnvironmentVariable(null,
        envVariable, axisID));
    File dir = new File(dirName);
    if (!checkDir(dir, envVariable)) {
      return null;
    }
    return dir;
  }

  private final File getDir(String envVariable, String dirName, AxisID axisID) {
    File parentDir = getDir(envVariable, axisID);
    if (parentDir == null) {
      return null;
    }
    File dir = new File(parentDir, dirName);
    if (!checkDir(dir, envVariable)) {
      return null;
    }
    return dir;
  }

  private final boolean checkDir(File dir, String envVariable) {
    if (!dir.exists()) {
      System.err.println("Warning:  " + dir.getAbsolutePath()
          + " does not exist.  See $" + envVariable + ".");
      return false;
    }
    if (!dir.isDirectory()) {
      System.err.println("Warning:  " + dir.getAbsolutePath()
          + " is not a directory.  See $" + envVariable + ".");
      return false;
    }
    if (!dir.canRead()) {
      System.err.println("Warning:  cannot read " + dir.getAbsolutePath()
          + ".  See $" + envVariable + ".");
      return false;
    }
    return true;
  }

  private final void initialize(String name, AxisID axisID)
      throws FileNotFoundException, IOException {
    initialize(name, axisID, null);
  }

  private final void initialize(String name, AxisID axisID, String envVariable)
      throws FileNotFoundException, IOException {
    autodocFile = setAutodocFile(name, axisID, envVariable);
    if (autodocFile == null) {
      return;
    }
    parser = new AutodocParser(this);
    if (internalTest) {
      //parser.testStreamTokenizer(false);
      //parser.testStreamTokenizer(true);
      //parser.testPrimativeTokenizer(false);
      //parser.testPrimativeTokenizer(true);
      //parser.testAutodocTokenizer(false);
      //parser.testAutodocTokenizer(true);
      //parser.testPreprocessor(false);
      //parser.testPreprocessor(true);
      //parser.test(false);
      //parser.test(true);
      //parser.test(false, true);
      parser.test(true, true);
      //print stored data
      print();
    }
    else {
      parser.initialize();
      parser.parse();
      //print stored data
      //print();
    }
  }
}
/**
 *<p> $$Log$
 *<p> $Revision 1.20  2005/10/12 22:43:12  sueh
 *<p> $bug# 532 Added isSectionExists(String type) to find out if a type of
 *<p> $section is in the autodoc.
 *<p> $
 *<p> $Revision 1.19  2005/09/21 16:36:26  sueh
 *<p> $bug# 532 Removed initialization from constructor.  Calling initialization
 *<p> $from getAutodoc().  Removed getFile().  Added
 *<p> $setAutodocFile(String name, AxisID, String envVariable), which is called
 *<p> $by initialize().  It sets autodocFile (was file).  SetAutodocFile() reports all
 *<p> $problems it finds to the error log.
 *<p> $
 *<p> $Revision 1.18  2005/09/01 17:58:14  sueh
 *<p> $bug# 532  Change getAutodoc() to allow specification of the autodoc
 *<p> $location environment variable for each autodoc.  Get the cpu autodoc from
 *<p> $the calibration directory.
 *<p> $
 *<p> $Revision 1.17  2005/08/27 22:34:44  sueh
 *<p> $bug# 532 Added cpu.adoc.  Added getAttribute(String name) to get the
 *<p> $file level attributes.
 *<p> $
 *<p> $Revision 1.16  2005/07/29 00:53:55  sueh
 *<p> $bug# 709 Going to EtomoDirector to get the current manager is unreliable
 *<p> $because the current manager changes when the user changes the tab.
 *<p> $Passing the manager where its needed.
 *<p> $
 *<p> $Revision 1.15  2005/05/17 19:35:44  sueh
 *<p> $bug# 658 Added getSection(SectionLocation) to return a section based on
 *<p> $SectionLocation.  Added getAttributeValues() to get a HashMap of
 *<p> $attribute values based on section type and attribute name.  This is being
 *<p> $used to get the values of all attributes called "required" in field sections.
 *<p> $
 *<p> $Revision 1.14  2005/05/12 01:30:37  sueh
 *<p> $bug# 658 Added beadtrack.
 *<p> $
 *<p> $Revision 1.13  2005/04/25 20:53:33  sueh
 *<p> $bug# 615 Passing the axis where a command originates to the message
 *<p> $functions so that the message will be popped up in the correct window.
 *<p> $This requires adding AxisID to many objects.
 *<p> $
 *<p> $Revision 1.12  2005/02/23 01:42:10  sueh
 *<p> $bug# 600 Adding solvematch to autodoc.
 *<p> $
 * <p> $Revision 1.11  2005/02/22 20:57:22  sueh
 * <p> $bug# 600 Adding ccderaser to autodoc.
 * <p> $
 * <p> $Revision 1.10  2005/02/11 16:44:50  sueh
 * <p> $bug# 600 Adding tiltalign.
 * <p> $
 * <p> $Revision 1.9  2004/11/30 18:28:54  sueh
 * <p> $bug# 556 Added combinefft autodoc.
 * <p> $
 * <p> $Revision 1.8  2004/03/30 17:42:16  sueh
 * <p> $bug# 409 adding mtffilter.adoc
 * <p> $
 * <p> $Revision 1.7  2004/01/02 18:04:53  sueh
 * <p> $bug# 372 adding doc
 * <p> $
 * <p> $Revision 1.6  2004/01/01 00:44:50  sueh
 * <p> $bug# 372 correcting interface name
 * <p> $
 * <p> $Revision 1.5  2003/12/31 17:46:43  sueh
 * <p> $bug# 372 add getFile()
 * <p> $
 * <p> $Revision 1.4  2003/12/31 02:01:36  sueh
 * <p> $bug# 372 fixed environment variable names
 * <p> $
 * <p> $Revision 1.3  2003/12/31 01:24:44  sueh
 * <p> $bug# 372 added autodoc data storage and retrieval
 * <p> $
 * <p> $Revision 1.2  2003/12/23 21:31:04  sueh
 * <p> $bug# 372 reformat.  Pass this pointer to AutodocParser, so
 * <p> $autodoc info can be stored in Autodoc.
 * <p> $
 * <p> $Revision 1.1  2003/12/22 23:47:45  sueh
 * <p> $bug# 372 Autodoc contains informatio from the autodoc file.
 * <p> $It instantiates at most one per type of autodoc file.
 * <p> $$ </p>
 */
