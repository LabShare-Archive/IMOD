package etomo.storage.autodoc;

import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.ui.Token;
import etomo.util.DatasetFiles;
import etomo.util.EnvironmentVariable;
import etomo.util.Utilities;

import java.awt.geom.IllegalPathStateException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;

/**
 * <p>Description:  Data storage for an autodoc file.
 * 
 * Versions:
 * 1.0
 * 1.1:  Added the break character "^".  When a value is formatted, the "^" is
 * replaced with a "\n".
 * 1.2:  Handling duplicate attribute names.  Duplicate attributes are
 * attributes with the same parentage (section and parent attribute names) and
 * the same name.  Before version 1.2 the last value assigned to a duplicate
 * attribute could be retrieved by using the name as a key.  In version 1.2
 * the first value assigned to a duplicate attribute can be retrieved by using the
 * name as a key.  When attributes of the same parentage are retrieved as an
 * ordered list, each duplicate attribute, and their different values, will be
 * included in the list.
 * 
 * The break character is no longer being handled by autodoc.  It is now considered
 * a "flavor" of autodoc.
 * 
 * To Use:
 * 
 * Set up environment variables:  make sure that either the AUTODOC_DIR or
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
 * <p>Copyright: Copyright 2002 - 2005</p>
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

final class Autodoc extends WriteOnlyNameValuePairList implements
    WritableAutodoc {
  public static final String rcsid = "$$Id$$";

  private static final String AUTODOC_DIR = "AUTODOC_DIR";
  private static final String IMOD_DIR = "IMOD_DIR";
  private static final String DEFAULT_AUTODOC_DIR = "autodoc";

  private static String absoluteDir = null;

  private final boolean allowAltComment;

  private LogFile autodocFile = null;
  private AutodocParser parser = null;

  //data
  private final AttributeMap attributeMap;
  private final Vector sectionList = new Vector();
  private final HashMap sectionMap = new HashMap();
  private final Vector nameValuePairList = new Vector();
  private String currentDelimiter = AutodocTokenizer.DEFAULT_DELIMITER;

  Autodoc() {
    this(false);
  }

  Autodoc(boolean allowAltComment) {
    this.allowAltComment = allowAltComment;
    attributeMap = new AttributeMap(this, this);
  }

  static void resetAbsoluteDir() {
    absoluteDir = null;
  }

  static void setAbsoluteDir(String absoluteDir) {
    Autodoc.absoluteDir = absoluteDir;
  }

  public String getName() {
    return autodocFile.getName();
  }

  LogFile getAutodocFile() {
    return autodocFile;
  }

  Section addSection(Token type, Token name) {
    Section existingSection = null;
    String key = Section.getKey(type, name);
    existingSection = (Section) sectionMap.get(key);
    if (existingSection == null) {
      Section newSection = new Section(type, name, this);
      sectionList.add(newSection);
      sectionMap.put(newSection.getKey(), newSection);
      return newSection;
    }
    return existingSection;
  }

  boolean isGlobal() {
    return true;
  }

  boolean isAttribute() {
    return false;
  }

  void setCurrentDelimiter(Token newDelimiter) {
    currentDelimiter = newDelimiter.getValues();
  }

  String getCurrentDelimiter() {
    return currentDelimiter;
  }

  WriteOnlyAttributeMap addAttribute(Token name) {
    return attributeMap.addAttribute(name);
  }

  public void write() throws LogFile.FileException, LogFile.WriteException {
    autodocFile.backup();
    long writeId = autodocFile.openWriter();
    for (int i = 0; i < nameValuePairList.size(); i++) {
      ((NameValuePair) nameValuePairList.get(i)).write(autodocFile, writeId);
    }
    for (int j = 0; j < sectionList.size(); j++) {
      ((Section) sectionList.get(j)).write(autodocFile, writeId);
    }
    autodocFile.closeWriter(writeId);
  }

  public void addAttributeAndNameValuePair(String name,
      String value) {
    //add attribute
    Token nameToken = new Token();
    nameToken.set(Token.Type.ONE_OR_MORE, name);
    attributeMap.addAttribute(nameToken);
    //add value to attribute
    Attribute attribute = attributeMap.getAttribute(name);
    Token valueToken = new Token();
    valueToken.set(Token.Type.ONE_OR_MORE, value);
    attribute.setValue(valueToken);
    //add name/value pair
    NameValuePair pair = addNameValuePair();
    //add name and value to pair
    pair.addAttribute(nameToken);
    pair.addValue(valueToken);
  }

  public ReadOnlyAttribute getAttribute(String name) {
    return attributeMap.getAttribute(name);
  }

  public WritableAttribute getWritableAttribute(String name) {
    return attributeMap.getAttribute(name);
  }

  NameValuePair addNameValuePair() {
    NameValuePair pair = NameValuePair.getNameValuePairInstance(this);
    nameValuePairList.add(pair);
    return pair;
  }

  public void addComment(Token comment) {
    nameValuePairList.add(NameValuePair.getCommentInstance(comment, this));
  }
  
  public void addComment(String comment) {
    Token token = new Token();
    token.set(Token.Type.ONE_OR_MORE,comment);
    addComment(token);
  }

  void addDelimiterChange(Token newDelimiter) {
    nameValuePairList.add(NameValuePair.getDelimiterChangeInstance(
        newDelimiter, this));
  }

  public void addEmptyLine() {
    nameValuePairList.add(NameValuePair.getEmptyLineInstance(this));
  }

  public NameValuePairLocation getNameValuePairLocation() {
    if (nameValuePairList == null) {
      return null;
    }
    return new NameValuePairLocation();
  }

  public NameValuePair nextNameValuePair(NameValuePairLocation location) {
    if (nameValuePairList == null || location == null
        || location.isOutOfRange(nameValuePairList)) {
      return null;
    }
    NameValuePair pair = (NameValuePair) nameValuePairList.get(location
        .getIndex());
    location.increment();
    return pair;
  }

  public ReadOnlySection getSection(String type, String name) {
    if (sectionMap == null) {
      return null;
    }
    String key = Section.getKey(type, name);
    Section section = (Section) sectionMap.get(key);
    return section;
  }

  public boolean sectionExists(String type) {
    return getSectionLocation(type) != null;
  }

  public SectionLocation getSectionLocation(String type) {
    Section section = null;
    for (int i = 0; i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      if (section.equalsType(type)) {
        return new SectionLocation(type, i);
      }
    }
    return null;
  }

  public ReadOnlySection nextSection(SectionLocation location) {
    if (location == null) {
      return null;
    }
    Section section = null;
    for (int i = location.getIndex(); i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      if (section.equalsType(location.getType())) {
        location.setIndex(i + 1);
        return section;
      }
    }
    return null;
  }
  
  public HashMap getAttributeValues(String sectionType, String attributeName) {
    return getAttributeValues( sectionType,  attributeName,
         false);
  }
  
  public HashMap getAttributeMultiLineValues(String sectionType, String attributeName) {
    return getAttributeValues( sectionType,  attributeName,
         true);
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
  private HashMap getAttributeValues(String sectionType, String attributeName,
      boolean multiLine) {
    if (sectionType == null || attributeName == null) {
      return null;
    }
    //Create attributeValues
    HashMap attributeValues = new HashMap();
    SectionLocation sectionLocation = getSectionLocation(sectionType);
    ReadOnlySection section = nextSection(sectionLocation);
    while (section != null) {
      try {
        String sectionName = section.getName();
        if (multiLine) {
          attributeValues.put(sectionName, section.getAttribute(attributeName)
              .getMultiLineValue());
        }
        else {
          attributeValues.put(sectionName, section.getAttribute(attributeName)
              .getValue());
        }
      }
      catch (NullPointerException e) {
        //An attribute with attributeName doesn't exist
      }
      //Go to next section
      section = nextSection(sectionLocation);
    }
    return attributeValues;
  }

  public void printStoredData() {
    System.out.println("Printing stored data:");
    //name value pair list
    System.out.println("LIST:");
    if (nameValuePairList != null) {
      NameValuePair nameValuePair = null;
      for (int i = 0; i < nameValuePairList.size(); i++) {
        nameValuePair = (NameValuePair) nameValuePairList.get(i);
        nameValuePair.print(0);
      }
    }
    //attribute map
    System.out.println("MAP:");
    attributeMap.print(0);
    //section list
    if (sectionList != null) {
      Section section = null;
      for (int i = 0; i < sectionList.size(); i++) {
        section = (Section) sectionList.get(i);
        section.print(0);
      }
    }
  }

  static void printIndent(int level) {
    if (level == 0) {
      return;
    }
    for (int i = 0; i < level; i++) {
      System.out.print("  ");
    }
  }

  /**
   * sets the autodoc file
   * @param name
   * @param axisID
   * @param envVariable
   */
  private LogFile setAutodocFile(String name, AxisID axisID, String envVariable) {
    File dir = getAbsoluteDir();
    if (dir != null) {
      return getAutodocFile(dir, name);
    }
    if (envVariable != null && !envVariable.matches("\\s*+")) {
      //if envVariable is set, then it points to the only valid directory for this
      //autodoc
      dir = Utilities.getExistingDir(envVariable, axisID);
      if (dir == null) {
        System.err.println("Warning:  can't open the " + name
            + " autodoc file.\nThis autodoc should be stored in $"
            + envVariable + ".\n");
        return null;
      }
      return getAutodocFile(dir, name);
    }
    dir = Utilities.getExistingDir(AUTODOC_DIR, axisID);
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

  private LogFile getAutodocFile(File autodocDir, String autodocName) {
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
    return LogFile.getInstance(file);
  }

  private File getAbsoluteDir() {
    if (absoluteDir == null) {
      return null;
    }
    File dir = new File(absoluteDir);
    if (!dir.isAbsolute()) {
      throw new IllegalPathStateException(absoluteDir
          + " is not an absolute path");
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

  public String getString() {
    return autodocFile.getAbsolutePath();
  }

  private File getDir(String envVariable, String dirName, AxisID axisID) {
    File parentDir = Utilities.getExistingDir(envVariable, axisID);
    if (parentDir == null) {
      return null;
    }
    File dir = new File(parentDir, dirName);
    if (!Utilities.checkExistingDir(dir, envVariable)) {
      return null;
    }
    return dir;
  }

  void initializeUITestAxis(LogFile autodocFile, AxisID axisID)
      throws FileNotFoundException, IOException, LogFile.ReadException {
    this.autodocFile = autodocFile;
    initialize(null, axisID, null, true);
  }

  void initialize(String name, AxisID axisID) throws FileNotFoundException,
      IOException, LogFile.ReadException {
    initialize(name, axisID, null, true);
  }

  void initializeUITest(String name, AxisID axisID)
      throws FileNotFoundException, IOException, LogFile.ReadException {
    initialize(name, axisID, "IMOD_UITEST_SOURCE", true);
  }

  void initializeCpu(String name, AxisID axisID) throws FileNotFoundException,
      IOException, LogFile.ReadException {
    initialize(name, axisID, EnvironmentVariable.CALIB_DIR, true);
  }

  /**
   * Initializes parser and prints parsing data instead of storing it.
   * @param type
   * @param showTokens
   * @param showDetails
   * @throws IOException
   * @throws LogFile.ReadException
   */
  void runInternalTest(InternalTestType type, boolean showTokens,
      boolean showDetails) throws IOException, LogFile.ReadException {
    if (type == InternalTestType.STREAM_TOKENIZER) {
      parser.testStreamTokenizer(showTokens, showDetails);
    }
    else if (type == InternalTestType.PRIMATIVE_TOKENIZER) {
      parser.testPrimativeTokenizer(showTokens);
    }
    else if (type == InternalTestType.AUTODOC_TOKENIZER) {
      parser.testAutodocTokenizer(showTokens);
    }
    else if (type == InternalTestType.PREPROCESSOR) {
      parser.testPreprocessor(showTokens);
    }
    else if (type == InternalTestType.PARSER) {
      parser.test(showTokens, showDetails);
    }
  }

  public boolean isError() {
    if (parser == null) {
      return true;
    }
    return parser.isError();
  }

  /**
   * Initial and parse.  Will not parse if the storeData parameter is false.
   * Set the storeData parameter to false to run an internal parsing test
   * (runInternalTest).
   * @param name
   * @param axisID
   * @param envVariable
   * @param storeData
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void initialize(String name, AxisID axisID, String envVariable,
      boolean storeData) throws FileNotFoundException, IOException,
      LogFile.ReadException {
    if (autodocFile == null) {
      autodocFile = setAutodocFile(name, axisID, envVariable);
    }
    if (autodocFile == null) {
      return;
    }
    parser = new AutodocParser(this, false,true);
    if (storeData) {
      parser.initialize();
      parser.parse();
    }
  }

  /**
   * Initial and parse.  Will not parse if the storeData parameter is false.
   * Set the storeData parameter to false to run an internal parsing test
   * (runInternalTest).
   * @param file
   * @param storeData
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  void initialize(File file, boolean storeData,boolean versionRequired) throws FileNotFoundException,
      IOException, LogFile.ReadException {
    autodocFile = LogFile.getInstance(file);
    parser = new AutodocParser(this, allowAltComment,versionRequired);
    if (storeData) {
      parser.initialize();
      parser.parse();
    }
  }

  static final class InternalTestType {
    static final InternalTestType STREAM_TOKENIZER = new InternalTestType();
    static final InternalTestType PRIMATIVE_TOKENIZER = new InternalTestType();
    static final InternalTestType AUTODOC_TOKENIZER = new InternalTestType();
    static final InternalTestType PREPROCESSOR = new InternalTestType();
    static final InternalTestType PARSER = new InternalTestType();

    private InternalTestType() {
    }
  }
}
/**
 *<p> $$Log$
 *<p> $Revision 1.16  2007/03/23 20:30:09  sueh
 *<p> $bug# 964 Added addAttributeAndNameValuePair, addComment(String), write(),
 *<p> $getWritableAttribute,getAttributeMultiLineValues.
 *<p> $
 *<p> $Revision 1.15  2007/03/21 18:14:26  sueh
 *<p> $bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 *<p> $Added AutodocFactory to create Autodoc instances.
 *<p> $
 *<p> $Revision 1.14  2007/03/15 21:44:50  sueh
 *<p> $bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 *<p> $unless the Attribute needs to be modified.
 *<p> $
 *<p> $Revision 1.13  2007/03/08 21:53:12  sueh
 *<p> $bug# 964 Save name/value pairs in the parser instead of saving them from the
 *<p> $Attribute.  This is necessary because the name/value pair must be placed in the
 *<p> $autodoc or section as soon as they are found to preserve the original order of the
 *<p> $autodoc file.
 *<p> $
 *<p> $Revision 1.12  2007/03/07 21:05:24  sueh
 *<p> $bug# 964 Fixed printing.  Made internal tests runnable from unit tests.
 *<p> $
 *<p> $Revision 1.11  2007/03/05 21:28:28  sueh
 *<p> $bug# 964 Stop controlling autodoc instances, except for the standard ones.
 *<p> $
 *<p> $Revision 1.10  2007/03/01 01:16:12  sueh
 *<p> $bug# 964 Added mutable boolean.  Added getMatlabInstance.
 *<p> $
 *<p> $Revision 1.9  2007/02/09 00:42:37  sueh
 *<p> $bug# 962 Added xfjointomo autodoc.
 *<p> $
 *<p> $Revision 1.8  2006/11/16 23:38:16  sueh
 *<p> $bug# 872 Changed setDir_test to setTestDir.  Changed getTestAutodocDir to
 *<p> $getTestDir.
 *<p> $
 *<p> $Revision 1.7  2006/09/13 23:30:50  sueh
 *<p> $bug# 921 Adding corrsearch3d.adoc.
 *<p> $
 *<p> $Revision 1.6  2006/07/21 22:11:32  sueh
 *<p> $bug# 901 Getting the calibration directory environment variable name from
 *<p> $EnvironmentVariable.
 *<p> $
 *<p> $Revision 1.5  2006/06/14 21:19:54  sueh
 *<p> $bug# 852 Added isAttribute().
 *<p> $
 *<p> $Revision 1.4  2006/06/14 00:15:20  sueh
 *<p> $bug# 852 Added densmatch because it is small and good for basic debugging of
 *<p> $the parcer.  Added getInstance(String fileName...) so that it is easy to open a
 *<p> $uitestaxis autodoc in the ui test source directory.
 *<p> $
 *<p> $Revision 1.3  2006/05/01 21:16:26  sueh
 *<p> $bug# 854
 *<p> $
 *<p> $Revision 1.2  2006/04/25 18:54:22  sueh
 *<p> $bug# 787 Implemented ReadOnlyNameValuePairList so that name/value
 *<p> $pairs can be read from either a global or section area using the same
 *<p> $code.
 *<p> $
 *<p> $Revision 1.1  2006/01/12 17:02:22  sueh
 *<p> $bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 *<p> $
 *<p> $Revision 1.25  2006/01/11 21:55:25  sueh
 *<p> $bug# 675 Removed attributeList.  Added attributeMap and
 *<p> $nameValuePairList.  Removed getAttributeLocation and nextAttribute.
 *<p> $Added addNameValuePair, getNameValuePairLocation, and
 *<p> $nextNameValuePair.
 *<p> $
 *<p> $Revision 1.24  2006/01/03 23:27:49  sueh
 *<p> $bug# 675 Converted metaData to an AttributeList.  Added UITEST_AXIS.
 *<p> $Added UITEST_AXIS_MAP, so that multiple UITEST_AXIS adocs can be
 *<p> $opened at once.  Added getAttributeLocation and nextAttribute so that
 *<p> $a list of attributes can be traversed.
 *<p> $
 *<p> $Revision 1.23  2005/12/23 02:13:36  sueh
 *<p> $bug# 675 Added UITEST type of autodoc.  Added the ability to pass the
 *<p> $autodoc file when opening an autodoc.  This is for test only and gives more
 *<p> $flexibility in autodoc names.  The name must still start with the standard
 *<p> $autodoc name and end in .adoc.
 *<p> $
 *<p> $Revision 1.22  2005/11/10 22:20:31  sueh
 *<p> $bug# 759 Added VERSION constant.
 *<p> $
 *<p> $Revision 1.21  2005/11/10 18:14:16  sueh
 *<p> $bug# 733 added setTestDir(), which sets the autodoc directory directly and
 *<p> $can only be used in test mode.  Rewrote getTestAutodocDir() to let it use
 *<p> $the test directory and to break it up into a group of less complicated
 *<p> $functions.
 *<p> $
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
