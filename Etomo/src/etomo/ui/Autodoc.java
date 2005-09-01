package etomo.ui;

import etomo.type.AxisID;
import etomo.util.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.lang.IllegalArgumentException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;
import java.util.Collection;
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
  public static final String rcsid =
    "$$Id$$";

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

  private static final String fileExt = new String(".adoc");

  private static Autodoc tiltxcorr = null;
  private static Autodoc test = null;
  private static Autodoc mtffilter = null;
  private static Autodoc combinefft = null;
  private static Autodoc tiltalign = null;
  private static Autodoc ccderaser = null;
  private static Autodoc solvematch = null;
  private static Autodoc beadtrack = null;
  private static Autodoc cpu = null;

  private String fileName = null;
  private File file = null;
  private AutodocParser parser = null;
  private boolean testMode = false;

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
      throw new IllegalArgumentException("name is null.");
    }
    if (name.equals(TILTXCORR)) {
      tiltxcorr = getAutodoc(tiltxcorr, name, axisID);
      return tiltxcorr;
    }
    if (name.equals(TEST)) {
      test = getAutodoc(test, name, axisID);
      return test;
    }
    if (name.equals(MTF_FILTER)) {
      mtffilter = getAutodoc(mtffilter, name, axisID);
      return mtffilter;
    }
    if (name.equals(COMBINE_FFT)) {
      combinefft = getAutodoc(combinefft, name, axisID);
      return combinefft;
    }
    if (name.equals(TILTALIGN)) {
      tiltalign = getAutodoc(tiltalign, name, axisID);
      return tiltalign;
    }
    if (name.equals(CCDERASER)) {
      ccderaser = getAutodoc(ccderaser, name, axisID);
      return ccderaser;
    }
    if (name.equals(SOLVEMATCH)) {
      solvematch = getAutodoc(solvematch, name, axisID);
      return solvematch;
    }
    if (name.equals(BEADTRACK)) {
      beadtrack = getAutodoc(beadtrack, name, axisID);
      return beadtrack;
    }
    if (name.equals(CPU)) {
      cpu = getAutodoc(cpu, name, axisID, "IMOD_CALIB_DIR");
      return cpu;
    }
    throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
  }

  public String getName() {
    return fileName;
  }
  
  public File getFile() {
    return file;
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
  public final HashMap getAttributeValues(String sectionType, String attributeName) {
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
        String attributeValue = section.getAttribute(attributeName).getUnformattedValue();
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
    if (metaData != null) {
      Attribute metaDataElement = null;
      Collection collection = metaData.values();
      Iterator iterator = collection.iterator();
      while (iterator.hasNext()) {
        metaDataElement = (Attribute) iterator.next();
        metaDataElement.print();
      }
    }
    if (sectionList == null) {
      return;
    }
    Section section = null;
    for (int i = 0; i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      section.print();
    }
  }

  private static Autodoc getAutodoc(Autodoc autodoc, String name, AxisID axisID)
  throws FileNotFoundException, IOException {
    return getAutodoc(autodoc, name, axisID, null);
  }
  
  private static Autodoc getAutodoc(Autodoc autodoc, String name, AxisID axisID, String envVariable)
    throws FileNotFoundException, IOException {
    if (autodoc == null) {
      autodoc = new Autodoc(name, axisID, envVariable);
      autodoc.initialize();
    }
    return autodoc;
  }
  
  private Autodoc(String name, AxisID axisID, String envVariable) {
    fileName = new String(name + fileExt);
    String dirName = new String(Utilities.getEnvironmentVariable(null, envVariable,
        axisID));
    file = new File(dirName, fileName);
    if (file.exists()) {
      return;
    }
    fileName = new String(name + fileExt);
    dirName = new String(Utilities.getEnvironmentVariable(null, AUTODOC_DIR,
        axisID));
    file = new File(dirName, fileName);
    if (file.exists()) {
      return;
    }
    dirName = new String(Utilities.getEnvironmentVariable(null, IMOD_DIR, axisID));
    File dir = new File(dirName, DEFAULT_AUTODOC_DIR);
    if (dir.exists()) {
      file = new File(dir, fileName);
    }
    if (file.exists()) {
      return;
    }
    file = new File(fileName);
  }

  private void initialize() throws FileNotFoundException, IOException {
    String errorMessage = null;
    if (file == null) {
      errorMessage =
        "Can't find "
          + fileName
          + ".  Set the environment variable "
          + AUTODOC_DIR;
    }
    else if (!file.exists()) {
      errorMessage =
        "Can't find "
          + file.getAbsolutePath()
          + ".  Set the environment variable "
          + AUTODOC_DIR;
    }

    else if (!file.canRead()) {
      errorMessage = "Can't read " + file.getAbsolutePath() + ".";
    }
    if (errorMessage != null) {
      throw new FileNotFoundException(errorMessage);
    }
    parser = new AutodocParser(this);
    if (testMode) {
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
      //parser.test(true, true);
    }
    else {
      parser.initialize();
      parser.parse();
    }
  }
}
/**
*<p> $$Log$
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
