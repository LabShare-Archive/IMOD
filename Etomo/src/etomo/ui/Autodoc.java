package etomo.ui;

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
* <p> $$Log$
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

public class Autodoc implements AttributeCollection {
  public static final String rcsid =
    "$$Id$$";

  public static final String AUTODOC_DIR = new String("AUTODOC_DIR");
  public static final String IMOD_DIR = new String("IMOD_DIR");
  public static final String DEFAULT_AUTODOC_DIR = new String("autodoc");
  public static final String TILTXCORR = new String("tiltxcorr");
  public static final String MTF_FILTER = new String("mtffilter");
  public static final String TEST = new String("test");

  private static final String fileExt = new String(".adoc");

  private static Autodoc tiltxcorr = null;
  private static Autodoc test = null;
  private static Autodoc mtffilter = null;

  private String fileName = null;
  private File file = null;
  private AutodocParser parser = null;
  private boolean testMode = false;

  //data
  private HashMap metaData = null;
  private Vector sectionList = null;
  private HashMap sectionMap = null;

  public static Autodoc get(String name)
    throws FileNotFoundException, IOException {
    if (name == null) {
      throw new IllegalArgumentException("name is null.");
    }
    if (name.equals(TILTXCORR)) {
      tiltxcorr = getAutodoc(tiltxcorr, name);
      return tiltxcorr;
    }
    if (name.equals(TEST)) {
      test = getAutodoc(test, name);
      return test;
    }
    if (name.equals(MTF_FILTER)) {
      mtffilter = getAutodoc(test, name);
      return mtffilter;
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

  public void print() {
    System.out.println("Autodoc: " + fileName);
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

  private static Autodoc getAutodoc(Autodoc autodoc, String name)
    throws FileNotFoundException, IOException {
    if (autodoc == null) {
      autodoc = new Autodoc(name);
      autodoc.initialize();
    }
    return autodoc;
  }

  private Autodoc(String name) {
    fileName = new String(name + fileExt);
    String dirName = new String(Utilities.getEnvironmentVariable(AUTODOC_DIR));
    file = new File(dirName, fileName);
    if (file.exists()) {
      return;
    }
    dirName = new String(Utilities.getEnvironmentVariable(IMOD_DIR));
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
