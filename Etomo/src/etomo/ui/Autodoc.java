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
* <p>Description:</p>
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
* <p> $Revision 1.2  2003/12/23 21:31:04  sueh
* <p> $bug# 372 reformat.  Pass this pointer to AutodocParser, so
* <p> $autodoc info can be stored in Autodoc.
* <p> $
* <p> $Revision 1.1  2003/12/22 23:47:45  sueh
* <p> $bug# 372 Autodoc contains informatio from the autodoc file.
* <p> $It instantiates at most one per type of autodoc file.
* <p> $$ </p>
*/

public class Autodoc implements AttributeInterface {
  public static final String rcsid =
    "$$Id$$";

  public static final String PATH_ENVIRONMENT_VARIABLE =
    new String("AUTODOC_PATH");
  public static final String TILTXCORR = new String("tiltxcorr");
  public static final String TEST = new String("test");

  private static final String fileExt = new String(".adoc");

  private static Autodoc tiltxcorr = null;
  private static Autodoc test = null;

  private String dirName = null;
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
    throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
  }

  public String getName() {
    return fileName;
  }
  
  public Section addSection(Token type, Token name) {
    if (sectionList == null) {
      sectionList = new Vector();
      sectionMap = new HashMap();
    }
    Section  existingSection = null;
    String key = Section.getKey(type, name);
    existingSection = (Section) sectionMap.get(key);
    if (existingSection == null) {
      Section newSection = new Section (type, name);
      sectionList.add(newSection);
      sectionMap.put(newSection.getKey(), newSection);
      return newSection;
    }
    return existingSection;
  }
  
  public AttributeInterface addAttribute(Token name) {
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
      return new Section();
    }
    String key = Section.getKey(type, name);
    Section section = (Section) sectionMap.get(key);
    if (section == null) {
      return new Section();
    }
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
      return new Section();
    }
    Section section = null;
    for (int i = location.getIndex() + 1; i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      if (section.equalsType(location.getType())) {
        location.setIndex(i);
        return section;
      }
    }
    return new Section();
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
    dirName =
      new String(Utilities.getEnvironmentVariable(PATH_ENVIRONMENT_VARIABLE));
    file = new File(dirName, fileName);
  }

  private void initialize() throws FileNotFoundException, IOException {
    String errorMessage = null;
    if (file == null) {
      errorMessage =
        "Unable to open autodoc file, " + fileName + " , in " + dirName + ".";
    }
    if (!file.exists()) {
      errorMessage =
        "The autodoc file, "
          + fileName
          + " , in "
          + dirName
          + " does not exist.";
    }
    if (!file.canRead()) {
      errorMessage =
        "The autodoc file, "
          + fileName
          + " , in "
          + dirName
          + " is not readable.";
    }
    if (errorMessage != null) {
      throw new FileNotFoundException(errorMessage);
    }
    parser = new AutodocParser(this, file);
    if (testMode) {
      //parser.testStreamTokenizer(false);
      //parser.testStreamTokenizer(true);
      //parser.testPrimativeTokenizer(false);
      //parser.testPrimativeTokenizer(true);
      //parser.testAutodocTokenizer(false);
      //parser.testAutodocTokenizer(true);
      //parser.testPreprocessor(false);
      //parser.testPreprocessor(true);
      parser.testParser(false);
      //parser.testParser(true);
      //parser.testParser(false, true);
      //parser.testParser(true, true);

    }
    else {
      parser.initialize();
      parser.parse();
    }
  }
  
}
