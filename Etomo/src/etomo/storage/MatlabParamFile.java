package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.storage.autodoc.SectionLocation;
import etomo.storage.autodoc.WritableAttribute;
import etomo.storage.autodoc.WritableAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.ui.UIHarness;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.5  2007/03/23 20:28:00  sueh
 * <p> bug# 964 Added the ability to write the autodoc based on the order of Field sections in another autodoc.  Also has the ability to write the autodoc without
 * <p> referring to the other autodoc.  Can write a new autodoc.  Can also update existing attributes or add new attributes to an existing autodoc.  Tries to add
 * <p> comments to add attributes or a new autodoc based on comment attributes from
 * <p> the other autodoc.
 * <p>
 * <p> Revision 1.4  2007/03/21 18:11:44  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Creating Autodoc using a factory.  Made the Volume inner class public so that it
 * <p> could be responsible for sets and gets.
 * <p>
 * <p> Revision 1.3  2007/03/20 23:02:27  sueh
 * <p> bug# 964 Distinguishing between tiltRange={} and tiltRange={[],[]}.  Returning
 * <p> relativeOrient elements separately.
 * <p>
 * <p> Revision 1.2  2007/03/20 00:43:39  sueh
 * <p> bug# 964 Added Volume.tiltRange and Volume.relativeOrient.
 * <p>
 * <p> Revision 1.1  2007/03/15 21:42:17  sueh
 * <p> bug# 964 A class which represents a .prm file.
 * <p> </p>
 */
public final class MatlabParamFile {
  public static final String rcsid = "$Id$";

  private static final char DIVIDER = ',';
  private static final char OPEN_LIST = '{';
  private static final char CLOSE_LIST = '}';
  private static final String OPEN_LIST_STRING = "{";//stupid java thinks chars are ints
  private static final String CLOSE_LIST_STRING = "}";//stupid java thinks chars are ints
  private static final char OPEN_ARRAY = '[';
  private static final char CLOSE_ARRAY = ']';
  private static final String OPEN_ARRAY_STRING = "[";//stupid java thinks chars are ints
  private static final String CLOSE_ARRAY_STRING = "]";//stupid java thinks chars are ints
  private static final char QUOTE = '\'';
  private static final String DIVIDER_STRING = DIVIDER + " ";
  private static final String FN_VOLUME_KEY = "fnVolume";
  private static final String FN_MOD_PARTICLE_KEY = "fnModParticle";
  private static final String INIT_MOTL_KEY = "initMOTL";
  private static final String TILT_RANGE_KEY = "tiltRange";
  private static final String RELATIVE_ORIENT_KEY = "relativeOrient";
  private final List volumeList = new ArrayList();
  private final File file;
  private final boolean newFile;
  private InitMotlCode initMotlCode = null;
  private boolean tiltRangeEmpty = false;

  public MatlabParamFile(File file, boolean newFile) {
    this.file = file;
    this.newFile = newFile;
  }

  /**
   * Reads data from the .prm autodoc.
   */
  public synchronized void read() {
    volumeList.clear();
    try {
      ReadOnlyAutodoc autodoc = null;
      if (newFile) {
        autodoc = (AutodocFactory.getEmptyMatlabInstance(file));
      }
      else {
        autodoc = (AutodocFactory.getMatlabInstance(file));
        if (autodoc == null) {
          UIHarness.INSTANCE.openMessageDialog("Unable to read "
              + file.getName() + ".", "File Error");
          return;
        }
      }
      //Parse each pair's value.
      String[] fnVolumeList = getList(autodoc.getAttribute(FN_VOLUME_KEY));
      String[] fnModParticleList = getList(autodoc
          .getAttribute(FN_MOD_PARTICLE_KEY));
      //see if initMotl is using a single code
      String initMotl = autodoc.getAttribute(INIT_MOTL_KEY).getValue();
      initMotlCode = InitMotlCode.getInstance(initMotl);
      String[] initMotlList = null;
      if (initMotlCode == null) {
        initMotlList = getList(initMotl);
      }
      String[] tiltRangeList = getList(autodoc.getAttribute(TILT_RANGE_KEY));
      String[][] tiltRangeArrayList = null;
      //Distinguish between {} (an empty list) and {[],[]}
      if (tiltRangeList.length == 0) {
        //empty list
        tiltRangeEmpty = true;
      }
      else {
        //not an empty list
        tiltRangeArrayList = getListOfArrays(tiltRangeList);
      }
      String[][] relativeOrientArrayList = getListOfArrays(getList(autodoc
          .getAttribute(RELATIVE_ORIENT_KEY)));
      //Add entries to volumeList, ignoring any entries for which there is no
      //corresponding fnVolume.
      for (int i = 0; i < fnVolumeList.length; i++) {
        Volume volume = new Volume();
        volume.setFnVolume(removeQuotes(fnVolumeList[i]));
        if (i < fnModParticleList.length) {
          volume.setFnModParticle(removeQuotes(fnModParticleList[i]));
        }
        if (initMotlList != null && i < initMotlList.length) {
          volume.setInitMotl(removeQuotes(initMotlList[i]));
        }
        if (!tiltRangeEmpty && i < tiltRangeArrayList.length) {
          volume.setTiltRange(tiltRangeArrayList[i]);
        }
        if (i < relativeOrientArrayList.length) {
          volume.setRelativeOrient(relativeOrientArrayList[i]);
        }
        volumeList.add(volume);
      }
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to load " + file.getName()
          + ".  IOException:  " + e.getMessage(), "File Error");
    }
    catch (LogFile.ReadException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to read " + file.getName()
          + ".  LogFile.ReadException:  " + e.getMessage(), "File Error");
    }
  }

  /**
   * Write stored data to the .prm autodoc.
   */
  public synchronized void write() {
    //create ParamList instances to build the values.
    ParamList fnVolume = new ParamList(true);
    ParamList fnModParticle = new ParamList(true);
    ParamList initMotlFile = null;
    if (initMotlCode == null) {
      initMotlFile = new ParamList(true);
    }
    ParamList tiltRange = new ParamList(false);
    ParamList relativeOrient = new ParamList(false);
    //build the values
    for (int i = 0; i < volumeList.size(); i++) {
      Volume volume = (Volume) volumeList.get(i);
      fnVolume.add(volume.getFnVolume());
      fnModParticle.add(volume.getFnModParticle());
      if (initMotlFile != null) {
        initMotlFile.add(volume.getInitMotl());
      }
      String array = "";
      if (!tiltRangeEmpty) {
        array = buildArray(volume.getTiltRange());
        tiltRange.add(array);
      }
      array = buildArray(volume.getRelativeOrient());
      relativeOrient.add(array);
    }
    //Place the string representation of each value in a map.
    //This allows the values to be passed to updateOrBuildAutodoc().
    //When building a new .prm autodoc, this also allows the values to be
    //accessed in the same order as the Field sections in peetprm.adoc.
    Map valueMap = new HashMap();
    valueMap.put(FN_VOLUME_KEY, fnVolume.toString());
    valueMap.put(FN_MOD_PARTICLE_KEY, fnModParticle.toString());
    if (initMotlFile != null) {
      valueMap.put(INIT_MOTL_KEY, initMotlFile.toString());
    }
    else {
      valueMap.put(INIT_MOTL_KEY, initMotlCode.getCodeString());
    }
    valueMap.put(TILT_RANGE_KEY, tiltRange.toString());
    valueMap.put(RELATIVE_ORIENT_KEY, relativeOrient.toString());
    //try to get the peetprm.adoc, which contains the comments for the .prm file
    //in its Field sections.
    ReadOnlyAutodoc commentAutodoc = null;
    try {
      commentAutodoc = AutodocFactory.getInstance(AutodocFactory.PEET_PRM,
          AxisID.ONLY);
    }
    catch (IOException e) {
      System.err.println("Problem with " + AutodocFactory.PEET_PRM
          + ".adoc.\nIOException:  " + e.getMessage());
    }
    catch (LogFile.ReadException e) {
      System.err.println("Problem with " + AutodocFactory.PEET_PRM
          + ".adoc.\nLogFile.ReadException:  " + e.getMessage());
    }
    try {
      WritableAutodoc autodoc = null;
      if (!newFile) {
        //try to get the existing .prm autodoc, unless this is an new PEET
        autodoc = AutodocFactory.getMatlabInstance(file);
      }
      if (autodoc != null) {
        //found an existing .prm autodoc - update it.
        updateOrBuildAutodoc(valueMap, autodoc, commentAutodoc);
      }
      else {
        //get an empty .prm autodoc
        autodoc = AutodocFactory.getEmptyMatlabInstance(file);
        if (commentAutodoc == null) {
          //The peetprm.adoc is not available.
          //Build a new .prm autodoc with no comments
          updateOrBuildAutodoc(valueMap, autodoc, null);
        }
        else {
          //Get the Field sections from the peetprm.adoc
          SectionLocation secLoc = commentAutodoc
              .getSectionLocation(EtomoAutodoc.FIELD_SECTION_NAME);
          if (secLoc == null) {
            //There are no Field sections in the peetprm.adoc.
            //Build a new .prm autodoc with no comments
            updateOrBuildAutodoc(valueMap, autodoc, null);
          }
          else {
            //Build a new .prm autodoc.  Use the Field sections from the
            //peetprm.adoc to dictate the order of the name/value pairs.
            //Also use the comments from the peetprm.adoc Field sections.
            ReadOnlySection section = null;
            while ((section = commentAutodoc.nextSection(secLoc)) != null) {
              addNameValuePair(autodoc, section.getName(), (String) valueMap
                  .get(section.getName()), section
                  .getAttribute(EtomoAutodoc.COMMENT_KEY));
            }
          }
        }
      }
      //write the autodoc file
      autodoc.write();
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to load " + file.getName()
          + ".  IOException:  " + e.getMessage(), "File Error");
    }
    catch (LogFile.ReadException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to read " + file.getName()
          + ".  LogFile.ReadException:  " + e.getMessage(), "File Error");
    }
    catch (LogFile.FileException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to back up "
          + file.getName() + ".  LogFile.FileException:  " + e.getMessage(),
          "File Error");
    }
    catch (LogFile.WriteException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to write to "
          + file.getName() + ".  LogFile.WriteException:  " + e.getMessage(),
          "File Error");
    }
  }

  /**
   * Updates or adds all the attributes to autodoc.
   * Will attempt to add comments when adding a new attribute.
   * Addes name/value pair when it adds a new attribute.
   * @param valueMap
   * @param autodoc
   * @param commentAutodoc
   */
  private void updateOrBuildAutodoc(Map valueMap, WritableAutodoc autodoc,
      ReadOnlyAutodoc commentAutodoc) {
    Map commentMap = null;
    if (commentAutodoc != null) {
      commentMap = commentAutodoc.getAttributeMultiLineValues(
          EtomoAutodoc.FIELD_SECTION_NAME, EtomoAutodoc.COMMENT_KEY);
    }
    //write to a autodoc, constructing attributes and name/value pairs as necessary
    //the order doesn't matter, because this is either an existing autodoc 
    //(so new entries will end up at the bottom), or the comment autodoc (which
    //provides the order) is not usable.
    setAttributeValue(autodoc, FN_VOLUME_KEY, (String) valueMap
        .get(FN_VOLUME_KEY), commentMap);
    setAttributeValue(autodoc, FN_MOD_PARTICLE_KEY, (String) valueMap
        .get(FN_MOD_PARTICLE_KEY), commentMap);
    setAttributeValue(autodoc, INIT_MOTL_KEY, (String) valueMap
        .get(INIT_MOTL_KEY), commentMap);
    setAttributeValue(autodoc, TILT_RANGE_KEY, (String) valueMap
        .get(TILT_RANGE_KEY), commentMap);
    setAttributeValue(autodoc, RELATIVE_ORIENT_KEY, (String) valueMap
        .get(RELATIVE_ORIENT_KEY), commentMap);
  }

  /**
   * Builds an array from an array of ConstEtomoNumber [1 2]
   * @param elementArray
   * @return
   */
  private String buildArray(final ConstEtomoNumber[] elementArray) {
    StringBuffer array = new StringBuffer();
    array.append(OPEN_ARRAY);
    boolean arrayEmpty = true;
    for (int i = 0; i < elementArray.length; i++) {
      if (i > 0) {
        array.append(DIVIDER_STRING);
      }
      if (!elementArray[i].isNull() || elementArray[i].isDefaultSet()) {
        arrayEmpty = false;
        array.append(elementArray[i].toDefaultedString());
      }
    }
    if (arrayEmpty) {
      return OPEN_ARRAY_STRING + CLOSE_ARRAY_STRING;
    }
    array.append(CLOSE_ARRAY);
    return array.toString();
  }

  /**
   * Gets the attribute.  If the attribute doesn't exist, it adds the attribute.
   * Adds or changes the value of the attribute.
   * @param autodoc
   * @param attributeName
   * @param attributeValue
   */
  private void setAttributeValue(WritableAutodoc autodoc, String attributeName,
      String attributeValue, Map commentMap) {
    WritableAttribute attribute = autodoc.getWritableAttribute(attributeName);
    if (attribute == null) {
      if (commentMap == null) {
        //new attribute, so add attribute and name/value pair
        addNameValuePair(autodoc, attributeName, attributeValue, (String) null);
      }
      else {
        //new attribute, so add comment, attribute, and name/value pair
        addNameValuePair(autodoc, attributeName, attributeValue,
            (String) commentMap.get(attributeName));
      }
    }
    else {
      attribute.changeValue(attributeValue);
    }
  }

  /**
   * Add a new name/value pair.  Adds a new-line and comment (if comment is not
   * null), an attribute, and a name/value pair.
   * @param autodoc
   * @param attributeName
   * @param attributeValue
   * @param commentAttribute
   */
  private void addNameValuePair(WritableAutodoc autodoc, String attributeName,
      String attributeValue, ReadOnlyAttribute commentAttribute) {
    if (attributeValue == null) {
      return;
    }
    if (commentAttribute == null) {
      addNameValuePair(autodoc, attributeName, attributeValue, (String) null);
    }
    else {
      addNameValuePair(autodoc, attributeName, attributeValue, commentAttribute
          .getMultiLineValue());
    }
  }

  /**
   * Add a new name/value pair.  Adds a new-line and comment (if comment is not
   * null), an attribute, and a name/value pair.
   * @param autodoc
   * @param attributeName
   * @param attributeValue
   * @param comment
   */
  private void addNameValuePair(WritableAutodoc autodoc, String attributeName,
      String attributeValue, String comment) {
    //try to add a comment
    if (comment != null) {
      //theres a comment, so add an empty line first
      autodoc.addEmptyLine();
      //Format and add the comment
      String[] commentArray = EtomoAutodoc.format(comment);
      for (int i = 0; i < commentArray.length; i++) {
        autodoc.addComment(" " + commentArray[i]);
      }
    }
    //Add the attribute and name/value pair
    autodoc.addAttributeAndNameValuePair(attributeName, attributeValue);
  }

  public int getVolumeListSize() {
    return volumeList.size();
  }

  public void setVolumeListSize(int size) {
    if (volumeList.size() == 0) {
      for (int i = 0; i < size; i++) {
        volumeList.add(new Volume());
      }
    }
  }

  public Volume getVolume(int index) {
    return (Volume) volumeList.get(index);
  }

  public String getFnVolume(int index) {
    return ((Volume) volumeList.get(index)).getFnVolume();
  }

  public String getFnModParticle(int index) {
    return ((Volume) volumeList.get(index)).getFnModParticle();
  }

  public InitMotlCode getInitMotlCode() {
    return initMotlCode;
  }

  public void setInitMotlCode(ConstEtomoNumber code) {
    initMotlCode = InitMotlCode.getInstance(code.toString());
  }

  public boolean isTiltRangeEmpty() {
    return tiltRangeEmpty;
  }

  public void setTiltRangeEmpty(boolean tiltRangeEmpty) {
    this.tiltRangeEmpty = tiltRangeEmpty;
  }

  /**
   * Arrays are surrounded by {}'s.  Array elements are separated by ",".  Find
   * the array in the value parameter and return it.  If there is no array,
   * return the whole value parameter.
   * @param ReadOnlyAttribute
   * @return
   */
  private String[] getList(final ReadOnlyAttribute attribute) {
    if (attribute == null) {
      return new String[0];
    }
    return getList(attribute.getValue());
  }

  /**
   * Lists are surrounded by {}'s.  List elements are separated by ",".  Find
   * the list in the value parameter and return it.  If there is no list, return
   * the whole value parameter.
   * @param String
   * @return String array of elements of list.  Zero length array is there is no attribute, an empty value, or an empty list, "{}".
   */
  private String[] getList(String value) {
    if (value == null) {
      return new String[0];
    }
    value = value.trim();
    if (value.charAt(0) == OPEN_LIST
        && value.charAt(value.length() - 1) == CLOSE_LIST) {
      //remove "{" and "}"
      value = value.substring(1, value.length() - 1).trim();
      if (value.length() == 0) {
        return new String[0];
      }
      //Handle lists of arrays (strips the inner "[" and "]" characters).
      if (value.charAt(0) == OPEN_ARRAY
          && value.charAt(value.length() - 1) == CLOSE_ARRAY) {
        return value.split("\\" + CLOSE_ARRAY_STRING + "\\s*,\\s*\\"
            + OPEN_ARRAY_STRING);
      }
      //split into list
      return value.split("\\s*,\\s*");
    }
    return new String[] { value };
  }

  /**
   * Arrays are surrounded by []'s.  Array elements are separated by whitespace
   * or ",".  Find each element of each array in the list and return them.
   * The arrays have already been stripped of some of the "[" and "]" characaters.
   * @param array
   * @return
   */
  private String[][] getListOfArrays(String[] list) {
    if (list.length == 0) {
      return new String[0][0];
    }
    String[][] listOfArrays = new String[list.length][];
    for (int i = 0; i < list.length; i++) {
      //Assume that this is an array.  Some of the "[" and "]" characters
      //have already been stripped.  Remove any "[" and "]" characters remaining.
      list[i] = list[i].trim();
      //Handle empty array:  "".
      if (list[i].length() == 0) {
        listOfArrays[i] = new String[0];
        continue;
      }
      //Remove "[".
      if (list[i].charAt(0) == OPEN_ARRAY) {
        list[i] = list[i].substring(1);
      }
      //Handle empty array:  "[".
      if (list[i].length() == 0) {
        listOfArrays[i] = new String[0];
        continue;
      }
      //Remove "]".
      if (list[i].charAt(list[i].length() - 1) == CLOSE_ARRAY) {
        list[i] = list[i].substring(0, list[i].length() - 1);
      }
      //Handle empty arrays:  "]" and "[]".
      if (list[i].length() == 0) {
        listOfArrays[i] = new String[0];
        continue;
      }
      //Arrays may be divided by "," or by whitespace only.
      listOfArrays[i] = list[i].split("\\s*[\\s,]\\s*");
    }
    return listOfArrays;
  }

  private String removeQuotes(String string) {
    string = string.trim();
    if (string.length() == 0) {
      return string;
    }
    if (string.charAt(0) == QUOTE
        && string.charAt(string.length() - 1) == QUOTE) {
      return string.substring(1, string.length() - 1);
    }
    return string;
  }

  public static final class InitMotlCode {
    private static final int ZERO_CODE = 0;
    private static final int Z_AXIS_CODE = 1;
    private static final int X_AND_Z_AXIS_CODE = 2;

    public static final InitMotlCode ZERO = new InitMotlCode(ZERO_CODE);
    public static final InitMotlCode Z_AXIS = new InitMotlCode(Z_AXIS_CODE);
    public static final InitMotlCode X_AND_Z_AXIS = new InitMotlCode(
        X_AND_Z_AXIS_CODE);

    private final int code;

    private InitMotlCode(int code) {
      this.code = code;
    }

    private static InitMotlCode getInstance(String code) {
      EtomoNumber enCode = new EtomoNumber();
      enCode.set(code);
      if (enCode == null) {
        return null;
      }
      if (enCode.equals(ZERO_CODE)) {
        return ZERO;
      }
      if (enCode.equals(Z_AXIS_CODE)) {
        return Z_AXIS;
      }
      if (enCode.equals(X_AND_Z_AXIS_CODE)) {
        return X_AND_Z_AXIS;
      }
      return null;
    }

    private static InitMotlCode getInstance(int code) {
      if (code == ZERO_CODE) {
        return ZERO;
      }
      if (code == Z_AXIS_CODE) {
        return Z_AXIS;
      }
      if (code == X_AND_Z_AXIS_CODE) {
        return X_AND_Z_AXIS;
      }
      return null;
    }

    public int getCodeInt() {
      return code;
    }

    public String getCodeString() {
      return new Integer(code).toString();
    }
  }

  public static final class Volume {
    private static final int TILT_RANGE_START_INDEX = 0;
    private static final int TILT_RANGE_END_INDEX = 1;
    private static final int RELATIVE_ORIENT_X_INDEX = 0;
    private static final int RELATIVE_ORIENT_Y_INDEX = 1;
    private static final int RELATIVE_ORIENT_Z_INDEX = 2;
    private String fnVolume = "";
    private String fnModParticle = "";
    private String initMotl = "";
    private final EtomoNumber[] tiltRange = new EtomoNumber[] {
        new EtomoNumber(EtomoNumber.Type.FLOAT),
        new EtomoNumber(EtomoNumber.Type.FLOAT) };
    private final EtomoNumber[] relativeOrient = new EtomoNumber[] {
        new EtomoNumber(EtomoNumber.Type.FLOAT),
        new EtomoNumber(EtomoNumber.Type.FLOAT),
        new EtomoNumber(EtomoNumber.Type.FLOAT) };

    private Volume() {
      //When empty, relativeOrient should use 0.
      relativeOrient[RELATIVE_ORIENT_X_INDEX].setDefault(0);
      relativeOrient[RELATIVE_ORIENT_Y_INDEX].setDefault(0);
      relativeOrient[RELATIVE_ORIENT_Z_INDEX].setDefault(0);
    }

    public void setFnVolume(String fnVolume) {
      this.fnVolume = fnVolume;
    }

    public String getFnVolume() {
      return fnVolume;
    }

    public void setFnModParticle(String fnModParticle) {
      this.fnModParticle = fnModParticle;
    }

    public String getFnModParticle() {
      return fnModParticle;
    }

    public void setInitMotl(String initMotl) {
      this.initMotl = initMotl;
    }

    public String getInitMotl() {
      return initMotl;
    }

    public String getTiltRangeStart() {
      return tiltRange[TILT_RANGE_START_INDEX].toString();
    }

    public void setTiltRangeStart(String tiltRangeStart) {
      tiltRange[TILT_RANGE_START_INDEX].set(tiltRangeStart);
    }

    public String getTiltRangeEnd() {
      return tiltRange[TILT_RANGE_END_INDEX].toString();
    }

    public void setTiltRangeEnd(String tiltRangeEnd) {
      tiltRange[TILT_RANGE_END_INDEX].set(tiltRangeEnd);
    }

    public boolean isRelativeOrientSet() {
      return !relativeOrient[RELATIVE_ORIENT_X_INDEX].isDefault()
          || !relativeOrient[RELATIVE_ORIENT_Y_INDEX].isDefault()
          || !relativeOrient[RELATIVE_ORIENT_Z_INDEX].isDefault();
    }

    public String getRelativeOrientX() {
      return relativeOrient[RELATIVE_ORIENT_X_INDEX].toString();
    }

    public void setRelativeOrientX(String relativeOrientX) {
      relativeOrient[RELATIVE_ORIENT_X_INDEX].set(relativeOrientX);
    }

    public String getRelativeOrientY() {
      return relativeOrient[RELATIVE_ORIENT_Y_INDEX].toString();
    }

    public void setRelativeOrientY(String relativeOrientY) {
      relativeOrient[RELATIVE_ORIENT_Y_INDEX].set(relativeOrientY);
    }

    public String getRelativeOrientZ() {
      return relativeOrient[RELATIVE_ORIENT_Z_INDEX].toString();
    }

    public void setRelativeOrientZ(String relativeOrientZ) {
      relativeOrient[RELATIVE_ORIENT_Z_INDEX].set(relativeOrientZ);
    }

    private ConstEtomoNumber[] getTiltRange() {
      return tiltRange;
    }

    private ConstEtomoNumber[] getRelativeOrient() {
      return relativeOrient;
    }

    private void setTiltRange(String[] tiltRange) {
      for (int i = 0; i < this.tiltRange.length; i++) {
        if (i >= tiltRange.length) {
          return;
        }
        this.tiltRange[i].set(tiltRange[i]);
      }
    }

    private void setRelativeOrient(String[] relativeOrient) {
      for (int i = 0; i < this.relativeOrient.length; i++) {
        if (i >= relativeOrient.length) {
          return;
        }
        this.relativeOrient[i].set(relativeOrient[i]);
      }
    }
  }

  private static final class ParamList {
    private final boolean stringList;
    private final StringBuffer elementBuffer = new StringBuffer();

    private boolean emptyList = true;
    private int currentIndex = 0;

    private ParamList(boolean stringList) {
      this.stringList = stringList;
    }

    private void add(String element) {
      if (currentIndex++ > 0) {
        elementBuffer.append(DIVIDER_STRING);
      }
      if (element.equals("")) {
        return;
      }
      emptyList = false;
      if (stringList) {
        elementBuffer.append(QUOTE);
      }
      elementBuffer.append(element);
      if (stringList) {
        elementBuffer.append(QUOTE);
      }
    }

    /**
     * Prints the required format for the .prm file - do not change
     */
    public String toString() {
      if (emptyList) {
        return OPEN_LIST_STRING + CLOSE_LIST_STRING;
      }
      return OPEN_LIST_STRING + elementBuffer.toString() + CLOSE_LIST_STRING;
    }
  }
}
