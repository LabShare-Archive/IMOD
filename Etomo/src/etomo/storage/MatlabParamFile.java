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
import etomo.type.ParsedArray;
import etomo.type.ParsedElement;
import etomo.type.ParsedList;
import etomo.type.ParsedNumber;
import etomo.type.ParsedQuotedString;
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
 * <p> Revision 1.7  2007/03/26 23:32:26  sueh
 * <p> bug# 964 Made keys public.
 * <p>
 * <p> Revision 1.6  2007/03/26 18:35:50  sueh
 * <p> bug# 964 Prevented MatlabParamFile from loading a .prm file unless the user asks
 * <p> for the file to be read.  Fixed the parsing of lists of arrays.
 * <p>
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

  public static final String REFERENCE_KEY = "reference";
  public static final String FN_VOLUME_KEY = "fnVolume";
  public static final String FN_MOD_PARTICLE_KEY = "fnModParticle";
  public static final String INIT_MOTL_KEY = "initMOTL";
  public static final String TILT_RANGE_KEY = "tiltRange";
  public static final String RELATIVE_ORIENT_KEY = "relativeOrient";
  public static final String SZ_VOL_KEY="szVol";
  public static final String FN_OUTPUT_KEY="fnOutput";
  public static final String D_PHI_KEY="dPhi";
  public static final String D_THETA_KEY="dTheta";
  public static final String D_PSI_KEY="dPsi";
  public static final String SEARCH_RADIUS_KEY="searchRadius";
  public static final String LOW_CUTOFF_KEY="lowCutoff";
  public static final String HI_CUTOFF_KEY="hiCutoff";
  public static final String CC_MODE_KEY="CCMode";
  public static final String REF_THRESHOLD_KEY="refThreshold";
  public static final String REF_FLAG_ALL_TOM_KEY="refFlagAllTom";
  public static final String EDGE_SHIFT_KEY="edgeShift";
  public static final String LST_THRESHOLDS_KEY="lstThresholds";
  public static final String LST_FLAG_ALL_TOM_KEY="lstFlagAllTom";
  public static final String MEAN_FILL_KEY="meanFill";
  public static final String ALIGNED_BASE_NAME_KEY="alignedBaseName";
  public static final String DEBUG_LEVEL_KEY="debugLevel";
  public static final String PARTICLE_PER_CPU_KEY="particlePerCPU";

  private final ParsedNumber particlePerCpu = new ParsedNumber();
  private final ParsedArray szVol = new ParsedArray();
  private final ParsedQuotedString fnOutput = new ParsedQuotedString();
  private final ParsedNumber ccMode = new ParsedNumber();
  private final ParsedNumber refFlagAllTom = new ParsedNumber();
  private final ParsedNumber edgeShift = new ParsedNumber();
  private final ParsedArray lstThresholds = new ParsedArray();
  private final ParsedNumber lstFlagAllTom = new ParsedNumber();
  private final ParsedNumber meanFill = new ParsedNumber();
  private final ParsedQuotedString alignedBaseName = new ParsedQuotedString();
  private final ParsedNumber debugLevel = new ParsedNumber();
  private final List volumeList = new ArrayList();
  private final List iterationList = new ArrayList();
  private final File file;
  private final boolean newFile;
  private InitMotlCode initMotlCode = null;
  private boolean tiltRangeEmpty = false;
  private ParsedQuotedString referenceFile = null;
  private ParsedArray reference = null;

  public MatlabParamFile(File file, boolean newFile) {
    this.file = file;
    this.newFile = newFile;
  }

  /**
   * Reads data from the .prm autodoc.
   */
  public synchronized void read() {
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
      //parse lists
      parseVolumeLists(autodoc);
      parseIterationLists(autodoc);
      //Parse other values
      //reference
      ReadOnlyAttribute attribute = autodoc.getAttribute(REFERENCE_KEY);
      if (ParsedQuotedString.isQuotedString(attribute)) {
        referenceFile = ParsedQuotedString.getInstance(attribute);
      }
      else {
        reference = ParsedArray.getInstance(attribute);
      }
      //particlePerCPU
      particlePerCpu.parse(autodoc.getAttribute(PARTICLE_PER_CPU_KEY));
      //szVol
      szVol.parse(autodoc.getAttribute(SZ_VOL_KEY));
      //fnOutput
      fnOutput.parse(autodoc.getAttribute(FN_OUTPUT_KEY));
      //CCMode
      ccMode.parse(autodoc.getAttribute(CC_MODE_KEY));
      //refFlagAllTom
      refFlagAllTom.parse(autodoc.getAttribute(REF_FLAG_ALL_TOM_KEY));
      //edgeShift
      edgeShift.parse(autodoc.getAttribute(EDGE_SHIFT_KEY));
      //lstThresholds
      lstThresholds.parse(autodoc.getAttribute(LST_THRESHOLDS_KEY));
      //lstFlagAllTom
      lstFlagAllTom.parse(autodoc.getAttribute(LST_FLAG_ALL_TOM_KEY));
      //meanFill
      meanFill.parse(autodoc.getAttribute(MEAN_FILL_KEY));
      //alignedBaseName
      alignedBaseName.parse(autodoc.getAttribute(ALIGNED_BASE_NAME_KEY));
      //debugLevel
      debugLevel.parse(autodoc.getAttribute(DEBUG_LEVEL_KEY));
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
    //create ParsedList instances to build the values to be written.
    ParsedList fnVolume = ParsedList.getStringInstance();
    ParsedList fnModParticle = ParsedList.getStringInstance();
    ParsedList initMotlFile = null;
    if (initMotlCode == null) {
      initMotlFile = ParsedList.getStringInstance();
    }
    ParsedList tiltRange = ParsedList.getNumericInstance(EtomoNumber.Type.FLOAT);
    ParsedList relativeOrient = ParsedList.getNumericInstance(EtomoNumber.Type.FLOAT,0);
    //build the values
    for (int i = 0; i < volumeList.size(); i++) {
      Volume volume = (Volume) volumeList.get(i);
      fnVolume.addElement(volume.getFnVolume());
      fnModParticle.addElement(volume.getFnModParticle());
      if (initMotlFile != null) {
        initMotlFile.addElement(volume.getInitMotl());
      }
      if (!tiltRangeEmpty) {
        tiltRange.addElement(volume.getTiltRange());
      }
      relativeOrient.addElement(volume.getRelativeOrient());
    }
    //Place the string representation of each value in a map.
    //This allows the values to be passed to updateOrBuildAutodoc().
    //When building a new .prm autodoc, this also allows the values to be
    //accessed in the same order as the Field sections in peetprm.adoc.
    Map valueMap = new HashMap();
    valueMap.put(FN_VOLUME_KEY, fnVolume.getParsableString());
    valueMap.put(FN_MOD_PARTICLE_KEY, fnModParticle.getParsableString());
    if (initMotlFile != null) {
      valueMap.put(INIT_MOTL_KEY, initMotlFile.getParsableString());
    }
    else {
      valueMap.put(INIT_MOTL_KEY, initMotlCode.toString());
    }
    valueMap.put(TILT_RANGE_KEY, tiltRange.getParsableString());
    valueMap.put(RELATIVE_ORIENT_KEY, relativeOrient.getParsableString());
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
  
  private void parseVolumeLists(ReadOnlyAutodoc autodoc) {
    volumeList.clear();
    int size = 0;
    //relativeOrient
    ParsedList relativeOrient = ParsedList.getNumericInstance(autodoc
        .getAttribute(RELATIVE_ORIENT_KEY), EtomoNumber.Type.FLOAT,0);
    size = Math.max(size, relativeOrient.size());
    //fnVolume
    ParsedList fnVolume = ParsedList.getStringInstance(autodoc
        .getAttribute(FN_VOLUME_KEY));
    size = Math.max(size, fnVolume.size());
    //fnModParticle
    ParsedList fnModParticle = ParsedList
        .getStringInstance(autodoc
            .getAttribute(FN_MOD_PARTICLE_KEY));
    size = Math.max(size, fnModParticle.size());
    //initMOTL
    ParsedList initMotlFile = null;
    ReadOnlyAttribute attribute = autodoc.getAttribute(INIT_MOTL_KEY);
    if (ParsedList.isList(attribute)) {
      initMotlFile = ParsedList.getStringInstance(attribute);
      size = Math.max(size, initMotlFile.size());
    }
    else {
      initMotlCode = InitMotlCode.getInstance(attribute);
    }
    //tiltRange
    ParsedList tiltRange = ParsedList.getNumericInstance(autodoc
        .getAttribute(TILT_RANGE_KEY), EtomoNumber.Type.FLOAT);
    size = Math.max(size, tiltRange.size());
    if (tiltRange.size() == 0) {
      tiltRangeEmpty = true;
    }
    //Add elements to volumeList
    for (int i = 0; i < size; i++) {
      Volume volume = new Volume();
      volume.setRelativeOrient(relativeOrient.getElement(i));
      volume.setFnVolume(fnVolume.getElement(i));
      volume.setFnModParticle(fnModParticle.getElement(i));
      if (initMotlCode == null) {
        volume.setInitMotl(initMotlFile.getElement(i));
      }
      if (!tiltRangeEmpty) {
        volume.setTiltRange(tiltRange.getElement(i));
      }
      volumeList.add(volume);
    }
  }
  
  private void parseIterationLists(ReadOnlyAutodoc autodoc) {
    iterationList.clear();
    int size = 0;
    //dPhi
    ParsedList dPhi = ParsedList.getNumericInstance(autodoc
        .getAttribute(D_PHI_KEY), EtomoNumber.Type.FLOAT);
    size = Math.max(size, dPhi.size());
    //dTheta
    ParsedList dTheta = ParsedList.getNumericInstance(autodoc
        .getAttribute(D_THETA_KEY), EtomoNumber.Type.FLOAT);
    size = Math.max(size, dTheta.size());
    //dPsi
    ParsedList dPsi = ParsedList.getNumericInstance(autodoc
        .getAttribute(D_PSI_KEY), EtomoNumber.Type.FLOAT);
    size = Math.max(size, dPsi.size());
    //searchRadius
    ParsedList searchRadius = ParsedList.getNumericInstance(autodoc
        .getAttribute(SEARCH_RADIUS_KEY));
    size = Math.max(size, searchRadius.size());
    //lowCutoff
    ParsedList lowCutoff = ParsedList.getNumericInstance(autodoc
        .getAttribute(LOW_CUTOFF_KEY), EtomoNumber.Type.FLOAT);
    size = Math.max(size, lowCutoff.size());
    //hiCutoff
    ParsedList hiCutoff = ParsedList.getNumericInstance(autodoc
        .getAttribute(HI_CUTOFF_KEY), EtomoNumber.Type.FLOAT);
    size = Math.max(size, hiCutoff.size());
    //refThreshold
    ParsedList refThreshold = ParsedList.getNumericInstance(autodoc
        .getAttribute(REF_THRESHOLD_KEY), EtomoNumber.Type.FLOAT);
    size = Math.max(size, refThreshold.size());
    //add elements to iterationList
    for (int j = 0; j < size; j++) {
      Iteration iteration = new Iteration();
      iteration.setDPhi(dPhi.getElement(j));
      iteration.setDTheta(dTheta.getElement(j));
      iteration.setDPsi(dPsi.getElement(j));
      iteration.setSearchRadius(searchRadius.getElement(j));
      iteration.setLowCutoff(lowCutoff.getElement(j));
      iteration.setHiCutoff(hiCutoff.getElement(j));
      iteration.setRefThreshold(refThreshold.getElement(j));
      iterationList.add(iteration);
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
    return ((Volume) volumeList.get(index)).getFnVolumeString();
  }

  public String getFnModParticle(int index) {
    return ((Volume) volumeList.get(index)).getFnModParticleString();
  }

  public InitMotlCode getInitMotlCode() {
    return initMotlCode;
  }

  public void setInitMotlCode(ConstEtomoNumber code) {
    initMotlCode = InitMotlCode.getInstance(code);
  }

  public boolean isTiltRangeEmpty() {
    return tiltRangeEmpty;
  }

  public void setTiltRangeEmpty(boolean tiltRangeEmpty) {
    this.tiltRangeEmpty = tiltRangeEmpty;
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
    
    public static InitMotlCode getInstance(ReadOnlyAttribute attribute) {
      if (attribute == null) {
        return null;
      }
      String value = attribute.getValue();
      if (value == null) {
        return null;
      }
      EtomoNumber code = new EtomoNumber();
      code.set(value);
      return getInstance(code);
    }
    
    public static InitMotlCode getInstance(ConstEtomoNumber code) {
      if (code.equals(ZERO_CODE)) {
        return ZERO;
      }
      if (code.equals(Z_AXIS_CODE)) {
        return Z_AXIS;
      }
      if (code.equals(X_AND_Z_AXIS_CODE)) {
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

    public int intValue() {
      return code;
    }

    public String toString() {
      return new Integer(code).toString();
    }
  }

  public static final class Volume {
    private static final int TILT_RANGE_START_INDEX = 0;
    private static final int TILT_RANGE_END_INDEX = 1;
    private static final int RELATIVE_ORIENT_X_INDEX = 0;
    private static final int RELATIVE_ORIENT_Y_INDEX = 1;
    private static final int RELATIVE_ORIENT_Z_INDEX = 2;
    private final ParsedArray tiltRange = new ParsedArray(EtomoNumber.Type.FLOAT);
    private final ParsedArray relativeOrient = new ParsedArray(EtomoNumber.Type.FLOAT,0);
    private final ParsedQuotedString fnVolume = new ParsedQuotedString();
    private final ParsedQuotedString fnModParticle = new ParsedQuotedString();
    private final ParsedQuotedString initMotl = new ParsedQuotedString();

    public void setFnVolume(ParsedElement fnVolume) {
      this.fnVolume.setElement(fnVolume);
    }
    
    public void setFnVolume(String fnVolume) {
      this.fnVolume.setRawString(fnVolume);
    }

    public String getFnVolumeString() {
      return fnVolume.getRawString();
    }
    
    public String getFnModParticleString() {
      return fnModParticle.getRawString();
    }
    
    public String getInitMotlString() {
      return initMotl.getRawString();
    }

    public void setFnModParticle(ParsedElement fnModParticle) {
      this.fnModParticle.setElement(fnModParticle);
    }
    
    public void setFnModParticle(String fnModParticle) {
      this.fnModParticle.setRawString(fnModParticle);
    }

    public void setInitMotl(ParsedElement initMotl) {
      this.initMotl.setElement(initMotl);
    }
    
    public void setInitMotl(String initMotl) {
      this.initMotl.setRawString(initMotl);
    }

    public String getTiltRangeStart() {
      return tiltRange.getRawString(TILT_RANGE_START_INDEX);
    }

    public void setTiltRangeStart(String tiltRangeStart) {
      tiltRange.setRawNumber(TILT_RANGE_START_INDEX,tiltRangeStart);
    }

    public String getTiltRangeEnd() {
      return tiltRange.getRawString(TILT_RANGE_END_INDEX);
    }

    public void setTiltRangeEnd(String tiltRangeEnd) {
      tiltRange.setRawNumber(TILT_RANGE_END_INDEX,tiltRangeEnd);
    }

    public boolean isRelativeOrientSet() {
      return !relativeOrient.isEmpty();
    }

    public String getRelativeOrientX() {
      return relativeOrient.getRawString(RELATIVE_ORIENT_X_INDEX);
    }

    public void setRelativeOrientX(String relativeOrientX) {
      relativeOrient.setRawNumber(RELATIVE_ORIENT_X_INDEX,relativeOrientX);
    }

    public String getRelativeOrientY() {
      return relativeOrient.getRawString(RELATIVE_ORIENT_Y_INDEX);
    }

    public void setRelativeOrientY(String relativeOrientY) {
      relativeOrient.setRawNumber(RELATIVE_ORIENT_Y_INDEX,relativeOrientY);
    }

    public String getRelativeOrientZ() {
      return relativeOrient.getRawString(RELATIVE_ORIENT_Z_INDEX);
    }

    public void setRelativeOrientZ(String relativeOrientZ) {
      relativeOrient.setRawNumber(RELATIVE_ORIENT_Z_INDEX,relativeOrientZ);
    }
    
    private ParsedQuotedString getFnVolume() {
      return fnVolume;
    }
    
    private ParsedQuotedString getFnModParticle() {
      return fnModParticle;
    }
    
    private ParsedQuotedString getInitMotl() {
      return initMotl;
    }

    private ParsedArray getTiltRange() {
      return tiltRange;
    }

    private ParsedArray getRelativeOrient() {
      return relativeOrient;
    }

    private void setTiltRange(ParsedElement tiltRange) {
      this.tiltRange.setElementArray(tiltRange);
    }

    private void setRelativeOrient(ParsedElement relativeOrient) {
      this.relativeOrient.setElementArray(relativeOrient);
    }
  }
  
  private static final class Iteration {
    private void setDPhi(ParsedElement dPhi) {}
    private void setDTheta(ParsedElement dTheta) {}
    private void setDPsi(ParsedElement dPsi) {}
    private void setSearchRadius(ParsedElement searchRadius) {}
    private void setLowCutoff(ParsedElement lowCutoff) {}
    private void setHiCutoff(ParsedElement hiCutoff) {}
    private void setRefThreshold(ParsedElement refThreshold) {}
  }
}
