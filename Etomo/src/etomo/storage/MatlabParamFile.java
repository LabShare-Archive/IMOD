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
import etomo.storage.autodoc.Statement;
import etomo.storage.autodoc.WritableAttribute;
import etomo.storage.autodoc.WritableAutodoc;
import etomo.storage.autodoc.WritableStatement;
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
 * <p> Revision 1.13  2007/04/09 21:59:33  sueh
 * <p> bug# 964 Added gets and sets and index constants for szVol.
 * <p>
 * <p> Revision 1.12  2007/04/09 20:01:09  sueh
 * <p> bug# 964 Added support for reference.  Reorganized writing functionality.
 * <p>
 * <p> Revision 1.11  2007/04/02 21:41:45  sueh
 * <p> bug# 964 Added INIT_MOTL_DEFAULT.
 * <p>
 * <p> Revision 1.10  2007/04/02 16:01:48  sueh
 * <p> bug# 964 Added defaults and min/max for spinners.
 * <p>
 * <p> Revision 1.9  2007/03/31 02:50:35  sueh
 * <p> bug# 964 Added Default values and CCModeCode.
 * <p>
 * <p> Revision 1.8  2007/03/30 23:39:35  sueh
 * <p> bug# 964 Modified this class to work with ParsedList and ParsedElement.
 * <p>
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
  public static final int REFERENCE_VOLUME_INDEX = 0;
  public static final int REFERENCE_PARTICLE_INDEX = 1;
  public static final String FN_VOLUME_KEY = "fnVolume";
  public static final String FN_MOD_PARTICLE_KEY = "fnModParticle";
  public static final String INIT_MOTL_KEY = "initMOTL";
  public static final InitMotlCode INIT_MOTL_DEFAULT = InitMotlCode.ZERO;
  public static final String TILT_RANGE_KEY = "tiltRange";
  public static final String RELATIVE_ORIENT_KEY = "relativeOrient";
  public static final String SZ_VOL_KEY = "szVol";
  public static final int SZ_VOL_X_INDEX = 0;
  public static final int SZ_VOL_Y_INDEX = 1;
  public static final int SZ_VOL_Z_INDEX = 2;
  public static final String FN_OUTPUT_KEY = "fnOutput";
  public static final String D_PHI_KEY = "dPhi";
  public static final String D_THETA_KEY = "dTheta";
  public static final String D_PSI_KEY = "dPsi";
  public static final String SEARCH_RADIUS_KEY = "searchRadius";
  public static final String LOW_CUTOFF_KEY = "lowCutoff";
  public static final int LOW_CUTOFF_DEFAULT = 0;
  public static final String HI_CUTOFF_KEY = "hiCutoff";
  public static final String CC_MODE_KEY = "CCMode";
  public static final String REF_THRESHOLD_KEY = "refThreshold";
  public static final String REF_FLAG_ALL_TOM_KEY = "refFlagAllTom";
  public static final String EDGE_SHIFT_KEY = "edgeShift";
  public static final int EDGE_SHIFT_DEFAULT = 2;
  public static final String LST_THRESHOLDS_KEY = "lstThresholds";
  public static final String LST_FLAG_ALL_TOM_KEY = "lstFlagAllTom";
  public static final String MEAN_FILL_KEY = "meanFill";
  public static final boolean MEAN_FILL_DEFAULT = true;
  public static final String ALIGNED_BASE_NAME_KEY = "alignedBaseName";
  public static final String DEBUG_LEVEL_KEY = "debugLevel";
  public static final int DEBUG_LEVEL_MIN = 0;
  public static final int DEBUG_LEVEL_MAX = 3;
  public static final int DEBUG_LEVEL_DEFAULT = 3;
  public static final String PARTICLE_PER_CPU_KEY = "particlePerCPU";
  public static final int PARTICLE_PER_CPU_MIN = 1;
  public static final int PARTICLE_PER_CPU_MAX = 20;
  public static final int PARTICLE_PER_CPU_DEFAULT = 5;

  private final ParsedNumber particlePerCpu = new ParsedNumber();
  private final ParsedArray szVol = new ParsedArray();
  private final ParsedQuotedString fnOutput = new ParsedQuotedString();
  private final ParsedNumber refFlagAllTom = new ParsedNumber();
  private final ParsedNumber edgeShift = new ParsedNumber();
  private final ParsedArray lstThresholds = new ParsedArray();
  private final ParsedNumber lstFlagAllTom = new ParsedNumber();
  private final ParsedNumber meanFill = new ParsedNumber();
  private final ParsedQuotedString alignedBaseName = new ParsedQuotedString();
  private final ParsedNumber debugLevel = new ParsedNumber();
  private final List volumeList = new ArrayList();
  private final List iterationList = new ArrayList();
  private final ParsedQuotedString referenceFile = new ParsedQuotedString();
  private final ParsedArray reference = new ParsedArray();
  private final File file;
  private InitMotlCode initMotlCode = null;
  private CCModeCode ccMode = null;
  private boolean tiltRangeEmpty = false;
  private boolean useReferenceFile = false;
  private boolean newFile;

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
      parseData(autodoc);
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
    //Place the string representation of each value in a map.
    //This allows the values to be passed to updateOrBuildAutodoc().
    //When building a new .prm autodoc, this also allows the values to be
    //accessed in the same order as the Field sections in peetprm.adoc.
    Map valueMap = new HashMap();
    buildParsableValues(valueMap);
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
      //the file is written, so it is no longer new
      newFile=false;
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

  public Volume getVolume(final int index) {
    Volume volume;
    if (index == volumeList.size()) {
      volume = new Volume();
      volumeList.add(volume);
      return volume;
    }
    return (Volume) volumeList.get(index);
  }

  public String getFnVolume(final int index) {
    return ((Volume) volumeList.get(index)).getFnVolumeString();
  }

  public String getFnModParticle(final int index) {
    return ((Volume) volumeList.get(index)).getFnModParticleString();
  }

  public InitMotlCode getInitMotlCode() {
    return initMotlCode;
  }

  public void setInitMotlCode(final ConstEtomoNumber code) {
    initMotlCode = InitMotlCode.getInstance(code);
  }

  public void setFnOutput(final String fnOutput) {
    this.fnOutput.setRawString(fnOutput);
  }

  public boolean useTiltRange() {
    return !tiltRangeEmpty;
  }

  public boolean useReferenceFile() {
    return useReferenceFile;
  }

  public void setTiltRangeEmpty(final boolean tiltRangeEmpty) {
    this.tiltRangeEmpty = tiltRangeEmpty;
  }

  public void setReferenceVolume(final Number referenceVolume) {
    useReferenceFile = false;
    reference.setRawNumber(REFERENCE_VOLUME_INDEX, referenceVolume.toString());
  }

  public String getReferenceParticle() {
    return reference.getRawString(REFERENCE_PARTICLE_INDEX);
  }

  public ConstEtomoNumber getReferenceVolume() {
    return reference.getRawNumber(REFERENCE_VOLUME_INDEX);
  }

  public void setEdgeShift(String edgeShift) {
    this.edgeShift.setRawNumber(edgeShift);
  }

  public void clearEdgeShift() {
    edgeShift.clear();
  }

  public String getEdgeShift() {
    return edgeShift.getRawString();
  }

  public void setSzVolX(String szVolX) {
    szVol.setRawNumber(SZ_VOL_X_INDEX, szVolX);
  }

  public void setSzVolY(String szVolY) {
    szVol.setRawNumber(SZ_VOL_Y_INDEX, szVolY);
  }

  public void setSzVolZ(String szVolZ) {
    szVol.setRawNumber(SZ_VOL_Z_INDEX, szVolZ);
  }

  public String getSzVolX() {
    return szVol.getRawString(SZ_VOL_X_INDEX);
  }

  public String getSzVolY() {
    return szVol.getRawString(SZ_VOL_Y_INDEX);
  }

  public String getSzVolZ() {
    return szVol.getRawString(SZ_VOL_Z_INDEX);
  }

  public String getReferenceFile() {
    return referenceFile.getRawString();
  }

  public void setReferenceParticle(final String referenceParticle) {
    useReferenceFile = false;
    reference.setRawNumber(REFERENCE_PARTICLE_INDEX, referenceParticle
        .toString());
  }

  public void setReferenceFile(final String referenceFile) {
    useReferenceFile = true;
    this.referenceFile.setRawString(referenceFile);
  }

  public int getVolumeListSize() {
    return volumeList.size();
  }

  public void setVolumeListSize(final int size) {
    if (volumeList.size() == 0) {
      for (int i = 0; i < size; i++) {
        volumeList.add(new Volume());
      }
    }
  }

  /**
   * Called by read().  Parses data from the the file.
   * @param autodoc
   */
  private void parseData(final ReadOnlyAutodoc autodoc) {
    parseVolumeData(autodoc);
    parseIterationData(autodoc);
    //reference
    ReadOnlyAttribute attribute = autodoc.getAttribute(REFERENCE_KEY);
    if (ParsedQuotedString.isQuotedString(attribute)) {
      useReferenceFile = true;
      referenceFile.parse(attribute);
    }
    else {
      useReferenceFile = false;
      reference.parse(attribute);
    }
    //particlePerCPU
    particlePerCpu.parse(autodoc.getAttribute(PARTICLE_PER_CPU_KEY));
    //szVol
    szVol.parse(autodoc.getAttribute(SZ_VOL_KEY));
    //fnOutput
    fnOutput.parse(autodoc.getAttribute(FN_OUTPUT_KEY));
    //CCMode
    ccMode = CCModeCode.getInstance(autodoc.getAttribute(CC_MODE_KEY));
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

  /**
   * Parses data from the the file.
   * @param autodoc
   */
  private void parseVolumeData(final ReadOnlyAutodoc autodoc) {
    volumeList.clear();
    int size = 0;
    //relativeOrient
    ParsedList relativeOrient = ParsedList.getNumericInstance(autodoc
        .getAttribute(RELATIVE_ORIENT_KEY), EtomoNumber.Type.FLOAT, 0);
    size = Math.max(size, relativeOrient.size());
    //fnVolume
    ParsedList fnVolume = ParsedList.getStringInstance(autodoc
        .getAttribute(FN_VOLUME_KEY));
    size = Math.max(size, fnVolume.size());
    //fnModParticle
    ParsedList fnModParticle = ParsedList.getStringInstance(autodoc
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

  /**
   * Parses data from the the file.
   * @param autodoc
   */
  private void parseIterationData(final ReadOnlyAutodoc autodoc) {
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
   * Called by write().  Places parsable strings (string that will be written to
   * the file) into a Map in preparation for writing.
   * @param valueMap
   */
  private void buildParsableValues(final Map valueMap) {
    buildParsableVolumeValues(valueMap);
    buildParsableIterationValues(valueMap);
    if (useReferenceFile) {
      valueMap.put(REFERENCE_KEY, referenceFile.getParsableString());
    }
    else {
      valueMap.put(REFERENCE_KEY, reference.getParsableString());
    }
    valueMap.put(FN_OUTPUT_KEY, fnOutput.getParsableString());
    //copy szVol X value to Y and Z when Y and/or Z is empty
    ConstEtomoNumber szVolX = szVol.getRawNumber();
    if (!szVolX.isNull()) {
      if (szVol.getRawNumber(SZ_VOL_Y_INDEX).isNull()) {
        szVol.setRawNumber(SZ_VOL_Y_INDEX, szVolX.toString());
      }
      if (szVol.getRawNumber(SZ_VOL_Z_INDEX).isNull()) {
        szVol.setRawNumber(SZ_VOL_Z_INDEX, szVolX.toString());
      }
    }
    valueMap.put(SZ_VOL_KEY, szVol.getParsableString());
    if (!tiltRangeEmpty) {
      valueMap.put(EDGE_SHIFT_KEY, edgeShift.getParsableString());
    }
  }

  /**
   * Places parsable strings (string that will be written to
   * the file) into a Map in preparation for writing.
   * @param valueMap
   */
  private void buildParsableVolumeValues(final Map valueMap) {
    ParsedList fnVolume = ParsedList.getStringInstance();
    ParsedList fnModParticle = ParsedList.getStringInstance();
    ParsedList initMotlFile = null;
    if (initMotlCode == null) {
      initMotlFile = ParsedList.getStringInstance();
    }
    ParsedList tiltRange = ParsedList
        .getNumericInstance(EtomoNumber.Type.FLOAT);
    ParsedList relativeOrient = ParsedList.getNumericInstance(
        EtomoNumber.Type.FLOAT, 0);
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
  }

  /**
   * Places parsable strings (string that will be written to
   * the file) into a Map in preparation for writing.
   * @param valueMap
   */
  private void buildParsableIterationValues(final Map valueMap) {
  }

  /**
   * Called by write().  Updates or adds all the name/value pair to autodoc.
   * Will attempt to add comments when adding a new name/value pair.
   * Adds attributes when it adds a new name/value pair.
   * @param valueMap
   * @param autodoc
   * @param commentAutodoc
   */
  private void updateOrBuildAutodoc(final Map valueMap,
      final WritableAutodoc autodoc, final ReadOnlyAutodoc commentAutodoc) {
    Map commentMap = null;
    if (commentAutodoc != null) {
      commentMap = commentAutodoc.getAttributeMultiLineValues(
          EtomoAutodoc.FIELD_SECTION_NAME, EtomoAutodoc.COMMENT_KEY);
    }
    //write to a autodoc, name/value pairs as necessary
    //the order doesn't matter, because this is either an existing autodoc 
    //(so new entries will end up at the bottom), or the comment autodoc (which
    //provides the order) is not usable.
    setNameValuePairValues(valueMap, autodoc, commentMap);
  }

  /**
   * Adds or changes the value of an name/valueu pair in the file.
   * @param valueMap
   * @param autodoc
   * @param commentMap
   */
  private void setNameValuePairValues(final Map valueMap,
      final WritableAutodoc autodoc, final Map commentMap) {
    setVolumeNameValuePairValues(valueMap, autodoc, commentMap);
    setIterationNameValuePairValues(valueMap, autodoc, commentMap);
    setNameValuePairValue(autodoc, REFERENCE_KEY, (String) valueMap
        .get(REFERENCE_KEY), commentMap);
    setNameValuePairValue(autodoc, FN_OUTPUT_KEY, (String) valueMap
        .get(FN_OUTPUT_KEY), commentMap);
    setNameValuePairValue(autodoc, SZ_VOL_KEY, (String) valueMap
        .get(SZ_VOL_KEY), commentMap);
    if (tiltRangeEmpty) {
      removeNameValuePair(autodoc, EDGE_SHIFT_KEY);
    }
    else {
      setNameValuePairValue(autodoc, EDGE_SHIFT_KEY, (String) valueMap
          .get(EDGE_SHIFT_KEY), commentMap);
    }
  }

  /**
   * Adds or changes the value of an name/value pair in the file.
   * @param valueMap
   * @param autodoc
   * @param commentMap
   */
  private void setVolumeNameValuePairValues(final Map valueMap,
      final WritableAutodoc autodoc, final Map commentMap) {
    setNameValuePairValue(autodoc, FN_VOLUME_KEY, (String) valueMap
        .get(FN_VOLUME_KEY), commentMap);
    setNameValuePairValue(autodoc, FN_MOD_PARTICLE_KEY, (String) valueMap
        .get(FN_MOD_PARTICLE_KEY), commentMap);
    setNameValuePairValue(autodoc, INIT_MOTL_KEY, (String) valueMap
        .get(INIT_MOTL_KEY), commentMap);
    setNameValuePairValue(autodoc, TILT_RANGE_KEY, (String) valueMap
        .get(TILT_RANGE_KEY), commentMap);
    setNameValuePairValue(autodoc, RELATIVE_ORIENT_KEY, (String) valueMap
        .get(RELATIVE_ORIENT_KEY), commentMap);
  }

  /**
   * Adds or changes the value of an name/value pair in the file.
   * @param valueMap
   * @param autodoc
   * @param commentMap
   */
  private void setIterationNameValuePairValues(final Map valueMap,
      final WritableAutodoc autodoc, final Map commentMap) {
  }

  /**
   * Gets the attribute.  If the attribute doesn't exist, it adds the attribute.
   * Adds or changes the value of the attribute.
   * @param autodoc
   * @param attributeName
   * @param attributeValue
   */
  private void setNameValuePairValue(final WritableAutodoc autodoc,
      final String name, final String value, final Map commentMap) {
    WritableAttribute attribute = autodoc.getWritableAttribute(name);
    if (attribute == null) {
      if (commentMap == null) {
        //new attribute, so add attribute and name/value pair
        addNameValuePair(autodoc, name, value, (String) null);
      }
      else {
        //new attribute, so add comment, attribute, and name/value pair
        addNameValuePair(autodoc, name, value, (String) commentMap.get(name));
      }
    }
    else {
      attribute.setValue(value);
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
  private void addNameValuePair(final WritableAutodoc autodoc,
      final String name, final String value,
      final ReadOnlyAttribute commentAttribute) {
    if (value == null) {
      return;
    }
    if (commentAttribute == null) {
      addNameValuePair(autodoc, name, value, (String) null);
    }
    else {
      addNameValuePair(autodoc, name, value, commentAttribute
          .getMultiLineValue());
    }
  }

  private void removeNameValuePair(final WritableAutodoc autodoc,
      final String name) {
    WritableStatement previousStatement = autodoc.removeNameValuePair(name);
    //remove the associated comments
    while (previousStatement != null
        && previousStatement.getType() == Statement.Type.COMMENT) {
      previousStatement = autodoc.removeStatement(previousStatement);
    }
    //remove the associated empty line
    if (previousStatement != null
        && previousStatement.getType() == Statement.Type.EMPTY_LINE) {
      autodoc.removeStatement(previousStatement);
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
  private void addNameValuePair(final WritableAutodoc autodoc,
      final String attributeName, String attributeValue, String comment) {
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
    autodoc.addNameValuePair(attributeName, attributeValue);
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

    private InitMotlCode(final int code) {
      this.code = code;
    }

    public static InitMotlCode getInstance(final ReadOnlyAttribute attribute) {
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

    public static InitMotlCode getInstance(final ConstEtomoNumber code) {
      if (code == null) {
        return null;
      }
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

    private static InitMotlCode getInstance(final int code) {
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

  public static final class CCModeCode {
    private static final int NORMALIZED_CODE = 0;
    private static final int LOCAL_CODE = 1;

    public static final CCModeCode NORMALIZED = new CCModeCode(NORMALIZED_CODE);
    public static final CCModeCode LOCAL = new CCModeCode(LOCAL_CODE);

    private final int code;

    private CCModeCode(final int code) {
      this.code = code;
    }

    public static CCModeCode getInstance(final ReadOnlyAttribute attribute) {
      if (attribute == null) {
        return NORMALIZED;
      }
      String value = attribute.getValue();
      if (value == null) {
        return NORMALIZED;
      }
      EtomoNumber code = new EtomoNumber();
      code.set(value);
      return getInstance(code);
    }

    public static CCModeCode getInstance(final ConstEtomoNumber code) {
      if (code == null) {
        return NORMALIZED;
      }
      if (code.equals(NORMALIZED_CODE)) {
        return NORMALIZED;
      }
      if (code.equals(LOCAL_CODE)) {
        return LOCAL;
      }
      return NORMALIZED;
    }

    public static CCModeCode getDefault() {
      return getInstance((ConstEtomoNumber) null);
    }

    private static CCModeCode getInstance(final int code) {
      if (code == NORMALIZED_CODE) {
        return NORMALIZED;
      }
      if (code == LOCAL_CODE) {
        return LOCAL;
      }
      return NORMALIZED;
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
    private final ParsedArray tiltRange = new ParsedArray(
        EtomoNumber.Type.FLOAT);
    private final ParsedArray relativeOrient = new ParsedArray(
        EtomoNumber.Type.FLOAT, 0);
    private final ParsedQuotedString fnVolume = new ParsedQuotedString();
    private final ParsedQuotedString fnModParticle = new ParsedQuotedString();
    private final ParsedQuotedString initMotl = new ParsedQuotedString();

    public void setFnVolume(final ParsedElement fnVolume) {
      this.fnVolume.setElement(fnVolume);
    }

    public void setFnVolume(final String fnVolume) {
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

    public void setFnModParticle(final ParsedElement fnModParticle) {
      this.fnModParticle.setElement(fnModParticle);
    }

    public void setFnModParticle(final String fnModParticle) {
      this.fnModParticle.setRawString(fnModParticle);
    }

    public void setInitMotl(final ParsedElement initMotl) {
      this.initMotl.setElement(initMotl);
    }

    public void setInitMotl(final String initMotl) {
      this.initMotl.setRawString(initMotl);
    }

    public String getTiltRangeStart() {
      return tiltRange.getRawString(TILT_RANGE_START_INDEX);
    }

    public void setTiltRangeStart(final String tiltRangeStart) {
      tiltRange.setRawNumber(TILT_RANGE_START_INDEX, tiltRangeStart);
    }

    public String getTiltRangeEnd() {
      return tiltRange.getRawString(TILT_RANGE_END_INDEX);
    }

    public void setTiltRangeEnd(String tiltRangeEnd) {
      tiltRange.setRawNumber(TILT_RANGE_END_INDEX, tiltRangeEnd);
    }

    public String getRelativeOrientX() {
      return relativeOrient.getRawString(RELATIVE_ORIENT_X_INDEX);
    }

    public void setRelativeOrientX(final String relativeOrientX) {
      relativeOrient.setRawNumber(RELATIVE_ORIENT_X_INDEX, relativeOrientX);
    }

    public String getRelativeOrientY() {
      return relativeOrient.getRawString(RELATIVE_ORIENT_Y_INDEX);
    }

    public void setRelativeOrientY(final String relativeOrientY) {
      relativeOrient.setRawNumber(RELATIVE_ORIENT_Y_INDEX, relativeOrientY);
    }

    public String getRelativeOrientZ() {
      return relativeOrient.getRawString(RELATIVE_ORIENT_Z_INDEX);
    }

    public void setRelativeOrientZ(final String relativeOrientZ) {
      relativeOrient.setRawNumber(RELATIVE_ORIENT_Z_INDEX, relativeOrientZ);
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

    private void setTiltRange(final ParsedElement tiltRange) {
      this.tiltRange.setElementArray(tiltRange);
    }

    private void setRelativeOrient(final ParsedElement relativeOrient) {
      this.relativeOrient.setElementArray(relativeOrient);
    }
  }

  private static final class Iteration {
    private final ParsedArray dPhi = new ParsedArray(EtomoNumber.Type.FLOAT);
    private final ParsedArray dTheta = new ParsedArray(EtomoNumber.Type.FLOAT);
    private final ParsedArray dPsi = new ParsedArray(EtomoNumber.Type.FLOAT);
    private final ParsedNumber searchRadius = new ParsedNumber();
    private ParsedElement lowCutoff = null;
    private ParsedElement hiCutoff = null;
    private final ParsedNumber refThreshold = new ParsedNumber(
        EtomoNumber.Type.FLOAT);

    private void setDPhi(final ParsedElement dPhi) {
      this.dPhi.setElementArray(dPhi);
    }

    private void setDTheta(final ParsedElement dTheta) {
      this.dTheta.setElementArray(dTheta);
    }

    private void setDPsi(final ParsedElement dPsi) {
      this.dPsi.setElementArray(dPsi);
    }

    private void setSearchRadius(final ParsedElement searchRadius) {
      this.searchRadius.setElement(searchRadius);
    }

    private void setLowCutoff(final ParsedElement lowCutoff) {
      this.lowCutoff = lowCutoff;
    }

    private void setHiCutoff(final ParsedElement hiCutoff) {
      this.hiCutoff = hiCutoff;
    }

    private void setRefThreshold(final ParsedElement refThreshold) {
      this.refThreshold.setElement(refThreshold);
    }
  }
}
