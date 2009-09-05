package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import etomo.ManagerKey;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.SectionLocation;
import etomo.storage.autodoc.Statement;
import etomo.storage.autodoc.WritableAttribute;
import etomo.storage.autodoc.WritableAutodoc;
import etomo.storage.autodoc.WritableStatement;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EnumeratedType;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.ParsedArray;
import etomo.type.ParsedArrayDescriptor;
import etomo.type.ParsedElement;
import etomo.type.ParsedList;
import etomo.type.ParsedNumber;
import etomo.type.ParsedQuotedString;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;

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
 * <p> Revision 1.29  2009/09/01 03:18:06  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.28  2009/04/02 21:47:55  sueh
 * <p> bug# 1203 In SearchAngleArea.setIncrement(String), corrected function so that it
 * <p> passes the modified value to descriptor.
 * <p>
 * <p> Revision 1.27  2009/03/17 00:45:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.26  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.25  2009/01/13 19:35:55  sueh
 * <p> bug# 1170 Changed N_WEIGHT_GROUP_DEFAULT to 8.  Added
 * <p> N_WEIGHT_GROUP_MIN.  Setting the floor of nWeightGroup to
 * <p> N_WEIGHT_GROUP_MIN for backwards compatibility, since the previous
 * <p> version have a min of 0, which causes the spinner to lock up.  Added
 * <p> isNWeightGroupEmpty.
 * <p>
 * <p> Revision 1.24  2008/10/10 20:42:48  sueh
 * <p> bug# 1142 Clear tiltRange when tiltRange check box is unchecked.
 * <p>
 * <p> Revision 1.23  2008/09/10 20:55:24  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.  Make
 * <p> the indexes more standard.  Handle the non-standard qualities of Phi, etc
 * <p> in MatlabParam so that ParsedArrayDescriptor can be simpler.  Handle
 * <p> tiltRange with a tiltRangeEmpty boolean.
 * <p>
 * <p> Revision 1.22  2008/09/05 20:52:37  sueh
 * <p> bug# 1136 Added useNWeightGroup to distinguish between an nWeightGroup
 * <p> with the spinner set to 0 and an nWeightGroup which is disabled.  Calling
 * <p> updateOrBuildAutodoc from write in third place because it has to be able
 * <p> to remove entries.  Updating setNameValuePairValues - it was missing some
 * <p> recent additions.
 * <p>
 * <p> Revision 1.21  2008/08/22 17:50:35  sueh
 * <p> bug# 1136 Added nWeightGroup.
 * <p>
 * <p> Revision 1.20  2008/08/21 00:01:58  sueh
 * <p> bug# 1135 Started to add SearchAngleArea - not in use yet.
 * <p>
 * <p> Revision 1.19  2008/06/20 18:54:23  sueh
 * <p> bug# 1119 ParsedArrayDescriptor can be either Matlab or non-Matlab now, so I need to tell the constructor the ParsedElementType.
 * <p>
 * <p> Revision 1.18  2008/05/30 21:20:30  sueh
 * <p> bug# 1102 Formatted.
 * <p>
 * <p> Revision 1.17  2008/04/15 21:00:23  sueh
 * <p> bug# 1105 Simplified setting the default.
 * <p>
 * <p> Revision 1.16  2008/04/02 01:54:12  sueh
 * <p> bug# 1095 Added mask fields.  Bug# 1097 In ParsedElement classes,
 * <p> matching Matlab's syntax.
 * <p>
 * <p> Revision 1.15  2008/03/06 00:25:08  sueh
 * <p> bug# 1088 Added sampleSphere and sampleInterval.
 * <p>
 * <p> Revision 1.14  2008/01/31 20:22:34  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.13  2007/12/17 21:04:58  sueh
 * <p> bug# 1060 Synchronizing meanFill with flgMeanFill in the .prm file.
 * <p>
 * <p> Revision 1.12  2007/12/14 21:46:05  sueh
 * <p> bug# 1060 Changed meanFill to flgMeanFill, keeping meanFill for backwards
 * <p> compatibility.
 * <p>
 * <p> Revision 1.11  2007/11/06 19:31:06  sueh
 * <p> bug# 1047 Calling the Parsed arrays where the descriptor is turned into an array
 * <p> "expanded arrays".
 * <p>
 * <p> Revision 1.10  2007/07/24 04:02:36  sueh
 * <p> bug# 1030 Changed ParsedArray.getParsableStringArray to
 * <p> getPaddedStringArray.  The function is only being used by lstThresholds and it
 * <p> needs padded strings.
 * <p>
 * <p> Revision 1.9  2007/06/08 22:14:58  sueh
 * <p> bug# 1014 Return false if read() failed.
 * <p>
 * <p> Revision 1.8  2007/06/07 21:30:25  sueh
 * <p> bug# 1012 In write(), if the autodoc exists read it and return it.  Moved the
 * <p> file back up out of Autodoc.write() into MatlabParam.write().  Changed the
 * <p> addNameValuePair functions to setNameValuePair.  In setNameValuePair,
 * <p> if the attribute already exists, just set its value.
 * <p>
 * <p> Revision 1.7  2007/06/05 21:28:10  sueh
 * <p> bug# 1010 Added flgWedgeWeight.
 * <p>
 * <p> Revision 1.6  2007/05/16 22:58:44  sueh
 * <p> bug# 964 Changed setFileDir to setFile.
 * <p>
 * <p> Revision 1.5  2007/05/16 01:45:37  sueh
 * <p> bug# 964 In setvolumeListSize(int size), set volumeList size to size.
 * <p>
 * <p> Revision 1.4  2007/05/15 19:56:08  sueh
 * <p> bug# 964 In setIterationListSize(int size) removing Iteration objects if the size of
 * <p> iterationList is greater then size.
 * <p>
 * <p> Revision 1.3  2007/05/11 15:47:35  sueh
 * <p> bug# 964 Added getLstThresholdsArray() to return the expanded
 * <p> lstThresholds array (with array descriptors converted into arrays) in a
 * <p> String array.
 * <p>
 * <p> Revision 1.2  2007/05/08 01:19:26  sueh
 * <p> bug# 964 In addNameValuePair(), adding the field name to the beginning
 * <p> of the comment.
 * <p>
 * <p> Revision 1.1  2007/05/07 17:20:27  sueh
 * <p> bug# 964 Changed MatlabParamFile to MatlabParam.
 * <p>
 * <p> Revision 1.22  2007/05/03 21:06:58  sueh
 * <p> bug# 964 Allow searchRadius to be an array.  Allow hiCutoff to display as
 * <p> a number when sigma is empty; this required hiCutoff to be compact as
 * <p> well as flexible because sigma is always set by the GUI.
 * <p>
 * <p> Revision 1.21  2007/05/02 16:34:38  sueh
 * <p> bug# 964 Default reference source not being set.
 * <p>
 * <p> Revision 1.20  2007/05/01 22:26:00  sueh
 * <p> bug# 964 Added yaxisType and yaxisContour.
 * <p>
 * <p> Revision 1.19  2007/04/26 02:45:23  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.18  2007/04/20 20:51:30  sueh
 * <p> bug# 964 Added support for refFlagAllTom, lstFlagAllTom, ParticlePerCpu.
 * <p>
 * <p> Revision 1.17  2007/04/19 21:37:05  sueh
 * <p> bug# 964 Added support for lstThresholds.  Added clear().  Simplified read so that
 * <p> is doesn't parse an empty autodoc.  Simplified write so that it doesn't read the
 * <p> existing file before writing.
 * <p>
 * <p> Revision 1.16  2007/04/13 21:47:11  sueh
 * <p> bug# 964 Not returning ConstEtomoNumber from ParsedElement, because it
 * <p> must be returned with a getDefaulted... function to be accurate.
 * <p> GetReferenceVolume is returning ParsedElement instead.
 * <p>
 * <p> Revision 1.15  2007/04/13 18:41:59  sueh
 * <p> bug# 964 Writing ccMode, meanFill, and lowCutoff
 * <p>
 * <p> Revision 1.14  2007/04/11 21:43:07  sueh
 * <p> bug# 964 Moved logic for szVol (copy X to Y and Z when Y and/or Z don't have
 * <p> values) from PeetDialog to MatlabParamFile.  Added removeNameValuePair to
 * <p> remove edgeShift from the file when tiltRange is not in use.
 * <p>
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
 * <p> bug# 964 Added the ability to write the autodoc based on the order of FieldInterface sections in another autodoc.  Also has the ability to write the autodoc without
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
public final class MatlabParam {
  public static final String rcsid = "$Id$";

  public static final String REFERENCE_KEY = "reference";
  public static final String FN_VOLUME_KEY = "fnVolume";
  public static final String FN_MOD_PARTICLE_KEY = "fnModParticle";
  public static final String INIT_MOTL_KEY = "initMOTL";
  public static final String TILT_RANGE_KEY = "tiltRange";
  public static final String RELATIVE_ORIENT_KEY = "relativeOrient";
  public static final String SZ_VOL_KEY = "szVol";
  public static final int X_INDEX = 0;
  public static final int Y_INDEX = 1;
  public static final int Z_INDEX = 2;
  public static final String FN_OUTPUT_KEY = "fnOutput";
  public static final String D_PHI_KEY = "dPhi";
  public static final String D_THETA_KEY = "dTheta";
  public static final String D_PSI_KEY = "dPsi";
  public static final String SEARCH_RADIUS_KEY = "searchRadius";
  public static final String LOW_CUTOFF_KEY = "lowCutoff";
  public static final String LOW_CUTOFF_DEFAULT = "0";
  public static final String HI_CUTOFF_KEY = "hiCutoff";
  public static final String CC_MODE_KEY = "CCMode";
  public static final String REF_THRESHOLD_KEY = "refThreshold";
  public static final String REF_FLAG_ALL_TOM_KEY = "refFlagAllTom";
  public static final String EDGE_SHIFT_KEY = "edgeShift";
  public static final int EDGE_SHIFT_DEFAULT = 2;
  public static final String LST_THRESHOLDS_KEY = "lstThresholds";
  public static final String LST_FLAG_ALL_TOM_KEY = "lstFlagAllTom";
  /**
   * @deprecated
   * Replaced by FLG_MEAN_FILL_KEY
   */
  public static final String MEAN_FILL_KEY = "meanFill";
  public static final String FLG_MEAN_FILL_KEY = "flgMeanFill";
  public static final boolean FLG_MEAN_FILL_DEFAULT = true;
  public static final String ALIGNED_BASE_NAME_KEY = "alignedBaseName";
  public static final String DEBUG_LEVEL_KEY = "debugLevel";
  public static final int DEBUG_LEVEL_MIN = 0;
  public static final int DEBUG_LEVEL_MAX = 3;
  public static final int DEBUG_LEVEL_DEFAULT = 3;
  public static final String PARTICLE_PER_CPU_KEY = "particlePerCPU";
  public static final int PARTICLE_PER_CPU_MIN = 1;
  public static final int PARTICLE_PER_CPU_MAX = 50;
  public static final int PARTICLE_PER_CPU_DEFAULT = 5;
  public static final String YAXIS_TYPE_KEY = "yaxisType";
  public static final String YAXIS_CONTOUR_KEY = "yaxisContour";
  public static final boolean REFERENCE_FILE_DEFAULT = false;
  public static final String FLG_WEDGE_WEIGHT_KEY = "flgWedgeWeight";
  public static final boolean FLG_WEDGE_WEIGHT_DEFAULT = false;
  public static final String SAMPLE_SPHERE_KEY = "sampleSphere";
  public static final String SAMPLE_INTERVAL_KEY = "sampleInterval";
  public static final String MASK_TYPE_KEY = "maskType";
  public static final String MASK_MODEL_PTS_KEY = "maskModelPts";
  public static final String INSIDE_MASK_RADIUS_KEY = "insideMaskRadius";
  public static final String OUTSIDE_MASK_RADIUS_KEY = "outsideMaskRadius";
  public static final String N_WEIGHT_GROUP_KEY = "nWeightGroup";
  public static final int N_WEIGHT_GROUP_DEFAULT = 8;
  public static final int N_WEIGHT_GROUP_MIN = 2;

  private static final int VOLUME_INDEX = 0;
  private static final int PARTICLE_INDEX = 1;
  private static final int MODEL_INDEX = 0;
  private static final int OBJECT_INDEX = 1;
  private static final int CONTOUR_INDEX = 2;
  private static final int RELATIVE_ORIENT_DEFAULT = 0;

  private final ParsedNumber particlePerCpu = ParsedNumber.getMatlabInstance();
  private final ParsedArray szVol = ParsedArray.getMatlabInstance();
  private final ParsedQuotedString fnOutput = ParsedQuotedString.getInstance();
  private final ParsedNumber refFlagAllTom = ParsedNumber.getMatlabInstance();
  private final ParsedNumber edgeShift = ParsedNumber.getMatlabInstance();
  private final ParsedArray lstThresholds = ParsedArray.getMatlabInstance();
  private final ParsedNumber lstFlagAllTom = ParsedNumber.getMatlabInstance();
  /**
   * @deprecated
   * Replaced by flgMeanFill.  Keep meanFill synchronized with flgMeanFill to
   * help with the transition to flgMeanFill.
   */
  private final ParsedNumber meanFill = ParsedNumber.getMatlabInstance();
  private final ParsedNumber flgMeanFill = ParsedNumber.getMatlabInstance();
  private final ParsedQuotedString alignedBaseName = ParsedQuotedString
      .getInstance();
  private final ParsedNumber debugLevel = ParsedNumber.getMatlabInstance();
  private final List volumeList = new ArrayList();
  private final List iterationList = new ArrayList();
  private final ParsedQuotedString referenceFile = ParsedQuotedString
      .getInstance();
  private final ParsedArray reference = ParsedArray.getMatlabInstance();
  private final ParsedArray yAxisContour = ParsedArray.getMatlabInstance();
  private final ParsedNumber flgWedgeWeight = ParsedNumber.getMatlabInstance();
  private final ParsedQuotedString sampleSphere = ParsedQuotedString
      .getInstance();
  private final ParsedNumber sampleInterval = ParsedNumber.getMatlabInstance();
  private final ParsedQuotedString maskType = ParsedQuotedString.getInstance();
  private final ParsedArray maskModelPts = ParsedArray.getMatlabInstance();
  private final ParsedNumber insideMaskRadius = ParsedNumber
      .getMatlabInstance();
  private final ParsedNumber outsideMaskRadius = ParsedNumber
      .getMatlabInstance();
  private final ParsedNumber nWeightGroup = ParsedNumber.getMatlabInstance();

  private final ManagerKey managerKey;

  private String lowCutoff = LOW_CUTOFF_DEFAULT;
  private InitMotlCode initMotlCode = InitMotlCode.DEFAULT;
  private CCMode ccMode = CCMode.DEFAULT;
  private boolean useReferenceFile = REFERENCE_FILE_DEFAULT;
  private YAxisType yAxisType = YAxisType.DEFAULT;
  private boolean useYaxisContour = false;
  private boolean useNWeightGroup = false;
  private boolean tiltRangeEmpty = false;

  private boolean newFile;
  private File file;

  public MatlabParam(File file, boolean newFile, ManagerKey managerKey) {
    this.managerKey = managerKey;
    this.file = file;
    this.newFile = newFile;
    nWeightGroup.setDefault(N_WEIGHT_GROUP_DEFAULT);
    nWeightGroup.setFloor(N_WEIGHT_GROUP_MIN);
  }

  /**
   * Change file to newDir + fnOutput + .prm.  Also sets newFile to true.  This allows
   * MatlabParam to read from one file and then write to another.
   * @param newDir
   */
  public void setFile(String newDir) {
    newFile = true;
    file = new File(newDir, fnOutput.getRawString()
        + DatasetFiles.MATLAB_PARAM_FILE_EXT);
  }

  /**
   * Reads data from the .prm autodoc.
   */
  public synchronized boolean read() {
    clear();
    //if newFile is on, either there is no file, or the user doesn't want to read it
    if (newFile) {
      return true;
    }
    try {
      ReadOnlyAutodoc autodoc = null;
      autodoc = (AutodocFactory.getMatlabInstance(file, managerKey));
      if (autodoc == null) {
        UIHarness.INSTANCE.openMessageDialog("Unable to read "
            + file.getAbsolutePath() + ".", "File Error", managerKey);
        return false;
      }
      parseData(autodoc);
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to load "
          + file.getAbsolutePath() + ".  IOException:  " + e.getMessage(),
          "File Error", managerKey);
      return false;
    }
    catch (LogFile.LockException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to read "
          + file.getAbsolutePath() + ".  LogFile.ReadException:  "
          + e.getMessage(), "File Error", managerKey);
      return false;
    }
    return true;
  }

  /**
   * Write stored data to the .prm autodoc.
   */
  public synchronized void write() {
    //Place the string representation of each value in a map.
    //This allows the values to be passed to updateOrBuildAutodoc().
    //When building a new .prm autodoc, this also allows the values to be
    //accessed in the same order as the FieldInterface sections in peetprm.adoc.
    Map valueMap = new HashMap();
    buildParsableValues(valueMap);
    //try to get the peetprm.adoc, which contains the comments for the .prm file
    //in its FieldInterface sections.
    ReadOnlyAutodoc commentAutodoc = null;
    try {
      commentAutodoc = AutodocFactory.getInstance(AutodocFactory.PEET_PRM,
          AxisID.ONLY, managerKey);
    }
    catch (IOException e) {
      System.err.println("Problem with " + AutodocFactory.PEET_PRM
          + ".adoc.\nIOException:  " + e.getMessage());
    }
    catch (LogFile.LockException e) {
      System.err.println("Problem with " + AutodocFactory.PEET_PRM
          + ".adoc.\nLogFile.ReadException:  " + e.getMessage());
    }
    try {
      WritableAutodoc autodoc = AutodocFactory.getMatlabInstance(file,
          managerKey);
      if (autodoc == null) {
        //get an empty .prm autodoc if the file doesn't exist
        autodoc = AutodocFactory.getEmptyMatlabInstance(file, managerKey);
      }
      else {
        LogFile logFile = LogFile.getInstance(file, managerKey);
        logFile.backup();
      }
      if (commentAutodoc == null) {
        //The peetprm.adoc is not available.
        //Build a new .prm autodoc with no comments
        updateOrBuildAutodoc(valueMap, autodoc, null);
      }
      else {
        //Get the FieldInterface sections from the peetprm.adoc
        SectionLocation secLoc = commentAutodoc
            .getSectionLocation(EtomoAutodoc.FIELD_SECTION_NAME);
        if (secLoc == null) {
          //There are no FieldInterface sections in the peetprm.adoc.
          //Build a new .prm autodoc with no comments
          updateOrBuildAutodoc(valueMap, autodoc, null);
        }
        else {
          //Build a new .prm autodoc.  Use the FieldInterface sections from the
          //peetprm.adoc to dictate the order of the name/value pairs.
          //Also use the comments from the peetprm.adoc FieldInterface sections.
          //This makes MatlabParam dependent on peetprm.adoc so peetprm.adoc
          //must be the responsibility of the Etomo developer.
          updateOrBuildAutodoc(valueMap, autodoc, commentAutodoc);
          /*ReadOnlySection section = null;
           System.out.println("setNameValuePair");
           while ((section = commentAutodoc.nextSection(secLoc)) != null) {
           setNameValuePair(autodoc, section.getName(), (String) valueMap
           .get(section.getName()), section
           .getAttribute(EtomoAutodoc.COMMENT_KEY));
           }*/
        }
      }
      //write the autodoc file (the backup is done by autodoc)
      autodoc.write();
      //the file is written, so it is no longer new
      newFile = false;
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to load " + file.getName()
          + ".  IOException:  " + e.getMessage(), "File Error", managerKey);
    }
    catch (LogFile.LockException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to read " + file.getName()
          + ".  LogFile.ReadException:  " + e.getMessage(), "File Error",
          managerKey);
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

  public Iteration getIteration(final int index) {
    Iteration iteration;
    if (index == iterationList.size()) {
      iteration = new Iteration();
      iteration.setLowCutoff(lowCutoff);
      iterationList.add(iteration);
      return iteration;
    }
    return (Iteration) iterationList.get(index);
  }

  public String getFnOutput() {
    return fnOutput.getRawString();
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

  public CCMode getCcMode() {
    return ccMode;
  }

  public YAxisType getYAxisType() {
    return yAxisType;
  }

  public void setInitMotlCode(EnumeratedType enumeratedType) {
    initMotlCode = (InitMotlCode) enumeratedType;
  }

  public void setCcMode(EnumeratedType enumeratedType) {
    ccMode = (CCMode) enumeratedType;
  }

  public void setYaxisType(EnumeratedType enumeratedType) {
    yAxisType = (YAxisType) enumeratedType;
    useYaxisContour = yAxisType == YAxisType.CONTOUR;
  }

  public void setSampleSphere(EnumeratedType enumeratedType) {
    this.sampleSphere.setRawString(((SampleSphere) enumeratedType).toString());
  }

  public void setMaskType(EnumeratedType enumeratedType) {
    this.maskType.setRawString(((MaskType) enumeratedType).toString());
  }

  public void setFnOutput(final String fnOutput) {
    this.fnOutput.setRawString(fnOutput);
  }

  public boolean useReferenceFile() {
    return useReferenceFile;
  }

  /**
   * Sets flgMeanFill and meanFill.
   * @param flgMeanFill
   */
  public void setFlgMeanFill(final boolean flgMeanFill) {
    this.flgMeanFill.setRawString(flgMeanFill);
    meanFill.setRawString(flgMeanFill);
  }

  public void setRefFlagAllTom(final boolean input) {
    refFlagAllTom.setRawString(input);
  }

  public void setLstFlagAllTom(final boolean input) {
    lstFlagAllTom.setRawString(input);
  }

  public void setFlgWedgeWeight(final boolean input) {
    flgWedgeWeight.setRawString(input);
  }

  public boolean isFlgMeanFill() {
    return flgMeanFill.getRawBoolean();
  }

  public boolean isMaskModelPtsEmpty() {
    return maskModelPts.isEmpty();
  }

  /**
   * If the tilt range check box is uncheck, then tilt range should be {}.
   */
  public void setTiltRangeEmpty() {
    tiltRangeEmpty = true;
  }

  /**
   * This is just for backwards compatibility in setting the tilt range check
   * box.
   * @return
   */
  public boolean isTiltRangeEmpty() {
    if (volumeList.isEmpty()) {
      return true;
    }
    for (int i = 0; i < volumeList.size(); i++) {
      if (!((Volume) volumeList.get(i)).isTiltRangeEmpty()) {
        return false;
      }
    }
    return true;
  }

  public boolean isRefFlagAllTom() {
    return refFlagAllTom.getRawBoolean();
  }

  public boolean isLstFlagAllTom() {
    return lstFlagAllTom.getRawBoolean();
  }

  public boolean isFlgWedgeWeight() {
    return flgWedgeWeight.getRawBoolean();
  }

  public String getAlignedBaseName() {
    return alignedBaseName.getRawString();
  }

  public void setAlignedBaseName(String alignedBaseName) {
    this.alignedBaseName.setRawString(alignedBaseName);
  }

  public void setReferenceVolume(final Number referenceVolume) {
    useReferenceFile = false;
    reference.setRawString(VOLUME_INDEX, referenceVolume.toString());
  }

  public void setNWeightGroup(final Number input) {
    nWeightGroup.setRawString(input);
  }

  public void setUseNWeightGroup(final boolean input) {
    useNWeightGroup = input;
  }

  public void setMaskModelPtsVolume(final Number input) {
    maskModelPts.setRawString(VOLUME_INDEX, input.toString());
  }

  public void setYAxisContourModelNumber(final Number input) {
    yAxisContour.setRawString(MODEL_INDEX, input.toString());
  }

  public String getMaskModelPtsParticle() {
    return maskModelPts.getRawString(PARTICLE_INDEX);
  }

  public ParsedElement getMaskModelPtsVolume() {
    return maskModelPts.getElement(VOLUME_INDEX);
  }

  public String getReferenceParticle() {
    return reference.getRawString(PARTICLE_INDEX);
  }

  public ParsedElement getReferenceVolume() {
    return reference.getElement(VOLUME_INDEX);
  }

  public ParsedElement getYAxisContourModelNumber() {
    return yAxisContour.getElement(MODEL_INDEX);
  }

  public String getYAxisContourObjectNumber() {
    return yAxisContour.getRawString(OBJECT_INDEX);
  }

  public String getYAxisContourContourNumber() {
    return yAxisContour.getRawString(CONTOUR_INDEX);
  }

  public SampleSphere getSampleSphere() {
    return SampleSphere.getInstance(sampleSphere.getRawString());
  }

  public String getMaskType() {
    return maskType.getRawString();
  }

  public void setEdgeShift(final String edgeShift) {
    this.edgeShift.setRawString(edgeShift);
  }

  public void clear() {
    particlePerCpu.clear();
    szVol.clear();
    fnOutput.clear();
    refFlagAllTom.clear();
    edgeShift.clear();
    lstThresholds.clear();
    lstFlagAllTom.clear();
    flgMeanFill.clear();
    meanFill.clear();
    alignedBaseName.clear();
    debugLevel.clear();
    volumeList.clear();
    iterationList.clear();
    referenceFile.clear();
    reference.clear();
    lowCutoff = LOW_CUTOFF_DEFAULT;
    initMotlCode = InitMotlCode.DEFAULT;
    ccMode = CCMode.DEFAULT;
    useReferenceFile = false;
    yAxisType = YAxisType.DEFAULT;
    useYaxisContour = false;
    yAxisContour.clear();
    flgWedgeWeight.setRawString(FLG_WEDGE_WEIGHT_DEFAULT);
    sampleInterval.clear();
    sampleSphere.clear();
    maskType.clear();
    maskModelPts.clear();
    insideMaskRadius.clear();
    outsideMaskRadius.clear();
    nWeightGroup.clear();
    useNWeightGroup = false;
    tiltRangeEmpty = false;
  }

  public void clearEdgeShift() {
    edgeShift.clear();
  }

  public String getEdgeShift() {
    return edgeShift.getRawString();
  }

  public String getSampleInterval() {
    return sampleInterval.getRawString();
  }

  public String getInsideMaskRadius() {
    return insideMaskRadius.getRawString();
  }

  public ParsedElement getNWeightGroup() {
    return nWeightGroup;
  }

  public boolean isNWeightGroupEmpty() {
    return nWeightGroup.isEmpty();
  }

  public String getOutsideMaskRadius() {
    return outsideMaskRadius.getRawString();
  }

  public File getFile() {
    return file;
  }

  public void setSampleInterval(final String input) {
    sampleInterval.setRawString(input);
  }

  public void setInsideMaskRadius(final String input) {
    insideMaskRadius.setRawString(input);
  }

  public void setOutsideMaskRadius(final String input) {
    outsideMaskRadius.setRawString(input);
  }

  public void setSzVolX(final String szVolX) {
    szVol.setRawString(X_INDEX, szVolX);
  }

  public void setSzVolY(final String szVolY) {
    szVol.setRawString(Y_INDEX, szVolY);
  }

  public void setSzVolZ(final String szVolZ) {
    szVol.setRawString(Z_INDEX, szVolZ);
  }

  /**
   * LowCutoff is an iteration value, but it is only set once, so get the value
   * at the first index
   * @return
   */
  public String getLowCutoff() {
    if (iterationList.size() == 0) {
      return lowCutoff;
    }
    return ((Iteration) iterationList.get(0)).getLowCutoffString();
  }

  /**
   * LowCutoff is only set once, so it is placed in all the Iteration instances
   * If the Iteration instances haven't been created yet, it should be add to them
   * from lowCutoff when they are.
   * @param input
   */
  public void setLowCutoff(final String input) {
    lowCutoff = input;
    for (int i = 0; i < iterationList.size(); i++) {
      ((Iteration) iterationList.get(i)).setLowCutoff(lowCutoff);
    }
  }

  public void setDebugLevel(final Number input) {
    debugLevel.setRawString(input.toString());
  }

  public void setParticlePerCPU(final Number input) {
    particlePerCpu.setRawString(input.toString());
  }

  public Number getDebugLevel() {
    return debugLevel.getRawNumber();
  }

  public Number getParticlePerCPU() {
    return particlePerCpu.getRawNumber();
  }

  public String getSzVolX() {
    return szVol.getRawString(X_INDEX);
  }

  public String getSzVolY() {
    return szVol.getRawString(Y_INDEX);
  }

  public String getSzVolZ() {
    return szVol.getRawString(Z_INDEX);
  }

  public String getReferenceFile() {
    return referenceFile.getRawString();
  }

  public void setReferenceParticle(final String referenceParticle) {
    useReferenceFile = false;
    reference.setRawString(PARTICLE_INDEX, referenceParticle);
  }

  public void setMaskModelPtsParticle(final String input) {
    maskModelPts.setRawString(PARTICLE_INDEX, input);
  }

  public void clearMaskModelPts() {
    maskModelPts.clear();
  }

  public void setYaxisContourObjectNumber(final String input) {
    yAxisContour.setRawString(OBJECT_INDEX, input);
  }

  public void setYaxisContourContourNumber(final String input) {
    yAxisContour.setRawString(CONTOUR_INDEX, input);
  }

  public void setReferenceFile(final String referenceFile) {
    useReferenceFile = true;
    this.referenceFile.setRawString(referenceFile);
  }

  public void setMaskType(final String input) {
    maskType.setRawString(input);
  }

  public int getVolumeListSize() {
    return volumeList.size();
  }

  public int getIterationListSize() {
    return iterationList.size();
  }

  public void setVolumeListSize(final int size) {
    //if volume list is too small, add new Volumes
    for (int i = volumeList.size(); i < size; i++) {
      volumeList.add(new Volume());
    }
    //if volume list is too big, remove Volumes from the end
    for (int i = size; i < volumeList.size(); i++) {
      volumeList.remove(i);
    }
  }

  public void setIterationListSize(final int size) {
    //if iteration list is too small, add new Iterations
    for (int i = iterationList.size(); i < size; i++) {
      Iteration iteration = new Iteration();
      iteration.setLowCutoff(lowCutoff);
      iterationList.add(iteration);
    }
    //if iteration list is too big, remove Iterations from the end
    for (int i = size; i < iterationList.size(); i++) {
      iterationList.remove(i);
    }
  }

  public void setLstThresholdsStart(String input) {
    lstThresholds.setRawStringStart(input);
  }

  public void setLstThresholdsIncrement(String input) {
    lstThresholds.setRawStringIncrement(input);
  }

  public void setLstThresholdsEnd(String input) {
    lstThresholds.setRawStringEnd(input);
  }

  public void setLstThresholdsAdditional(String input) {
    lstThresholds.setRawStrings(input);
  }

  public String getLstThresholdsStart() {
    return lstThresholds.getRawStringStart();
  }

  public String getLstThresholdsEnd() {
    return lstThresholds.getRawStringEnd();
  }

  public String getLstThresholdsIncrement() {
    return lstThresholds.getRawStringIncrement();
  }

  public String[] getLstThresholdsExpandedArray() {
    return lstThresholds.getPaddedStringExpandedArray();
  }

  /**
   * Find all the numbers after the descriptor and return their values
   * @return
   */
  public String getLstThresholdsAdditional() {
    return lstThresholds.getRawStringsExceptFirstArrayDescriptor();
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
    ccMode = CCMode.getInstance(autodoc.getAttribute(CC_MODE_KEY));
    //refFlagAllTom
    refFlagAllTom.parse(autodoc.getAttribute(REF_FLAG_ALL_TOM_KEY));
    //edgeShift
    edgeShift.parse(autodoc.getAttribute(EDGE_SHIFT_KEY));
    //lstThresholds
    lstThresholds.parse(autodoc.getAttribute(LST_THRESHOLDS_KEY));
    //lstFlagAllTom
    lstFlagAllTom.parse(autodoc.getAttribute(LST_FLAG_ALL_TOM_KEY));
    //flgMeanFill
    flgMeanFill.parse(autodoc.getAttribute(FLG_MEAN_FILL_KEY));
    //backwards compatibility
    if (flgMeanFill.isEmpty()) {
      flgMeanFill.parse(autodoc.getAttribute(MEAN_FILL_KEY));
    }
    //alignedBaseName
    alignedBaseName.parse(autodoc.getAttribute(ALIGNED_BASE_NAME_KEY));
    //debugLevel
    debugLevel.parse(autodoc.getAttribute(DEBUG_LEVEL_KEY));
    //YaxisType
    yAxisType = YAxisType.getInstance(autodoc.getAttribute(YAXIS_TYPE_KEY));
    useYaxisContour = yAxisType == YAxisType.CONTOUR;
    //YaxisContour
    yAxisContour.parse(autodoc.getAttribute(YAXIS_CONTOUR_KEY));
    //flgWedgeWeight
    flgWedgeWeight.parse(autodoc.getAttribute(FLG_WEDGE_WEIGHT_KEY));
    //sampleSphere
    sampleSphere.parse(autodoc.getAttribute(SAMPLE_SPHERE_KEY));
    //sampleInterval
    sampleInterval.parse(autodoc.getAttribute(SAMPLE_INTERVAL_KEY));
    //maskType
    maskType.parse(autodoc.getAttribute(MASK_TYPE_KEY));
    //maskModelPts
    maskModelPts.parse(autodoc.getAttribute(MASK_MODEL_PTS_KEY));
    //insideMaskRadius
    insideMaskRadius.parse(autodoc.getAttribute(INSIDE_MASK_RADIUS_KEY));
    //outsideMaskRadius
    outsideMaskRadius.parse(autodoc.getAttribute(OUTSIDE_MASK_RADIUS_KEY));
    //nWeightGroup
    nWeightGroup.parse(autodoc.getAttribute(N_WEIGHT_GROUP_KEY));
  }

  /**
   * Parses data from the the file.
   * @param autodoc
   */
  private void parseVolumeData(final ReadOnlyAutodoc autodoc) {
    volumeList.clear();
    int size = 0;
    //relativeOrient
    ParsedList relativeOrient = ParsedList
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    relativeOrient.setDefault(RELATIVE_ORIENT_DEFAULT);
    relativeOrient.parse(autodoc.getAttribute(RELATIVE_ORIENT_KEY));
    size = Math.max(size, relativeOrient.size());
    //fnVolume
    ParsedList fnVolume = ParsedList.getStringInstance();
    fnVolume.parse(autodoc.getAttribute(FN_VOLUME_KEY));
    size = Math.max(size, fnVolume.size());
    //fnModParticle
    ParsedList fnModParticle = ParsedList.getStringInstance();
    fnModParticle.parse(autodoc.getAttribute(FN_MOD_PARTICLE_KEY));
    size = Math.max(size, fnModParticle.size());
    //initMOTL
    ParsedList initMotlFile = null;
    ReadOnlyAttribute attribute = autodoc.getAttribute(INIT_MOTL_KEY);
    if (ParsedList.isList(attribute)) {
      initMotlCode = null;
      initMotlFile = ParsedList.getStringInstance();
      initMotlFile.parse(attribute);
      size = Math.max(size, initMotlFile.size());
    }
    else {
      initMotlCode = InitMotlCode.getInstance(attribute);
    }
    //tiltRange
    ParsedList tiltRange = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    tiltRange.parse(autodoc.getAttribute(TILT_RANGE_KEY));
    size = Math.max(size, tiltRange.size());
    //Add elements to volumeList
    for (int i = 0; i < size; i++) {
      Volume volume = new Volume();
      volume.setRelativeOrient(relativeOrient.getElement(i));
      volume.setFnVolume(fnVolume.getElement(i));
      volume.setFnModParticle(fnModParticle.getElement(i));
      if (initMotlCode == null) {
        volume.setInitMotl(initMotlFile.getElement(i));
      }
      volume.setTiltRange(tiltRange.getElement(i));
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
    ParsedList dPhi = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    dPhi.parse(autodoc.getAttribute(D_PHI_KEY));
    size = Math.max(size, dPhi.size());
    //dTheta
    ParsedList dTheta = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    dTheta.parse(autodoc.getAttribute(D_THETA_KEY));
    size = Math.max(size, dTheta.size());
    //dPsi
    ParsedList dPsi = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    dPsi.parse(autodoc.getAttribute(D_PSI_KEY));
    size = Math.max(size, dPsi.size());
    //searchRadius
    ParsedList searchRadius = ParsedList.getMatlabInstance();
    searchRadius.parse(autodoc.getAttribute(SEARCH_RADIUS_KEY));
    size = Math.max(size, searchRadius.size());
    //lowCutoff
    ParsedList lowCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    lowCutoff.parse(autodoc.getAttribute(LOW_CUTOFF_KEY));
    //hiCutoff
    ParsedList hiCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    hiCutoff.parse(autodoc.getAttribute(HI_CUTOFF_KEY));
    size = Math.max(size, hiCutoff.size());
    //refThreshold
    ParsedList refThreshold = ParsedList
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    refThreshold.parse(autodoc.getAttribute(REF_THRESHOLD_KEY));
    size = Math.max(size, refThreshold.size());
    //add elements to iterationList
    for (int i = 0; i < size; i++) {
      Iteration iteration = new Iteration();
      iteration.setDPhi(dPhi.getElement(i));
      iteration.setDTheta(dTheta.getElement(i));
      iteration.setDPsi(dPsi.getElement(i));
      iteration.setSearchRadius(searchRadius.getElement(i));
      iteration.setLowCutoff(lowCutoff.getElement(i));
      iteration.setHiCutoff(hiCutoff.getElement(i));
      iteration.setRefThreshold(refThreshold.getElement(i));
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
    ParsedElement szVolX = szVol.getElement(X_INDEX);
    if (szVolX != null && !szVolX.isEmpty()) {
      if (szVol.isEmpty(Y_INDEX)) {
        szVol.setRawString(Y_INDEX, szVolX.getRawString());
      }
      if (szVol.isEmpty(Z_INDEX)) {
        szVol.setRawString(Z_INDEX, szVolX.getRawString());
      }
    }
    valueMap.put(SZ_VOL_KEY, szVol.getParsableString());
    if (!isTiltRangeEmpty()) {
      valueMap.put(EDGE_SHIFT_KEY, edgeShift.getParsableString());
    }
    valueMap.put(CC_MODE_KEY, ccMode.toString());
    if (initMotlCode != null) {
      valueMap.put(INIT_MOTL_KEY, initMotlCode.toString());
    }
    valueMap.put(MEAN_FILL_KEY, meanFill.getParsableString());
    valueMap.put(FLG_MEAN_FILL_KEY, flgMeanFill.getParsableString());
    valueMap.put(ALIGNED_BASE_NAME_KEY, alignedBaseName.getParsableString());
    valueMap.put(DEBUG_LEVEL_KEY, debugLevel.getParsableString());
    valueMap.put(LST_THRESHOLDS_KEY, lstThresholds.getParsableString());
    valueMap.put(REF_FLAG_ALL_TOM_KEY, refFlagAllTom.getParsableString());
    valueMap.put(LST_FLAG_ALL_TOM_KEY, lstFlagAllTom.getParsableString());
    valueMap.put(PARTICLE_PER_CPU_KEY, particlePerCpu.getParsableString());
    valueMap.put(YAXIS_TYPE_KEY, yAxisType.toString());
    if (useYaxisContour) {
      valueMap.put(YAXIS_CONTOUR_KEY, yAxisContour.getParsableString());
    }
    valueMap.put(FLG_WEDGE_WEIGHT_KEY, flgWedgeWeight.getParsableString());
    valueMap.put(SAMPLE_SPHERE_KEY, sampleSphere.getParsableString());
    valueMap.put(SAMPLE_INTERVAL_KEY, sampleInterval.getParsableString());
    valueMap.put(MASK_TYPE_KEY, maskType.getParsableString());
    valueMap.put(MASK_MODEL_PTS_KEY, maskModelPts.getParsableString());
    valueMap.put(INSIDE_MASK_RADIUS_KEY, insideMaskRadius.getParsableString());
    valueMap
        .put(OUTSIDE_MASK_RADIUS_KEY, outsideMaskRadius.getParsableString());
    if (useNWeightGroup) {
      valueMap.put(N_WEIGHT_GROUP_KEY, nWeightGroup.getParsableString());
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
    ParsedList tiltRange = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    ParsedList relativeOrient = ParsedList
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    relativeOrient.setDefault(RELATIVE_ORIENT_DEFAULT);
    //build the lists
    for (int i = 0; i < volumeList.size(); i++) {
      Volume volume = (Volume) volumeList.get(i);
      fnVolume.addElement(volume.getFnVolume());
      fnModParticle.addElement(volume.getFnModParticle());
      if (initMotlCode == null) {
        initMotlFile.addElement(volume.getInitMotl());
      }
      tiltRange.addElement(volume.getTiltRange());
      relativeOrient.addElement(volume.getRelativeOrient());
    }
    valueMap.put(FN_VOLUME_KEY, fnVolume.getParsableString());
    valueMap.put(FN_MOD_PARTICLE_KEY, fnModParticle.getParsableString());
    if (initMotlCode == null) {
      valueMap.put(INIT_MOTL_KEY, initMotlFile.getParsableString());
    }
    if (tiltRangeEmpty) {
      tiltRange.clear();
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
    ParsedList dPhi = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    ParsedList dTheta = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    ParsedList dPsi = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    ParsedList searchRadius = ParsedList.getMatlabInstance();
    ParsedList lowCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    ParsedList hiCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.FLOAT);
    ParsedList refThreshold = ParsedList
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    //build the lists
    for (int i = 0; i < iterationList.size(); i++) {
      Iteration iteration = (Iteration) iterationList.get(i);
      dPhi.addElement(iteration.getDPhi());
      dTheta.addElement(iteration.getDTheta());
      dPsi.addElement(iteration.getDPsi());
      searchRadius.addElement(iteration.getSearchRadius());
      lowCutoff.addElement(iteration.getLowCutoff());
      hiCutoff.addElement(iteration.getHiCutoff());
      refThreshold.addElement(iteration.getRefThreshold());
    }
    valueMap.put(D_PHI_KEY, dPhi.getParsableString());
    valueMap.put(D_THETA_KEY, dTheta.getParsableString());
    valueMap.put(D_PSI_KEY, dPsi.getParsableString());
    valueMap.put(SEARCH_RADIUS_KEY, searchRadius.getParsableString());
    valueMap.put(LOW_CUTOFF_KEY, lowCutoff.getParsableString());
    valueMap.put(HI_CUTOFF_KEY, hiCutoff.getParsableString());
    valueMap.put(REF_THRESHOLD_KEY, refThreshold.getParsableString());
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
   * Adds or changes the value of an name/value pair in the file.
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
    if (isTiltRangeEmpty()) {
      removeNameValuePair(autodoc, EDGE_SHIFT_KEY);
    }
    else {
      setNameValuePairValue(autodoc, EDGE_SHIFT_KEY, (String) valueMap
          .get(EDGE_SHIFT_KEY), commentMap);
    }
    setNameValuePairValue(autodoc, CC_MODE_KEY, (String) valueMap
        .get(CC_MODE_KEY), commentMap);
    setNameValuePairValue(autodoc, MEAN_FILL_KEY, (String) valueMap
        .get(MEAN_FILL_KEY), commentMap);
    setNameValuePairValue(autodoc, FLG_MEAN_FILL_KEY, (String) valueMap
        .get(FLG_MEAN_FILL_KEY), commentMap);
    setNameValuePairValue(autodoc, ALIGNED_BASE_NAME_KEY, (String) valueMap
        .get(ALIGNED_BASE_NAME_KEY), commentMap);
    setNameValuePairValue(autodoc, DEBUG_LEVEL_KEY, (String) valueMap
        .get(DEBUG_LEVEL_KEY), commentMap);
    setNameValuePairValue(autodoc, LST_THRESHOLDS_KEY, (String) valueMap
        .get(LST_THRESHOLDS_KEY), commentMap);
    setNameValuePairValue(autodoc, REF_FLAG_ALL_TOM_KEY, (String) valueMap
        .get(REF_FLAG_ALL_TOM_KEY), commentMap);
    setNameValuePairValue(autodoc, LST_FLAG_ALL_TOM_KEY, (String) valueMap
        .get(LST_FLAG_ALL_TOM_KEY), commentMap);
    setNameValuePairValue(autodoc, PARTICLE_PER_CPU_KEY, (String) valueMap
        .get(PARTICLE_PER_CPU_KEY), commentMap);
    setNameValuePairValue(autodoc, YAXIS_TYPE_KEY, (String) valueMap
        .get(YAXIS_TYPE_KEY), commentMap);
    setNameValuePairValue(autodoc, YAXIS_CONTOUR_KEY, (String) valueMap
        .get(YAXIS_CONTOUR_KEY), commentMap);
    setNameValuePairValue(autodoc, FLG_WEDGE_WEIGHT_KEY, (String) valueMap
        .get(FLG_WEDGE_WEIGHT_KEY), commentMap);
    setNameValuePairValue(autodoc, SAMPLE_SPHERE_KEY, (String) valueMap
        .get(SAMPLE_SPHERE_KEY), commentMap);
    setNameValuePairValue(autodoc, SAMPLE_INTERVAL_KEY, (String) valueMap
        .get(SAMPLE_INTERVAL_KEY), commentMap);
    setNameValuePairValue(autodoc, MASK_TYPE_KEY, (String) valueMap
        .get(MASK_TYPE_KEY), commentMap);
    setNameValuePairValue(autodoc, MASK_MODEL_PTS_KEY, (String) valueMap
        .get(MASK_MODEL_PTS_KEY), commentMap);
    setNameValuePairValue(autodoc, INSIDE_MASK_RADIUS_KEY, (String) valueMap
        .get(INSIDE_MASK_RADIUS_KEY), commentMap);
    setNameValuePairValue(autodoc, OUTSIDE_MASK_RADIUS_KEY, (String) valueMap
        .get(OUTSIDE_MASK_RADIUS_KEY), commentMap);
    if (useNWeightGroup) {
      setNameValuePairValue(autodoc, N_WEIGHT_GROUP_KEY, (String) valueMap
          .get(N_WEIGHT_GROUP_KEY), commentMap);
    }
    else {
      removeNameValuePair(autodoc, N_WEIGHT_GROUP_KEY);
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
    setNameValuePairValue(autodoc, D_PHI_KEY, (String) valueMap.get(D_PHI_KEY),
        commentMap);
    setNameValuePairValue(autodoc, D_THETA_KEY, (String) valueMap
        .get(D_THETA_KEY), commentMap);
    setNameValuePairValue(autodoc, D_PSI_KEY, (String) valueMap.get(D_PSI_KEY),
        commentMap);
    setNameValuePairValue(autodoc, SEARCH_RADIUS_KEY, (String) valueMap
        .get(SEARCH_RADIUS_KEY), commentMap);
    setNameValuePairValue(autodoc, LOW_CUTOFF_KEY, (String) valueMap
        .get(LOW_CUTOFF_KEY), commentMap);
    setNameValuePairValue(autodoc, HI_CUTOFF_KEY, (String) valueMap
        .get(HI_CUTOFF_KEY), commentMap);
    setNameValuePairValue(autodoc, REF_THRESHOLD_KEY, (String) valueMap
        .get(REF_THRESHOLD_KEY), commentMap);
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
    if (value == null) {
      return;
    }
    WritableAttribute attribute = autodoc.getWritableAttribute(name);
    if (attribute == null) {
      if (commentMap == null) {
        //new attribute, so add attribute and name/value pair
        setNameValuePair(autodoc, name, value, (String) null);
      }
      else {
        //new attribute, so add comment, attribute, and name/value pair
        setNameValuePair(autodoc, name, value, (String) commentMap.get(name));
      }
    }
    else {
      attribute.setValue(value);
    }
  }

  /**
   * Adds or updates a name/value pair.  If adding, also trys to add a new-line
   * and comment.
   * @param autodoc
   * @param attributeName
   * @param attributeValue
   * @param commentAttribute
   */
  private void setNameValuePair(final WritableAutodoc autodoc,
      final String name, final String value,
      final ReadOnlyAttribute commentAttribute) {
    if (value == null) {
      return;
    }
    if (commentAttribute == null) {
      setNameValuePair(autodoc, name, value, (String) null);
    }
    else {
      setNameValuePair(autodoc, name, value, commentAttribute
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
   * Adds or updates a name/value pair.  If adding, also trys to add a new-line
   * and a comment.
   * @param autodoc
   * @param attributeName
   * @param attributeValue
   * @param comment
   */
  private void setNameValuePair(final WritableAutodoc autodoc,
      final String attributeName, String attributeValue, String comment) {
    WritableAttribute attribute = autodoc.getWritableAttribute(attributeName);
    if (attribute == null) {
      //If the attribute doesn't exist try to add a comment and add the attribute
      if (comment != null) {
        //there's a comment, so add an empty line first
        autodoc.addEmptyLine();
        //Format and add the comment
        String[] commentArray = EtomoAutodoc.format(attributeName + ":\n"
            + comment);
        for (int i = 0; i < commentArray.length; i++) {
          autodoc.addComment(" " + commentArray[i]);
        }
      }
      //Add the attribute and name/value pair
      autodoc.addNameValuePair(attributeName, attributeValue);
    }
    else {
      //If atttribute does exist, change its value
      attribute.setValue(attributeValue);
    }
  }

  public static final class InitMotlCode implements EnumeratedType {
    public static final InitMotlCode ZERO = new InitMotlCode(0);
    public static final InitMotlCode Z_AXIS = new InitMotlCode(1);
    public static final InitMotlCode X_AND_Z_AXIS = new InitMotlCode(2);
    public static final InitMotlCode DEFAULT = ZERO;

    private final EtomoNumber value = new EtomoNumber();

    private InitMotlCode(final int value) {
      this.value.set(value);
    }

    public String toString() {
      return value.toString();
    }

    public boolean isDefault() {
      if (this == DEFAULT) {
        return true;
      }
      return false;
    }

    private static InitMotlCode getInstance(final ReadOnlyAttribute attribute) {
      if (attribute == null) {
        return DEFAULT;
      }
      String value = attribute.getValue();
      if (value == null) {
        return DEFAULT;
      }
      if (ZERO.value.equals(value)) {
        return ZERO;
      }
      if (Z_AXIS.value.equals(value)) {
        return Z_AXIS;
      }
      if (X_AND_Z_AXIS.value.equals(value)) {
        return X_AND_Z_AXIS;
      }
      return DEFAULT;
    }

    public ConstEtomoNumber getValue() {
      return value;
    }
  }

  public static final class MaskType implements EnumeratedType {
    private static final String NONE_VALUE = "none";
    private static final String VOLUME_VALUE = "DUMMY_VOLUME_VALUE";
    private static final String SPHERE_VALUE = "sphere";
    private static final String CYLINDER_VALUE = "cylinder";
    public static final MaskType NONE = new MaskType(NONE_VALUE);
    public static final MaskType VOLUME = new MaskType(VOLUME_VALUE);
    public static final MaskType SPHERE = new MaskType(SPHERE_VALUE);
    public static final MaskType CYLINDER = new MaskType(CYLINDER_VALUE);
    private static final MaskType DEFAULT = NONE;

    private final String value;

    private MaskType(final String value) {
      this.value = value;
    }

    public String toString() {
      return value.toString();
    }

    public ConstEtomoNumber getValue() {
      return null;
    }

    public boolean isDefault() {
      if (this == DEFAULT) {
        return true;
      }
      return false;
    }

    /**
     * Get instance from a string.  An empty string returns the default
     * instance.  An unrecognized string returns a volume instance because the
     * volume string is actually an absolute file path.
     * @param value
     * @return
     */
    public static MaskType getInstance(final String value) {
      if (value == null || value.matches("\\s*")) {
        return DEFAULT;
      }
      if (NONE_VALUE.equals(value)) {
        return NONE;
      }
      if (VOLUME_VALUE.equals(value)) {
        return VOLUME;
      }
      if (SPHERE_VALUE.equals(value)) {
        return SPHERE;
      }
      if (CYLINDER_VALUE.equals(value)) {
        return CYLINDER;
      }
      return VOLUME;
    }

  }

  public static final class SampleSphere implements EnumeratedType {
    private static final String NONE_VALUE = "none";
    private static final String FULL_VALUE = "full";
    private static final String HALF_VALUE = "half";
    public static final SampleSphere NONE = new SampleSphere(NONE_VALUE);
    public static final SampleSphere FULL = new SampleSphere(FULL_VALUE);
    public static final SampleSphere HALF = new SampleSphere(HALF_VALUE);
    private static final SampleSphere DEFAULT = NONE;

    private final String value;

    private SampleSphere(final String value) {
      this.value = value;
    }

    public String toString() {
      return value.toString();
    }

    public ConstEtomoNumber getValue() {
      return null;
    }

    public boolean isDefault() {
      if (this == DEFAULT) {
        return true;
      }
      return false;
    }

    private static SampleSphere getInstance(final String value) {
      if (value == null) {
        return DEFAULT;
      }
      if (NONE_VALUE.equals(value)) {
        return NONE;
      }
      if (FULL_VALUE.equals(value)) {
        return FULL;
      }
      if (HALF_VALUE.equals(value)) {
        return HALF;
      }
      return DEFAULT;
    }
  }

  public static final class YAxisType implements EnumeratedType {
    private static final EtomoNumber Y_AXIS_VALUE = new EtomoNumber().set(0);
    private static final EtomoNumber PARTICLE_MODEL_VALUE = new EtomoNumber()
        .set(1);
    private static final EtomoNumber CONTOUR_VALUE = new EtomoNumber().set(2);

    public static final YAxisType Y_AXIS = new YAxisType(Y_AXIS_VALUE);
    public static final YAxisType PARTICLE_MODEL = new YAxisType(
        PARTICLE_MODEL_VALUE);
    public static final YAxisType CONTOUR = new YAxisType(CONTOUR_VALUE);
    public static final YAxisType DEFAULT = Y_AXIS;

    private final ConstEtomoNumber value;

    private YAxisType(final ConstEtomoNumber value) {
      this.value = value;
    }

    public boolean isDefault() {
      return this == DEFAULT;
    }

    public String toString() {
      return value.toString();
    }

    public ConstEtomoNumber getValue() {
      return null;
    }

    private static YAxisType getInstance(final ReadOnlyAttribute attribute) {
      if (attribute == null) {
        return DEFAULT;
      }
      String value = attribute.getValue();
      if (value == null) {
        return DEFAULT;
      }
      if (Y_AXIS_VALUE.equals(value)) {
        return Y_AXIS;
      }
      if (PARTICLE_MODEL_VALUE.equals(value)) {
        return PARTICLE_MODEL;
      }
      if (CONTOUR_VALUE.equals(value)) {
        return CONTOUR;
      }
      return DEFAULT;
    }
  }

  public static final class CCMode implements EnumeratedType {
    public static final CCMode NORMALIZED = new CCMode(0);
    public static final CCMode LOCAL = new CCMode(1);
    public static final CCMode DEFAULT = NORMALIZED;

    private final EtomoNumber value = new EtomoNumber();

    private CCMode(final int value) {
      this.value.set(value);
    }

    public boolean isDefault() {
      return this == DEFAULT;
    }

    private static CCMode getInstance(final ReadOnlyAttribute attribute) {
      if (attribute == null) {
        return DEFAULT;
      }
      String value = attribute.getValue();
      if (value == null) {
        return DEFAULT;
      }
      if (NORMALIZED.value.equals(value)) {
        return NORMALIZED;
      }
      if (LOCAL.value.equals(value)) {
        return LOCAL;
      }
      return DEFAULT;
    }

    public ConstEtomoNumber getValue() {
      return value;
    }

    public String toString() {
      return value.toString();
    }
  }

  public static final class Volume {
    private static final int START_INDEX = 0;
    private static final int END_INDEX = 1;
    private final ParsedArray tiltRange = ParsedArray
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    private final ParsedArray relativeOrient = ParsedArray
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    private final ParsedQuotedString fnVolume = ParsedQuotedString
        .getInstance();
    private final ParsedQuotedString fnModParticle = ParsedQuotedString
        .getInstance();
    private final ParsedQuotedString initMotl = ParsedQuotedString
        .getInstance();

    private Volume() {
      relativeOrient.setDefault(RELATIVE_ORIENT_DEFAULT);
    }

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
      return tiltRange.getRawString(START_INDEX);
    }

    public void setTiltRangeStart(final String tiltRangeStart) {
      tiltRange.setRawString(START_INDEX, tiltRangeStart);
    }

    public String getTiltRangeEnd() {
      return tiltRange.getRawString(END_INDEX);
    }

    public void setTiltRangeEnd(String tiltRangeEnd) {
      tiltRange.setRawString(END_INDEX, tiltRangeEnd);
    }

    public String getRelativeOrientX() {
      return relativeOrient.getRawString(X_INDEX);
    }

    public void setRelativeOrientX(final String relativeOrientX) {
      relativeOrient.setRawString(X_INDEX, relativeOrientX);
    }

    public String getRelativeOrientY() {
      return relativeOrient.getRawString(Y_INDEX);
    }

    public void setRelativeOrientY(final String relativeOrientY) {
      relativeOrient.setRawString(Y_INDEX, relativeOrientY);
    }

    public String getRelativeOrientZ() {
      return relativeOrient.getRawString(Z_INDEX);
    }

    public void setRelativeOrientZ(final String relativeOrientZ) {
      relativeOrient.setRawString(Z_INDEX, relativeOrientZ);
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

    private boolean isTiltRangeEmpty() {
      return tiltRange.isEmpty();
    }

    private ParsedArray getTiltRange() {
      return tiltRange;
    }

    private ParsedArray getRelativeOrient() {
      return relativeOrient;
    }

    private void setTiltRange(final ParsedElement tiltRange) {
      this.tiltRange.set(tiltRange);
    }

    private void setRelativeOrient(final ParsedElement relativeOrient) {
      this.relativeOrient.set(relativeOrient);
    }
  }

  /**
   * Class to handle Phi, Theta, Psi.  Sets End to 0 if it is null.  Sets Start
   * to the negation of End.  Sets Increment to 1 if it is 0.
   */
  private static final class SearchAngleArea {
    private final ParsedArrayDescriptor descriptor = ParsedArrayDescriptor
        .getInstance(EtomoNumber.Type.FLOAT);

    /**
     * Sets both the End and Start values.  The Start is set to the negation of
     * the End value.  If End is empty, set it and Start to 0.
     * @param input
     */
    private void setEnd(final String input) {
      EtomoNumber end = new EtomoNumber(EtomoNumber.Type.FLOAT);
      end.set(input);
      if (end.isNull()) {
        end.set(0);
      }
      descriptor.setRawStringEnd(end.toString());
      if (!end.equals(0)) {
        end.multiply(-1);
      }
      descriptor.setRawStringStart(end.toString());
    }

    /**
     * Sets the increment value.  If increment is 0, set it to 1.
     * @param input
     */
    private void setIncrement(final String input) {
      EtomoNumber increment = new EtomoNumber(EtomoNumber.Type.FLOAT);
      increment.set(input);
      if (increment.equals(0)) {
        increment.set(1);
      }
      descriptor.setRawStringIncrement(increment.toString());
    }

    private String getEnd() {
      return descriptor.getRawStringEnd();
    }

    private void set(final ParsedElement input) {
      descriptor.set(input);
    }

    private ParsedElement getParsedElement() {
      return descriptor;
    }

    private String getIncrement() {
      return descriptor.getRawStringIncrement();
    }
  }

  public static final class Iteration {
    private static final int CUTOFF_INDEX = 0;
    private static final int SIGMA_INDEX = 1;

    private final ParsedArray searchRadius = ParsedArray.getMatlabInstance();
    private final ParsedArray lowCutoff = ParsedArray
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    private final ParsedArray hiCutoff = ParsedArray
        .getMatlabInstance(EtomoNumber.Type.FLOAT);
    private final ParsedNumber refThreshold = ParsedNumber
        .getMatlabInstance(EtomoNumber.Type.FLOAT);

    //search spaces
    private final SearchAngleArea dPhi = new SearchAngleArea();
    private final SearchAngleArea dTheta = new SearchAngleArea();
    private final SearchAngleArea dPsi = new SearchAngleArea();

    private Iteration() {
    }

    public void setDPhiEnd(final String input) {
      dPhi.setEnd(input);
    }

    public void setDThetaEnd(final String input) {
      dTheta.setEnd(input);
    }

    public void setDPsiEnd(final String input) {
      dPsi.setEnd(input);
    }

    public void setDPhiIncrement(final String input) {
      dPhi.setIncrement(input);
    }

    public void setDThetaIncrement(final String input) {
      dTheta.setIncrement(input);
    }

    public void setDPsiIncrement(final String input) {
      dPsi.setIncrement(input);
    }

    public void setSearchRadius(final String input) {
      this.searchRadius.setRawString(input);
    }

    public String getDPhiEnd() {
      return dPhi.getEnd();
    }

    public String getDThetaEnd() {
      return dTheta.getEnd();
    }

    public String getDPsiEnd() {
      return dPsi.getEnd();
    }

    public void setHiCutoffCutoff(String input) {
      hiCutoff.setRawString(CUTOFF_INDEX, input);
    }

    public void setHiCutoffSigma(String input) {
      hiCutoff.setRawString(SIGMA_INDEX, input);
    }

    public void setRefThreshold(String input) {
      refThreshold.setRawString(input);
    }

    /**
     * assume that the current format is start:inc:end even if inc is empty
     * @return inc
     */
    public String getDPhiIncrement() {
      return dPhi.getIncrement();
    }

    /**
     * assume that the current format is start:inc:end even if inc is empty
     * @return inc
     */
    public String getDThetaIncrement() {
      return dTheta.getIncrement();
    }

    /**
     * assume that the current format is start:inc:end even if inc is empty
     * @return inc
     */
    public String getDPsiIncrement() {
      return dPsi.getIncrement();
    }

    public String getSearchRadiusString() {
      return searchRadius.getRawString();
    }

    public String getHiCutoffCutoff() {
      return hiCutoff.getRawString(CUTOFF_INDEX);
    }

    public String getHiCutoffSigma() {
      return hiCutoff.getRawString(SIGMA_INDEX);
    }

    public String getRefThresholdString() {
      return refThreshold.getRawString();
    }

    private void setDPhi(final ParsedElement input) {
      dPhi.set(input);
    }

    private void setDTheta(final ParsedElement input) {
      dTheta.set(input);
    }

    private void setDPsi(final ParsedElement input) {
      dPsi.set(input);
    }

    private ParsedElement getDPhi() {
      return dPhi.getParsedElement();
    }

    private ParsedElement getDTheta() {
      return dTheta.getParsedElement();
    }

    private ParsedElement getDPsi() {
      return dPsi.getParsedElement();
    }

    private void setSearchRadius(final ParsedElement searchRadius) {
      this.searchRadius.set(searchRadius);
    }

    private ParsedElement getSearchRadius() {
      return searchRadius;
    }

    private void setLowCutoff(final ParsedElement input) {
      lowCutoff.set(input);
    }

    private void setHiCutoff(final ParsedElement input) {
      hiCutoff.set(input);
    }

    private void setLowCutoff(final String input) {
      lowCutoff.setRawString(input);
    }

    private ParsedElement getLowCutoff() {
      return lowCutoff;
    }

    private ParsedElement getHiCutoff() {
      return hiCutoff;
    }

    private String getLowCutoffString() {
      if (lowCutoff.isEmpty()) {
        return LOW_CUTOFF_DEFAULT;
      }
      return lowCutoff.getRawString();
    }

    private ParsedElement getRefThreshold() {
      return refThreshold;
    }

    private void setRefThreshold(final ParsedElement refThreshold) {
      this.refThreshold.setElement(refThreshold);
    }
  }
}
