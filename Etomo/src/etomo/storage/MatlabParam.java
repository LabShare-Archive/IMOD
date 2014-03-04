package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import etomo.BaseManager;
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
import etomo.ui.FieldLabels;
import etomo.ui.UIComponent;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;

/**
 * <p>Description: 
 * prmParser doesn't like:
 * dPhi = {:}
 * szVol = [, , ]
 * outsideMaskRadius = 
 * 
 * </p>
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
 * <p> Revision 1.48  2011/05/19 16:30:40  sueh
 * <p> bug# 1473 Added resetVolumeList.
 * <p>
 * <p> Revision 1.47  2011/05/16 23:06:07  sueh
 * <p> bug# 1487 Backing out bug# 1485 and some of bug# 1445.
 * <p>
 * <p> Revision 1.46  2011/05/15 01:54:52  sueh
 * <p> bug# 1485 Keeping initMotlCode empty when there are init MOTL files conflicts with a new dataset.  Added
 * <p> InitMotlCode.FILES.
 * <p>
 * <p> Revision 1.45  2011/04/20 04:42:59  sueh
 * <p> bug# 1445 Change initMotlCode to a ParsedNumber so it can contain a value that is not in the InitMotlCode
 * <p> enum class.  Added RANDOM_ROTATIONS to the InitMotlCode enum class.  Added resetInitMotlCode() for the
 * <p> initMotlFile option.  Added setInitMotlCode(String) for values that are not in the InitMotlCode enum class.
 * <p>
 * <p> Revision 1.44  2011/02/22 04:47:53  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.43  2011/02/08 00:59:18  sueh
 * <p> bug# 1430 Making sampleInterval a float.
 * <p>
 * <p> Revision 1.42  2010/11/13 16:05:03  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.41  2010/05/27 05:35:12  sueh
 * <p> bug# 1368 Fixed bug number in a comment.
 * <p>
 * <p> Revision 1.40  2010/05/20 23:49:29  sueh
 * <p> bug# 1368 Deprecated flgMeanFill.  Defaulted it to 1.  No longer loading it
 * <p> from the .prm file.  Removed meanFill.
 * <p>
 * <p> Revision 1.39  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.38  2009/12/08 16:01:53  sueh
 * <p> bug# 1287 Added flgAlignAverages.
 * <p>
 * <p> Revision 1.37  2009/12/08 02:43:47  sueh
 * <p> bug# 1286 Added getLstFlagAllTom and getSzVol.
 * <p>
 * <p> Revision 1.36  2009/11/24 00:09:58  sueh
 * <p> bug# 1292 Put YAXIS_CONTOUR_KEY back so it can be used to remove
 * <p> yaxisContour entries.
 * <p>
 * <p> Revision 1.35  2009/11/23 23:25:39  sueh
 * <p> bug# 1292 Removing yaxisContour.  Added yaxisObjectNum and
 * <p> yaxisContourNum.
 * <p>
 * <p> Revision 1.34  2009/11/20 16:56:22  sueh
 * <p> bug# 1282 Added flgRemoveDuplicates, duplicateShiftTolerance, and
 * <p> duplicateAngularTolerance.
 * <p>
 * <p> Revision 1.33  2009/10/30 20:53:34  sueh
 * <p> bug# 1284 Changed the CCMode default to local.
 * <p>
 * <p> Revision 1.32  2009/09/28 18:34:23  sueh
 * <p> bug# 1235 In buildParsableValues, removing edgeShift if it is empty.
 * <p>
 * <p> Revision 1.31  2009/09/20 21:27:59  sueh
 * <p> bug# 1268 Removed LabeledSpinner.setValue(Object).
 * <p>
 * <p> Revision 1.30  2009/09/05 00:33:52  sueh
 * <p> bug# 1256 In write, stopped turning on debug when getting the matlab
 * <p> autodoc.
 * <p>
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
  public static final int REFERENCE_FLG_FAIR_REFERENCE_GROUPS_DEFAULT = 10;
  public static final int REFERENCE_FLG_FAIR_REFERENCE_PARTICLES_DEFAULT = 10;
  public static final String FN_VOLUME_KEY = "fnVolume";
  public static final String FN_MOD_PARTICLE_KEY = "fnModParticle";
  public static final String TILT_RANGE_KEY = "tiltRange";
  /**
   * @deprecated
   */
  private static final String RELATIVE_ORIENT_KEY = "relativeOrient";
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
  public static final String LOW_CUTOFF_SIGMA_DEFAULT = "0.05";
  public static final String HI_CUTOFF_KEY = "hiCutoff";
  /**
   * @deprecated
   */
  public static final String CC_MODE_KEY = "CCMode";
  public static final String REF_THRESHOLD_KEY = "refThreshold";
  public static final String REF_FLAG_ALL_TOM_KEY = "refFlagAllTom";
  public static final String EDGE_SHIFT_KEY = "edgeShift";
  public static final int EDGE_SHIFT_DEFAULT = 1;
  public static final int EDGE_SHIFT_MIN = 0;
  public static final int EDGE_SHIFT_MAX = 3;
  public static final String LST_THRESHOLDS_KEY = "lstThresholds";
  public static final String LST_FLAG_ALL_TOM_KEY = "lstFlagAllTom";
  public static final String ALIGNED_BASE_NAME_KEY = "alignedBaseName";
  public static final String DEBUG_LEVEL_KEY = "debugLevel";
  public static final int DEBUG_LEVEL_MIN = 0;
  public static final int DEBUG_LEVEL_MAX = 3;
  public static final int DEBUG_LEVEL_DEFAULT = 3;
  public static final String PARTICLE_PER_CPU_KEY = "particlePerCPU";
  public static final int PARTICLE_PER_CPU_MIN = 1;
  public static final int PARTICLE_PER_CPU_MAX = 50;
  public static final int PARTICLE_PER_CPU_DEFAULT = 20;
  /**
   * @deprecated replaced by yaxisObject and yaxisContour.
   */
  public static final String YAXIS_CONTOUR_KEY = "yaxisContour";
  public static final String YAXIS_OBJECT_NUM_KEY = "yaxisObjectNum";
  public static final String YAXIS_CONTOUR_NUM_KEY = "yaxisContourNum";
  public static final String FLG_WEDGE_WEIGHT_KEY = "flgWedgeWeight";
  public static final boolean FLG_WEDGE_WEIGHT_DEFAULT = false;
  public static final String SAMPLE_INTERVAL_KEY = "sampleInterval";
  public static final String MASK_TYPE_KEY = "maskType";
  public static final String MASK_MODEL_PTS_KEY = "maskModelPts";
  public static final String INSIDE_MASK_RADIUS_KEY = "insideMaskRadius";
  public static final String OUTSIDE_MASK_RADIUS_KEY = "outsideMaskRadius";
  public static final String N_WEIGHT_GROUP_KEY = "nWeightGroup";
  public static final int N_WEIGHT_GROUP_DEFAULT = 8;
  public static final int N_WEIGHT_GROUP_OFF = 0;
  public static final int N_WEIGHT_GROUP_MIN = 0;
  public static final int N_WEIGHT_GROUP_MAX = 32;
  public static final String FLG_REMOVE_DUPLICATES_KEY = "flgRemoveDuplicates";
  public static final String DUPLICATE_SHIFT_TOLERANCE_KEY = "duplicateShiftTolerance";
  public static final String DUPLICATE_ANGULAR_TOLERANCE_KEY = "duplicateAngularTolerance";
  public static final String FLG_ALIGN_AVERAGES_KEY = "flgAlignAverages";
  public static final String FLG_FAIR_REFERENCE_KEY = "flgFairReference";
  public static final String FLG_ABS_VALUE_KEY = "flgAbsValue";
  public static final boolean FLG_ABS_VALUE_DEFAULT = true;
  public static final String FLG_STRICT_SEARCH_LIMITS_KEY = "flgStrictSearchLimits";
  public static final boolean FLG_STRICT_SEARCH_LIMITS_DEFAULT = false;
  public static final String SELECT_CLASS_ID_KEY = "selectClassID";
  public static final String FLG_NO_REFERENCE_REFINEMENT_KEY = "flgNoReferenceRefinement";

  private static final int VOLUME_INDEX = 0;
  private static final int PARTICLE_INDEX = 1;
  private static final int LEVEL_INDEX = 0;
  private static final int Z_ROTATION_INDEX = 0;
  private static final int Y_ROTATION_INDEX = 1;

  private final ParsedNumber particlePerCpu = ParsedNumber
      .getMatlabInstance(PARTICLE_PER_CPU_KEY);
  private final ParsedArray szVol = ParsedArray.getMatlabInstance(SZ_VOL_KEY);
  private final ParsedQuotedString fnOutput = ParsedQuotedString
      .getInstance(FN_OUTPUT_KEY);
  private final ParsedNumber refFlagAllTom = ParsedNumber
      .getMatlabInstance(REF_FLAG_ALL_TOM_KEY);
  private final ParsedNumber edgeShift = ParsedNumber.getMatlabInstance(EDGE_SHIFT_KEY);
  private final ParsedArray lstThresholds = ParsedArray
      .getMatlabInstance(LST_THRESHOLDS_KEY);
  private final ParsedNumber lstFlagAllTom = ParsedNumber
      .getMatlabInstance(LST_FLAG_ALL_TOM_KEY);
  private final ParsedQuotedString alignedBaseName = ParsedQuotedString
      .getInstance(ALIGNED_BASE_NAME_KEY);
  private final ParsedNumber debugLevel = ParsedNumber.getMatlabInstance(DEBUG_LEVEL_KEY);
  private final List<Volume> volumeList = new ArrayList<Volume>();
  private final List iterationList = new ArrayList();
  private final ParsedQuotedString referenceFile = ParsedQuotedString
      .getInstance(REFERENCE_KEY);
  private final ParsedArray reference = ParsedArray.getMatlabInstance(REFERENCE_KEY);
  private final ParsedNumber yaxisObjectNum = ParsedNumber
      .getMatlabInstance(YAXIS_OBJECT_NUM_KEY);
  private final ParsedNumber yaxisContourNum = ParsedNumber
      .getMatlabInstance(YAXIS_CONTOUR_NUM_KEY);
  private final ParsedNumber flgWedgeWeight = ParsedNumber
      .getMatlabInstance(FLG_WEDGE_WEIGHT_KEY);
  private final ParsedQuotedString sampleSphere = ParsedQuotedString
      .getInstance(SampleSphere.KEY);
  private final ParsedNumber sampleInterval = ParsedNumber.getMatlabInstance(
      EtomoNumber.Type.DOUBLE, SAMPLE_INTERVAL_KEY);
  private final ParsedQuotedString maskType = ParsedQuotedString
      .getInstance(MASK_TYPE_KEY);
  private final ParsedArray maskModelPts = ParsedArray.getMatlabInstance(
      EtomoNumber.Type.DOUBLE, MASK_MODEL_PTS_KEY);
  private final ParsedNumber insideMaskRadius = ParsedNumber
      .getMatlabInstance(INSIDE_MASK_RADIUS_KEY);
  private final ParsedNumber outsideMaskRadius = ParsedNumber
      .getMatlabInstance(OUTSIDE_MASK_RADIUS_KEY);
  private final ParsedNumber nWeightGroup = ParsedNumber
      .getMatlabInstance(N_WEIGHT_GROUP_KEY);
  private final ParsedNumber flgRemoveDuplicates = ParsedNumber
      .getMatlabInstance(FLG_REMOVE_DUPLICATES_KEY);
  private final ParsedNumber flgAlignAverages = ParsedNumber
      .getMatlabInstance(FLG_ALIGN_AVERAGES_KEY);
  private final ParsedNumber flgFairReference = ParsedNumber
      .getMatlabInstance(FLG_FAIR_REFERENCE_KEY);
  private final ParsedNumber flgAbsValue = ParsedNumber
      .getMatlabInstance(FLG_ABS_VALUE_KEY);
  private final ParsedNumber flgStrictSearchLimits = ParsedNumber
      .getMatlabInstance(FLG_STRICT_SEARCH_LIMITS_KEY);
  /**
   * @deprecated
   * backwards compatibility for selectClassID, which used to be a number
   */
  private final ParsedNumber bcSelectClassID = ParsedNumber
      .getMatlabInstance(SELECT_CLASS_ID_KEY);
  private final ParsedArray selectClassID = ParsedArray
      .getMatlabInstance(SELECT_CLASS_ID_KEY);
  private final ParsedNumber flgNoReferenceRefinement = ParsedNumber
      .getMatlabInstance(FLG_NO_REFERENCE_REFINEMENT_KEY);

  private final BaseManager manager;
  private final AxisID axisID;

  private String lowCutoff = LOW_CUTOFF_DEFAULT;
  private String lowCutoffSigma = LOW_CUTOFF_SIGMA_DEFAULT;
  private InitMotlCode initMotlCode = InitMotlCode.DEFAULT;
  private boolean useReferenceFile = false;
  private YAxisType yAxisType = YAxisType.DEFAULT;
  private boolean tiltRangeEmpty = false;
  private boolean isTiltRangeMultiAxes = false;

  private boolean newFile;
  private File file;

  public MatlabParam(final BaseManager manager, final AxisID axisID, File file,
      boolean newFile) {
    this.manager = manager;
    this.axisID = axisID;
    this.file = file;
    this.newFile = newFile;
    nWeightGroup.setDefault(N_WEIGHT_GROUP_DEFAULT);
    flgFairReference.setDefault(false);
    flgAbsValue.setDefault(FLG_ABS_VALUE_DEFAULT);
    flgStrictSearchLimits.setDefault(FLG_STRICT_SEARCH_LIMITS_DEFAULT);
    edgeShift.setDefault(EDGE_SHIFT_DEFAULT);
    flgNoReferenceRefinement.setDefault(false);
    particlePerCpu.setDefault(PARTICLE_PER_CPU_DEFAULT);
    refFlagAllTom.setDefault(1);
    lstFlagAllTom.setDefault(1);
    debugLevel.setDefault(DEBUG_LEVEL_DEFAULT);
    flgWedgeWeight.setDefault(FLG_WEDGE_WEIGHT_DEFAULT);
    insideMaskRadius.setDefault(0);
    flgRemoveDuplicates.setDefault(false);
    flgAlignAverages.setDefault(false);
  }

  /**
   * Change file to newDir + fnOutput + .prm.  Also sets newFile to true.  This allows
   * MatlabParam to read from one file and then write to another.
   * @param newDir
   */
  public void setFile(String newDir) {
    newFile = true;
    file = new File(newDir, fnOutput.getRawString() + DatasetFiles.MATLAB_PARAM_FILE_EXT);
  }

  /**
   * Reads data from the .prm autodoc.
   */
  public synchronized boolean read(BaseManager manager, final List<String> errorList,
      final UIComponent component) {
    clear();
    // if newFile is on, either there is no file, or the user doesn't want to read it
    if (newFile) {
      return true;
    }
    try {
      ReadOnlyAutodoc autodoc = null;
      autodoc = (AutodocFactory.getMatlabInstance(manager, file));
      if (autodoc == null) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Unable to read " + file.getAbsolutePath() + ".", "File Error");
        return false;
      }
      parseData(autodoc, errorList, component);
      if (errorList != null && !errorList.isEmpty()) {
        return false;
      }
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(
          manager,
          "Unable to load " + file.getAbsolutePath() + ".  IOException:  "
              + e.getMessage(), "File Error");
      return false;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(
          manager,
          "Unable to read " + file.getAbsolutePath() + ".  LogFile.ReadException:  "
              + e.getMessage(), "File Error");
      return false;
    }
    return true;
  }

  /**
   * Write stored data to the .prm autodoc.
   */
  public synchronized void write(BaseManager manager) {
    // Place the string representation of each value in a map.
    // This allows the values to be passed to updateOrBuildAutodoc().
    // When building a new .prm autodoc, this also allows the values to be
    // accessed in the same order as the FieldInterface sections in peetprm.adoc.
    Map valueMap = new HashMap();
    buildParsableValues(valueMap);
    // try to get the peetprm.adoc, which contains the comments for the .prm file
    // in its FieldInterface sections.
    ReadOnlyAutodoc commentAutodoc = null;
    try {
      commentAutodoc = AutodocFactory.getInstance(manager, AutodocFactory.PEET_PRM,
          AxisID.ONLY);
    }
    catch (IOException e) {
      System.err.println("Problem with " + AutodocFactory.PEET_PRM
          + ".adoc.\nIOException:  " + e.getMessage());
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      System.err.println("Problem with " + AutodocFactory.PEET_PRM
          + ".adoc.\nLogFile.ReadException:  " + e.getMessage());
    }
    try {
      WritableAutodoc autodoc = AutodocFactory.getMatlabInstance(manager, file);
      if (autodoc == null) {
        // get an empty .prm autodoc if the file doesn't exist
        autodoc = AutodocFactory.getEmptyMatlabInstance(manager, file);
      }
      else {
        LogFile logFile = LogFile.getInstance(file);
        if (!logFile.isBackedup()) {
          logFile.doubleBackupOnce();
        }
        else {
          logFile.backup();
        }
      }
      if (commentAutodoc == null) {
        // The peetprm.adoc is not available.
        // Build a new .prm autodoc with no comments
        updateOrBuildAutodoc(manager, valueMap, autodoc, null);
      }
      else {
        // Get the FieldInterface sections from the peetprm.adoc
        SectionLocation secLoc = commentAutodoc
            .getSectionLocation(EtomoAutodoc.FIELD_SECTION_NAME);
        if (secLoc == null) {
          // There are no FieldInterface sections in the peetprm.adoc.
          // Build a new .prm autodoc with no comments
          updateOrBuildAutodoc(manager, valueMap, autodoc, null);
        }
        else {
          // Build a new .prm autodoc. Use the FieldInterface sections from the
          // peetprm.adoc to dictate the order of the name/value pairs.
          // Also use the comments from the peetprm.adoc FieldInterface sections.
          // This makes MatlabParam dependent on peetprm.adoc so peetprm.adoc
          // must be the responsibility of the Etomo developer.
          updateOrBuildAutodoc(manager, valueMap, autodoc, commentAutodoc);
          /* ReadOnlySection section = null; System.out.println("setNameValuePair"); while
           * ((section = commentAutodoc.nextSection(secLoc)) != null) {
           * setNameValuePair(autodoc, section.getName(), (String) valueMap
           * .get(section.getName()), section .getAttribute(EtomoAutodoc.COMMENT_KEY)); } */
        }
      }
      // write the autodoc file (the backup is done by autodoc)
      autodoc.write();
      // the file is written, so it is no longer new
      newFile = false;
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to load " + file.getName()
          + ".  IOException:  " + e.getMessage(), "File Error");
    }
    catch (LogFile.LockException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to read " + file.getName()
          + ".  LogFile.ReadException:  " + e.getMessage(), "File Error");
    }
  }

  public Volume getVolume(final int index) {
    Volume volume;
    if (index == volumeList.size()) {
      volume = new Volume(manager, axisID);
      volumeList.add(volume);
      return volume;
    }
    return (Volume) volumeList.get(index);
  }

  public Iteration getIteration(final int index) {
    Iteration iteration;
    if (index == iterationList.size()) {
      iteration = new Iteration();
      iteration.setLowCutoff(lowCutoff, lowCutoffSigma);
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

  public String getTiltRangeMultiAxes(final int index) {
    return ((Volume) volumeList.get(index)).getTiltRangeMultiAxesString();
  }

  public InitMotlCode getInitMotlCode() {
    return initMotlCode;
  }

  public YAxisType getYAxisType() {
    return yAxisType;
  }

  public void setInitMotlCode(EnumeratedType enumeratedType) {
    initMotlCode = (InitMotlCode) enumeratedType;
  }

  public void setYaxisType(EnumeratedType enumeratedType) {
    yAxisType = (YAxisType) enumeratedType;
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

  public void setFlgRemoveDuplicates(boolean input) {
    flgRemoveDuplicates.setRawString(input);
  }

  public boolean useReferenceFile() {
    return useReferenceFile;
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

  public boolean isMaskModelPtsEmpty() {
    return maskModelPts.isEmpty();
  }

  /**
   * If the tilt range check box is uncheck, then tilt range should be {}.
   */
  public void setTiltRangeEmpty() {
    tiltRangeEmpty = true;
  }

  public void setTiltRangeMultiAxes(final boolean input) {
    isTiltRangeMultiAxes = input;
  }

  public boolean isTiltRangeMultiAxes() {
    return isTiltRangeMultiAxes;
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

  public boolean isFlgRemoveDuplicates() {
    return flgRemoveDuplicates.getRawBoolean();
  }

  public boolean isRefFlagAllTom() {
    return refFlagAllTom.getRawBoolean();
  }

  public boolean isFlgAlignAverages() {
    return flgAlignAverages.getRawBoolean();
  }

  public boolean isFlgFairReference() {
    return flgFairReference.getRawBoolean();
  }

  public boolean isFlgNoReferenceRefinement() {
    return flgNoReferenceRefinement.getRawBoolean();
  }

  public boolean isFlgAbsValue() {
    return flgAbsValue.getRawBoolean();
  }

  public boolean isFlgStrictSearchLimits() {
    return flgStrictSearchLimits.getRawBoolean();
  }

  public void setFlgAlignAverages(final boolean input) {
    flgAlignAverages.setRawString(input);
  }

  public void setFlgFairReference(final boolean input) {
    flgFairReference.setRawString(input);
  }

  public void setFlgNoReferenceRefinement(final boolean input) {
    flgNoReferenceRefinement.setRawString(input);
  }

  public void setFlgAbsValue(final boolean input) {
    flgAbsValue.setRawString(input);
  }

  public void setFlgStrictSearchLimits(final boolean input) {
    flgStrictSearchLimits.setRawString(input);
  }

  public String getLstFlagAllTom() {
    return lstFlagAllTom.getRawString();
  }

  public boolean isLstFlagAllTom() {
    return lstFlagAllTom.getRawBoolean();
  }

  public boolean isFlgWedgeWeight() {
    return flgWedgeWeight.getRawBoolean();
  }

  public boolean isAlignedBaseNameEmpty() {
    return alignedBaseName.isEmpty();
  }

  public void setAlignedBaseName(String alignedBaseName) {
    this.alignedBaseName.setRawString(alignedBaseName);
  }

  public String getAlignedBaseName() {
    return alignedBaseName.getRawString();
  }

  public void resetAlignedBaseName() {
    this.alignedBaseName.clear();
  }

  public void setReferenceVolume(final Number input) {
    setReferenceVolume(input.toString());
  }

  public void setReferenceVolume(final String input) {
    useReferenceFile = false;
    reference.setRawString(VOLUME_INDEX, input);
  }

  public void setNWeightGroup(final Number input) {
    nWeightGroup.setRawString(input);
  }

  /**
   * Set mastModelPts.  If only one of the rotations is set, the other one should be zero.
   * @param zRotation
   * @param yRotation
   */
  public void setMaskModelPts(String zRotation, String yRotation) {
    boolean zEmpty = zRotation == null || zRotation.matches("\\s*");
    boolean yEmpty = yRotation == null || yRotation.matches("\\s*");
    if (zEmpty && yEmpty) {
      maskModelPts.clear();
      return;
    }
    if (zEmpty || yEmpty) {
      if (zEmpty) {
        zRotation = "0";
      }
      else {
        yRotation = "0";
      }
    }
    maskModelPts.setRawString(Z_ROTATION_INDEX, zRotation);
    maskModelPts.setRawString(Y_ROTATION_INDEX, yRotation);
  }

  public String getMaskModelPtsYRotation() {
    return maskModelPts.getRawString(Y_ROTATION_INDEX);
  }

  public String getMaskModelPtsZRotation() {
    return maskModelPts.getRawString(Z_ROTATION_INDEX);
  }

  public String getReferenceParticle() {
    return reference.getRawString(PARTICLE_INDEX);
  }

  public String getReferenceLevel() {
    return reference.getRawString(LEVEL_INDEX);
  }

  public ParsedElement getReferenceVolume() {
    return reference.getElement(VOLUME_INDEX);
  }

  public String getYaxisObjectNum() {
    return yaxisObjectNum.getRawString();
  }

  public String getYaxisContourNum() {
    return yaxisContourNum.getRawString();
  }

  public SampleSphere getSampleSphere(final UIComponent component) {
    return SampleSphere.getInstance(sampleSphere, component);
  }

  public String getMaskType() {
    return maskType.getRawString();
  }

  public void setEdgeShift(final Number edgeShift) {
    this.edgeShift.setRawString(edgeShift);
  }

  public void clear() {
    particlePerCpu.setRawString(PARTICLE_PER_CPU_DEFAULT);
    szVol.clear();
    fnOutput.clear();
    refFlagAllTom.setRawString(1);
    edgeShift.setRawString(1);
    lstThresholds.clear();
    lstFlagAllTom.setRawString(1);
    alignedBaseName.clear();
    debugLevel.setRawString(DEBUG_LEVEL_DEFAULT);
    volumeList.clear();
    iterationList.clear();
    referenceFile.clear();
    reference.clear();
    lowCutoff = LOW_CUTOFF_DEFAULT;
    lowCutoffSigma = LOW_CUTOFF_SIGMA_DEFAULT;
    initMotlCode = InitMotlCode.DEFAULT;
    useReferenceFile = false;
    yAxisType = YAxisType.DEFAULT;
    yaxisObjectNum.clear();
    yaxisContourNum.clear();
    flgWedgeWeight.setRawString(FLG_WEDGE_WEIGHT_DEFAULT);
    sampleInterval.clear();
    sampleSphere.clear();
    maskType.setRawString(MaskType.DEFAULT.toString());
    maskModelPts.clear();
    insideMaskRadius.setRawString(0);
    outsideMaskRadius.clear();
    nWeightGroup.setRawString(N_WEIGHT_GROUP_DEFAULT);
    tiltRangeEmpty = false;
    flgRemoveDuplicates.setRawString(false);
    flgAlignAverages.setRawString(false);
    flgFairReference.setRawString(false);
    flgAbsValue.setRawString(FLG_ABS_VALUE_DEFAULT);
    flgStrictSearchLimits.setRawString(FLG_STRICT_SEARCH_LIMITS_DEFAULT);
    bcSelectClassID.clear();
    selectClassID.clear();
    flgNoReferenceRefinement.setRawString(false);
  }

  public void clearEdgeShift() {
    edgeShift.clear();
  }

  public ParsedElement getEdgeShift() {
    return edgeShift;
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
  public String getLowCutoffCutoff() {
    if (iterationList.size() == 0) {
      return lowCutoff;
    }
    return ((Iteration) iterationList.get(0)).getLowCutoffCutoffString();
  }

  /**
   * LowCutoff is an iteration value, but it is only set once, so get the value
   * at the first index
   * @return
   */
  public String getLowCutoffSigma() {
    if (iterationList.size() == 0) {
      return lowCutoffSigma;
    }
    return ((Iteration) iterationList.get(0)).getLowCutoffSigmaString();
  }

  /**
   * LowCutoff is only set once, so it is placed in all the Iteration instances
   * If the Iteration instances haven't been created yet, it should be add to them
   * from lowCutoff when they are.
   * @param input
   */
  public void setLowCutoff(final String cutoff, final String sigma) {
    lowCutoff = cutoff;
    lowCutoffSigma = sigma;
    for (int i = 0; i < iterationList.size(); i++) {
      ((Iteration) iterationList.get(i)).setLowCutoff(lowCutoff, lowCutoffSigma);
    }
  }

  public void setDebugLevel(final Number input) {
    debugLevel.setRawString(input.toString());
  }

  public void setParticlePerCPU(final Number input) {
    particlePerCpu.setRawString(input.toString());
  }

  public void setSelectClassID(final String input) {
    selectClassID.setRawString(input);
  }

  public ConstEtomoNumber getDebugLevel() {
    return debugLevel.getEtomoNumber();
  }

  public ConstEtomoNumber getParticlePerCPU() {
    return particlePerCpu.getEtomoNumber();
  }

  public String getSelectClassID() {
    return selectClassID.getRawString();
  }

  public String getSzVol() {
    return szVol.getRawString();
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

  public void setReferenceLevel(final String input) {
    useReferenceFile = false;
    reference.setRawString(LEVEL_INDEX, input);
  }

  public void setReferenceParticle(final Number input) {
    useReferenceFile = false;
    reference.setRawString(PARTICLE_INDEX, input.toString());
  }

  public void clearMaskModelPts() {
    maskModelPts.clear();
  }

  public void setYaxisObjectNum(final String input) {
    yaxisObjectNum.setRawString(input);
  }

  public void setYaxisContourNum(final String input) {
    yaxisContourNum.setRawString(input);
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
    // if volume list is too small, add new Volumes
    for (int i = volumeList.size(); i < size; i++) {
      volumeList.add(new Volume(manager, axisID));
    }
    // if volume list is too big, remove Volumes from the end
    for (int i = size; i < volumeList.size(); i++) {
      volumeList.remove(i);
    }
  }

  public void setIterationListSize(final int size) {
    // if iteration list is too small, add new Iterations
    for (int i = iterationList.size(); i < size; i++) {
      Iteration iteration = new Iteration();
      iteration.setLowCutoff(lowCutoff, "");
      iterationList.add(iteration);
    }
    // if iteration list is too big, remove Iterations from the end
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
   * @param element
   * @param errorList
   * @return true if error found
   */
  private boolean addError(final ParsedElement element, final List<String> errorList) {
    boolean retval = false;
    if (errorList != null) {
      String error = element.validate();
      if (error != null) {
        errorList.add(error);
        retval = true;
      }
    }
    return retval;
  }

  private void addError(final ParsedList list, final List<String> errorList) {
    if (errorList != null) {
      String error = list.validate();
      if (error != null) {
        errorList.add(error);
      }
    }
  }

  /**
   * Called by read().  Parses data from the the file.
   * @param autodoc
   */
  private void parseData(final ReadOnlyAutodoc autodoc, final List<String> errorList,
      final UIComponent component) {
    parseVolumeData(autodoc, errorList, component);
    parseIterationData(autodoc, errorList, component);
    String error = null;
    // reference
    ReadOnlyAttribute attribute = autodoc.getAttribute(REFERENCE_KEY);
    if (ParsedQuotedString.isQuotedString(attribute)) {
      useReferenceFile = true;
      referenceFile.parse(attribute);
      addError(referenceFile, errorList);
    }
    else {
      useReferenceFile = false;
      reference.parse(attribute);
      addError(reference, errorList);
    }
    // particlePerCPU
    particlePerCpu.parse(autodoc.getAttribute(PARTICLE_PER_CPU_KEY));
    addError(particlePerCpu, errorList);
    // szVol
    szVol.parse(autodoc.getAttribute(SZ_VOL_KEY));
    addError(szVol, errorList);
    // fnOutput
    fnOutput.parse(autodoc.getAttribute(FN_OUTPUT_KEY));
    addError(fnOutput, errorList);
    // refFlagAllTom
    refFlagAllTom.parse(autodoc.getAttribute(REF_FLAG_ALL_TOM_KEY));
    if (!addError(refFlagAllTom, errorList)) {
      checkValue(refFlagAllTom, new int[] { 0, 1 }, component, REF_FLAG_ALL_TOM_KEY,
          null, 1);
    }
    // edgeShift
    edgeShift.parse(autodoc.getAttribute(EDGE_SHIFT_KEY));
    if (!addError(edgeShift, errorList)) {
      checkValue(edgeShift, EDGE_SHIFT_MIN, EDGE_SHIFT_MAX, 1, component, EDGE_SHIFT_KEY,
          FieldLabels.EDGE_SHIFT_LABEL, String.valueOf(EDGE_SHIFT_DEFAULT));
    }
    // lstThresholds
    lstThresholds.parse(autodoc.getAttribute(LST_THRESHOLDS_KEY));
    addError(lstThresholds, errorList);
    // lstFlagAllTom
    lstFlagAllTom.parse(autodoc.getAttribute(LST_FLAG_ALL_TOM_KEY));
    addError(lstFlagAllTom, errorList);
    if (!addError(lstFlagAllTom, errorList)) {
      checkValue(lstFlagAllTom, new int[] { 0, 1 }, component, LST_FLAG_ALL_TOM_KEY,
          null, 1);
    }
    // alignedBaseName
    alignedBaseName.parse(autodoc.getAttribute(ALIGNED_BASE_NAME_KEY));
    addError(alignedBaseName, errorList);
    // debugLevel
    debugLevel.parse(autodoc.getAttribute(DEBUG_LEVEL_KEY));
    addError(debugLevel, errorList);
    if (!addError(debugLevel, errorList)) {
      checkValue(debugLevel, DEBUG_LEVEL_MIN, DEBUG_LEVEL_MAX, 1, component,
          DEBUG_LEVEL_KEY, FieldLabels.DEBUG_LEVEL_LABEL,
          String.valueOf(DEBUG_LEVEL_DEFAULT));
    }
    // YaxisType
    yAxisType = YAxisType.getInstance(autodoc.getAttribute(YAxisType.KEY), component);
    // YaxisObjectNum
    yaxisObjectNum.parse(autodoc.getAttribute(YAXIS_OBJECT_NUM_KEY));
    addError(yaxisObjectNum, errorList);
    // YaxisContourNum
    yaxisContourNum.parse(autodoc.getAttribute(YAXIS_CONTOUR_NUM_KEY));
    addError(yaxisContourNum, errorList);
    // flgWedgeWeight
    flgWedgeWeight.parse(autodoc.getAttribute(FLG_WEDGE_WEIGHT_KEY));
    if (!addError(flgWedgeWeight, errorList)) {
      checkValue(flgWedgeWeight, new int[] { 0, 1 }, component, FLG_WEDGE_WEIGHT_KEY,
          null, 1);
    }
    // sampleSphere
    sampleSphere.parse(autodoc.getAttribute(SampleSphere.KEY));
    addError(sampleSphere, errorList);
    // sampleInterval
    sampleInterval.parse(autodoc.getAttribute(SAMPLE_INTERVAL_KEY));
    addError(sampleInterval, errorList);
    // maskType
    maskType.parse(autodoc.getAttribute(MASK_TYPE_KEY));
    addError(maskType, errorList);
    // maskModelPts
    maskModelPts.parse(autodoc.getAttribute(MASK_MODEL_PTS_KEY));
    addError(maskModelPts, errorList);
    // insideMaskRadius
    insideMaskRadius.parse(autodoc.getAttribute(INSIDE_MASK_RADIUS_KEY));
    addError(insideMaskRadius, errorList);
    // outsideMaskRadius
    outsideMaskRadius.parse(autodoc.getAttribute(OUTSIDE_MASK_RADIUS_KEY));
    addError(outsideMaskRadius, errorList);
    // nWeightGroup
    nWeightGroup.parse(autodoc.getAttribute(N_WEIGHT_GROUP_KEY));
    if (!addError(nWeightGroup, errorList)) {
      checkValue(nWeightGroup, N_WEIGHT_GROUP_MIN, N_WEIGHT_GROUP_MAX, 1, component,
          N_WEIGHT_GROUP_KEY, FieldLabels.N_WEIGHT_GROUP_LABEL,
          String.valueOf(N_WEIGHT_GROUP_DEFAULT));
    }
    // flgRemoveDuplicates
    flgRemoveDuplicates.parse(autodoc.getAttribute(FLG_REMOVE_DUPLICATES_KEY));
    addError(flgRemoveDuplicates, errorList);
    if (!addError(flgRemoveDuplicates, errorList)) {
      checkValue(flgRemoveDuplicates, new int[] { 0, 1 }, component,
          FLG_REMOVE_DUPLICATES_KEY, FieldLabels.FLG_REMOVE_DUPLICATES_LABEL, 1);
    }
    // flgAlignAverages
    flgAlignAverages.parse(autodoc.getAttribute(FLG_ALIGN_AVERAGES_KEY));
    addError(flgAlignAverages, errorList);
    if (!addError(flgAlignAverages, errorList)) {
      checkValue(flgAlignAverages, new int[] { 0, 1 }, component, FLG_ALIGN_AVERAGES_KEY,
          FieldLabels.FLG_ALIGN_AVERAGES_LABEL, 1);
    }
    // flgFairReference
    flgFairReference.parse(autodoc.getAttribute(FLG_FAIR_REFERENCE_KEY));
    if (!addError(flgFairReference, errorList)) {
      checkValue(flgFairReference, new int[] { 0, 1 }, component, FLG_FAIR_REFERENCE_KEY,
          FieldLabels.FLG_FAIR_REFERENCE_LABEL, -1);
    }
    // flgAbsValue
    flgAbsValue.parse(autodoc.getAttribute(FLG_ABS_VALUE_KEY));
    if (!addError(flgAbsValue, errorList)) {
      checkValue(flgAbsValue, new int[] { 0, 1 }, component, FLG_ABS_VALUE_KEY,
          FieldLabels.FLG_ABS_VALUE_LABEL, 1);
    }
    // flgStrictSearchLimits
    flgStrictSearchLimits.parse(autodoc.getAttribute(FLG_STRICT_SEARCH_LIMITS_KEY));
    addError(flgStrictSearchLimits, errorList);
    if (!addError(flgStrictSearchLimits, errorList)) {
      checkValue(flgStrictSearchLimits, new int[] { 0, 1 }, component,
          FLG_STRICT_SEARCH_LIMITS_KEY, FieldLabels.FLG_STRICT_SEARCH_LIMITS_LABEL, 1);
    }
    // selectClassID
    attribute = autodoc.getAttribute(SELECT_CLASS_ID_KEY);
    selectClassID.parse(attribute);
    // Backwards compatibility - read it in if its a number
    if (selectClassID.validate() != null) {
      bcSelectClassID.parse(attribute);
      if (bcSelectClassID.validate() != null) {
        addError(selectClassID, errorList);
      }
      else {
        selectClassID.setRawString(bcSelectClassID.getRawString());
      }
    }
    // FlgNoReferenceRefinement
    flgNoReferenceRefinement.parse(autodoc.getAttribute(FLG_NO_REFERENCE_REFINEMENT_KEY));
    if (!addError(flgNoReferenceRefinement, errorList)) {
      checkValue(flgNoReferenceRefinement, new int[] { 0, 1 }, component,
          FLG_NO_REFERENCE_REFINEMENT_KEY, FieldLabels.FLG_NO_REFERENCE_REFINEMENT_LABEL,
          -1);
    }
  }

  void checkValue(final ParsedNumber number, final int[] expectedValues,
      final UIComponent component, final String paramName, final String fieldLabel,
      final int replacementValueIndex) {
    if (number == null || number.isMissingAttribute() || expectedValues == null) {
      return;
    }
    if (expectedValues != null) {
      boolean ok = false;
      for (int i = 0; i < expectedValues.length; i++) {
        if (number.equals(expectedValues[i])) {
          ok = true;
        }
      }
      if (!ok) {
        UIHarness.INSTANCE
            .openProblemValueMessageDialog(
                component,
                "Unknown",
                paramName,
                null,
                fieldLabel,
                number.getRawString(),
                replacementValueIndex != -1
                    && replacementValueIndex < expectedValues.length ? String
                    .valueOf(expectedValues[replacementValueIndex]) : null, null);
      }
    }
  }

  /**
   * Checks for an out of range value. If it is, pops up a warning and changes number to
   * to replacement value (if replacement value is not null).
   * @param number
   * @param min
   * @param max
   * @param step
   * @param component
   * @param paramName
   * @param fieldLabel
   * @param replacementValue
   */
  void checkValue(final ParsedNumber number, final int min, final int max,
      final int step, final UIComponent component, final String paramName,
      final String fieldLabel, final String replacementValue) {
    if (number == null || number.isMissingAttribute()) {
      return;
    }
    Number nNumber = number.getRawNumber();
    if (nNumber == null) {
      return;
    }
    int num = nNumber.intValue();
    boolean ok = false;
    if (step == 1) {
      if (num >= min && num <= max) {
        ok = true;
      }
    }
    else {
      for (int i = min; i <= max; i += step) {
        if (num == i) {
          ok = true;
          break;
        }
      }
    }
    if (!ok) {
      UIHarness.INSTANCE.openProblemValueMessageDialog(component, "Out of range",
          paramName, null, fieldLabel, number.getRawString(), replacementValue, null);
      if (replacementValue != null) {
        number.setRawString(replacementValue);
      }
      return;
    }
    return;
  }

  public boolean validate(final boolean forRun) {
    Iterator<Volume> i = volumeList.iterator();
    while (i.hasNext()) {
      if (!i.next().validate(forRun)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Parses data from the the file.
   * @param autodoc
   */
  private void parseVolumeData(final ReadOnlyAutodoc autodoc,
      final List<String> errorList, final UIComponent component) {
    volumeList.clear();
    int size = 0;
    String error = null;
    // relativeOrient
    ParsedList relativeOrient = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE,
        RELATIVE_ORIENT_KEY);
    relativeOrient.parse(autodoc.getAttribute(RELATIVE_ORIENT_KEY));
    addError(relativeOrient, errorList);
    size = Math.max(size, relativeOrient.size());
    // fnVolume
    ParsedList fnVolume = ParsedList.getStringInstance(FN_VOLUME_KEY);
    fnVolume.parse(autodoc.getAttribute(FN_VOLUME_KEY));
    addError(fnVolume, errorList);
    size = Math.max(size, fnVolume.size());
    // fnModParticle
    ParsedList fnModParticle = ParsedList.getStringInstance(FN_MOD_PARTICLE_KEY);
    fnModParticle.parse(autodoc.getAttribute(FN_MOD_PARTICLE_KEY));
    addError(fnModParticle, errorList);
    size = Math.max(size, fnModParticle.size());
    // initMOTL
    ParsedList initMotlFile = null;
    ReadOnlyAttribute attribute = autodoc.getAttribute(InitMotlCode.KEY);
    if (ParsedList.isList(attribute)) {
      initMotlCode = null;
      initMotlFile = ParsedList.getStringInstance(InitMotlCode.KEY);
      initMotlFile.parse(attribute);
      addError(initMotlFile, errorList);
      size = Math.max(size, initMotlFile.size());
    }
    else {
      initMotlCode = InitMotlCode.getInstance(attribute, component);
    }
    // tiltRange
    ParsedList tiltRange;
    attribute = autodoc.getAttribute(TILT_RANGE_KEY);
    if (!ParsedList.isStringList(attribute)) {
      isTiltRangeMultiAxes = false;
      tiltRange = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE, TILT_RANGE_KEY);
    }
    else {
      isTiltRangeMultiAxes = true;
      tiltRange = ParsedList.getStringInstance(TILT_RANGE_KEY);
    }
    tiltRange.parse(attribute);
    addError(tiltRange, errorList);
    size = Math.max(size, tiltRange.size());
    // Add elements to volumeList
    for (int i = 0; i < size; i++) {
      Volume volume = new Volume(manager, axisID);
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

  public void resetVolumeList() {
    for (int i = 0; i < volumeList.size(); i++) {
      volumeList.clear();
    }
  }

  /**
   * Parses data from the the file.
   * @param autodoc
   */
  private void parseIterationData(final ReadOnlyAutodoc autodoc,
      final List<String> errorList, final UIComponent component) {
    iterationList.clear();
    int size = 0;
    // dPhi
    ParsedList dPhi = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE, D_PHI_KEY);
    dPhi.parse(autodoc.getAttribute(D_PHI_KEY));
    addError(dPhi, errorList);
    size = Math.max(size, dPhi.size());
    // dTheta
    ParsedList dTheta = ParsedList
        .getMatlabInstance(EtomoNumber.Type.DOUBLE, D_THETA_KEY);
    dTheta.parse(autodoc.getAttribute(D_THETA_KEY));
    addError(dTheta, errorList);
    size = Math.max(size, dTheta.size());
    // dPsi
    ParsedList dPsi = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE, D_PSI_KEY);
    dPsi.parse(autodoc.getAttribute(D_PSI_KEY));
    addError(dPsi, errorList);
    size = Math.max(size, dPsi.size());
    // searchRadius
    ParsedList searchRadius = ParsedList.getMatlabInstance(SEARCH_RADIUS_KEY);
    searchRadius.parse(autodoc.getAttribute(SEARCH_RADIUS_KEY));
    addError(searchRadius, errorList);
    size = Math.max(size, searchRadius.size());
    // lowCutoff
    ParsedList lowCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE,
        LOW_CUTOFF_KEY);
    lowCutoff.parse(autodoc.getAttribute(LOW_CUTOFF_KEY));
    addError(lowCutoff, errorList);
    // hiCutoff
    ParsedList hiCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE,
        HI_CUTOFF_KEY);
    hiCutoff.parse(autodoc.getAttribute(HI_CUTOFF_KEY));
    addError(hiCutoff, errorList);
    size = Math.max(size, hiCutoff.size());
    // refThreshold
    ParsedList refThreshold = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE,
        REF_THRESHOLD_KEY);
    refThreshold.parse(autodoc.getAttribute(REF_THRESHOLD_KEY));
    addError(refThreshold, errorList);
    // duplicateShiftTolerance
    ParsedArray duplicateShiftTolerance = ParsedArray
        .getMatlabInstance(DUPLICATE_SHIFT_TOLERANCE_KEY);
    duplicateShiftTolerance.parse(autodoc.getAttribute(DUPLICATE_SHIFT_TOLERANCE_KEY));
    addError(duplicateShiftTolerance, errorList);
    // duplicateAngularTolerance
    ParsedArray duplicateAngularTolerance = ParsedArray
        .getMatlabInstance(DUPLICATE_ANGULAR_TOLERANCE_KEY);
    duplicateAngularTolerance
        .parse(autodoc.getAttribute(DUPLICATE_ANGULAR_TOLERANCE_KEY));
    addError(duplicateAngularTolerance, errorList);
    size = Math.max(size, refThreshold.size());
    // add elements to iterationList
    for (int i = 0; i < size; i++) {
      Iteration iteration = new Iteration();
      iteration.setDPhi(dPhi.getElement(i), component);
      iteration.setDTheta(dTheta.getElement(i), component);
      iteration.setDPsi(dPsi.getElement(i), component);
      iteration.setSearchRadius(searchRadius.getElement(i));
      iteration.setLowCutoff(lowCutoff.getElement(i));
      iteration.setHiCutoff(hiCutoff.getElement(i));
      iteration.setRefThreshold(refThreshold.getElement(i));
      iteration.setDuplicateShiftTolerance(duplicateShiftTolerance.getElement(i));
      iteration.setDuplicateAngularTolerance(duplicateAngularTolerance.getElement(i));
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
    // copy szVol X value to Y and Z when Y and/or Z is empty
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
    if (!isTiltRangeEmpty() && !edgeShift.isEmpty()) {
      valueMap.put(EDGE_SHIFT_KEY, edgeShift.getParsableString());
    }
    if (initMotlCode != null) {
      valueMap.put(InitMotlCode.KEY, initMotlCode.toString());
    }
    valueMap.put(ALIGNED_BASE_NAME_KEY, alignedBaseName.getParsableString());
    valueMap.put(DEBUG_LEVEL_KEY, debugLevel.getParsableString());
    valueMap.put(LST_THRESHOLDS_KEY, lstThresholds.getParsableString());
    valueMap.put(REF_FLAG_ALL_TOM_KEY, refFlagAllTom.getParsableString());
    valueMap.put(LST_FLAG_ALL_TOM_KEY, lstFlagAllTom.getParsableString());
    valueMap.put(PARTICLE_PER_CPU_KEY, particlePerCpu.getParsableString());
    valueMap.put(YAxisType.KEY, yAxisType.toString());
    valueMap.put(YAXIS_OBJECT_NUM_KEY, yaxisObjectNum.getParsableString());
    valueMap.put(YAXIS_CONTOUR_NUM_KEY, yaxisContourNum.getParsableString());
    valueMap.put(FLG_WEDGE_WEIGHT_KEY, flgWedgeWeight.getParsableString());
    valueMap.put(SampleSphere.KEY, sampleSphere.getParsableString());
    valueMap.put(SAMPLE_INTERVAL_KEY, sampleInterval.getParsableString());
    valueMap.put(MASK_TYPE_KEY, maskType.getParsableString());
    valueMap.put(MASK_MODEL_PTS_KEY, maskModelPts.getParsableString());
    valueMap.put(INSIDE_MASK_RADIUS_KEY, insideMaskRadius.getParsableString());
    valueMap.put(OUTSIDE_MASK_RADIUS_KEY, outsideMaskRadius.getParsableString());
    valueMap.put(N_WEIGHT_GROUP_KEY, nWeightGroup.getParsableString());
    valueMap.put(FLG_REMOVE_DUPLICATES_KEY, flgRemoveDuplicates.getParsableString());
    valueMap.put(FLG_ALIGN_AVERAGES_KEY, flgAlignAverages.getParsableString());
    valueMap.put(FLG_FAIR_REFERENCE_KEY, flgFairReference.getParsableString());
    valueMap.put(FLG_ABS_VALUE_KEY, flgAbsValue.getParsableString());
    valueMap.put(FLG_STRICT_SEARCH_LIMITS_KEY, flgStrictSearchLimits.getParsableString());
    if (!selectClassID.isEmpty()) {
      valueMap.put(SELECT_CLASS_ID_KEY, selectClassID.getParsableString());
    }
    else {
      valueMap.remove(SELECT_CLASS_ID_KEY);
    }
    valueMap.put(FLG_NO_REFERENCE_REFINEMENT_KEY,
        flgNoReferenceRefinement.getParsableString());
  }

  /**
   * Places parsable strings (string that will be written to
   * the file) into a Map in preparation for writing.
   * @param valueMap
   */
  private void buildParsableVolumeValues(final Map valueMap) {
    ParsedList fnVolume = ParsedList.getStringInstance(FN_VOLUME_KEY);
    ParsedList fnModParticle = ParsedList.getStringInstance(FN_MOD_PARTICLE_KEY);
    ParsedList initMotlFile = null;
    if (initMotlCode == null) {
      initMotlFile = ParsedList.getStringInstance(InitMotlCode.KEY);
    }
    ParsedList tiltRange;
    if (!isTiltRangeMultiAxes) {
      tiltRange = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE, TILT_RANGE_KEY);
    }
    else {
      tiltRange = ParsedList.getStringInstance(TILT_RANGE_KEY);
    }
    // build the lists
    for (int i = 0; i < volumeList.size(); i++) {
      Volume volume = (Volume) volumeList.get(i);
      fnVolume.addElement(volume.getFnVolume());
      fnModParticle.addElement(volume.getFnModParticle());
      if (initMotlCode == null) {
        initMotlFile.addElement(volume.getInitMotl());
      }
      tiltRange.addElement(volume.getTiltRange(isTiltRangeMultiAxes));
    }
    valueMap.put(FN_VOLUME_KEY, fnVolume.getParsableString());
    valueMap.put(FN_MOD_PARTICLE_KEY, fnModParticle.getParsableString());
    if (initMotlCode == null) {
      valueMap.put(InitMotlCode.KEY, initMotlFile.getParsableString());
    }
    if (tiltRangeEmpty) {
      tiltRange.clear();
    }
    valueMap.put(TILT_RANGE_KEY, tiltRange.getParsableString());
  }

  /**
   * Places parsable strings (string that will be written to
   * the file) into a Map in preparation for writing.
   * @param valueMap
   */
  private void buildParsableIterationValues(final Map valueMap) {
    ParsedList dPhi = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE, D_PHI_KEY);
    ParsedList dTheta = ParsedList
        .getMatlabInstance(EtomoNumber.Type.DOUBLE, D_THETA_KEY);
    ParsedList dPsi = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE, D_PSI_KEY);
    ParsedList searchRadius = ParsedList.getMatlabInstance(SEARCH_RADIUS_KEY);
    ParsedList lowCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE,
        LOW_CUTOFF_KEY);
    ParsedList hiCutoff = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE,
        HI_CUTOFF_KEY);
    ParsedList refThreshold = ParsedList.getMatlabInstance(EtomoNumber.Type.DOUBLE,
        REF_THRESHOLD_KEY);
    ParsedArray duplicateShiftTolerance = ParsedArray
        .getMatlabInstance(DUPLICATE_SHIFT_TOLERANCE_KEY);
    ParsedArray duplicateAngularTolerance = ParsedArray
        .getMatlabInstance(DUPLICATE_ANGULAR_TOLERANCE_KEY);
    // build the lists
    for (int i = 0; i < iterationList.size(); i++) {
      Iteration iteration = (Iteration) iterationList.get(i);
      dPhi.addElement(iteration.getDPhi());
      dTheta.addElement(iteration.getDTheta());
      dPsi.addElement(iteration.getDPsi());
      searchRadius.addElement(iteration.getSearchRadius());
      lowCutoff.addElement(iteration.getLowCutoff());
      hiCutoff.addElement(iteration.getHiCutoff());
      refThreshold.addElement(iteration.getRefThreshold());
      duplicateShiftTolerance.addElement(iteration.getDuplicateShiftTolerance());
      duplicateAngularTolerance.addElement(iteration.getDuplicateAngularTolerance());

    }
    valueMap.put(D_PHI_KEY, dPhi.getParsableString());
    valueMap.put(D_THETA_KEY, dTheta.getParsableString());
    valueMap.put(D_PSI_KEY, dPsi.getParsableString());
    valueMap.put(SEARCH_RADIUS_KEY, searchRadius.getParsableString());
    valueMap.put(LOW_CUTOFF_KEY, lowCutoff.getParsableString());
    valueMap.put(HI_CUTOFF_KEY, hiCutoff.getParsableString());
    valueMap.put(REF_THRESHOLD_KEY, refThreshold.getParsableString());
    valueMap.put(DUPLICATE_SHIFT_TOLERANCE_KEY,
        duplicateShiftTolerance.getParsableString());
    valueMap.put(DUPLICATE_ANGULAR_TOLERANCE_KEY,
        duplicateAngularTolerance.getParsableString());
  }

  /**
   * Called by write().  Updates or adds all the name/value pair to autodoc.
   * Will attempt to add comments when adding a new name/value pair.
   * Adds attributes when it adds a new name/value pair.
   * @param valueMap
   * @param autodoc
   * @param commentAutodoc
   */
  private void updateOrBuildAutodoc(BaseManager manager, final Map valueMap,
      final WritableAutodoc autodoc, final ReadOnlyAutodoc commentAutodoc) {
    Map commentMap = null;
    if (commentAutodoc != null) {
      commentMap = commentAutodoc.getAttributeMultiLineValues(
          EtomoAutodoc.FIELD_SECTION_NAME, EtomoAutodoc.COMMENT_KEY);
    }
    // write to a autodoc, name/value pairs as necessary
    // the order doesn't matter, because this is either an existing autodoc
    // (so new entries will end up at the bottom), or the comment autodoc (which
    // provides the order) is not usable.
    setNameValuePairValues(manager, valueMap, autodoc, commentMap);
  }

  /**
   * Adds or changes the value of an name/value pair in the file.
   * @param valueMap
   * @param autodoc
   * @param commentMap
   */
  private void setNameValuePairValues(BaseManager manager, final Map valueMap,
      final WritableAutodoc autodoc, final Map commentMap) {
    setVolumeNameValuePairValues(manager, valueMap, autodoc, commentMap);
    setIterationNameValuePairValues(manager, valueMap, autodoc, commentMap);
    setNameValuePairValue(manager, autodoc, REFERENCE_KEY,
        (String) valueMap.get(REFERENCE_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FN_OUTPUT_KEY,
        (String) valueMap.get(FN_OUTPUT_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, SZ_VOL_KEY,
        (String) valueMap.get(SZ_VOL_KEY), commentMap);
    if (isTiltRangeEmpty()) {
      removeNameValuePair(autodoc, EDGE_SHIFT_KEY);
    }
    else {
      setNameValuePairValue(manager, autodoc, EDGE_SHIFT_KEY,
          (String) valueMap.get(EDGE_SHIFT_KEY), commentMap);
    }
    setNameValuePairValue(manager, autodoc, ALIGNED_BASE_NAME_KEY,
        (String) valueMap.get(ALIGNED_BASE_NAME_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, DEBUG_LEVEL_KEY,
        (String) valueMap.get(DEBUG_LEVEL_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, LST_THRESHOLDS_KEY,
        (String) valueMap.get(LST_THRESHOLDS_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, REF_FLAG_ALL_TOM_KEY,
        (String) valueMap.get(REF_FLAG_ALL_TOM_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, LST_FLAG_ALL_TOM_KEY,
        (String) valueMap.get(LST_FLAG_ALL_TOM_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, PARTICLE_PER_CPU_KEY,
        (String) valueMap.get(PARTICLE_PER_CPU_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, YAxisType.KEY,
        (String) valueMap.get(YAxisType.KEY), commentMap);
    removeNameValuePair(autodoc, YAXIS_CONTOUR_KEY);
    setNameValuePairValue(manager, autodoc, YAXIS_OBJECT_NUM_KEY,
        (String) valueMap.get(YAXIS_OBJECT_NUM_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, YAXIS_CONTOUR_NUM_KEY,
        (String) valueMap.get(YAXIS_CONTOUR_NUM_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FLG_WEDGE_WEIGHT_KEY,
        (String) valueMap.get(FLG_WEDGE_WEIGHT_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, SampleSphere.KEY,
        (String) valueMap.get(SampleSphere.KEY), commentMap);
    setNameValuePairValue(manager, autodoc, SAMPLE_INTERVAL_KEY,
        (String) valueMap.get(SAMPLE_INTERVAL_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, MASK_TYPE_KEY,
        (String) valueMap.get(MASK_TYPE_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, MASK_MODEL_PTS_KEY,
        (String) valueMap.get(MASK_MODEL_PTS_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, INSIDE_MASK_RADIUS_KEY,
        (String) valueMap.get(INSIDE_MASK_RADIUS_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, OUTSIDE_MASK_RADIUS_KEY,
        (String) valueMap.get(OUTSIDE_MASK_RADIUS_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, N_WEIGHT_GROUP_KEY,
        (String) valueMap.get(N_WEIGHT_GROUP_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FLG_REMOVE_DUPLICATES_KEY,
        (String) valueMap.get(FLG_REMOVE_DUPLICATES_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FLG_ALIGN_AVERAGES_KEY,
        (String) valueMap.get(FLG_ALIGN_AVERAGES_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FLG_FAIR_REFERENCE_KEY,
        (String) valueMap.get(FLG_FAIR_REFERENCE_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FLG_ABS_VALUE_KEY,
        (String) valueMap.get(FLG_ABS_VALUE_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FLG_STRICT_SEARCH_LIMITS_KEY,
        (String) valueMap.get(FLG_STRICT_SEARCH_LIMITS_KEY), commentMap);
    String value = (String) valueMap.get(SELECT_CLASS_ID_KEY);
    if (value != null) {
      setNameValuePairValue(manager, autodoc, SELECT_CLASS_ID_KEY,
          (String) valueMap.get(SELECT_CLASS_ID_KEY), commentMap);
    }
    else {
      removeNameValuePair(autodoc, SELECT_CLASS_ID_KEY);
    }
    setNameValuePairValue(manager, autodoc, FLG_NO_REFERENCE_REFINEMENT_KEY,
        (String) valueMap.get(FLG_NO_REFERENCE_REFINEMENT_KEY), commentMap);
  }

  /**
   * Adds or changes the value of an name/value pair in the file.
   * @param valueMap
   * @param autodoc
   * @param commentMap
   */
  private void setVolumeNameValuePairValues(BaseManager manager, final Map valueMap,
      final WritableAutodoc autodoc, final Map commentMap) {
    setNameValuePairValue(manager, autodoc, FN_VOLUME_KEY,
        (String) valueMap.get(FN_VOLUME_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, FN_MOD_PARTICLE_KEY,
        (String) valueMap.get(FN_MOD_PARTICLE_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, InitMotlCode.KEY,
        (String) valueMap.get(InitMotlCode.KEY), commentMap);
    setNameValuePairValue(manager, autodoc, TILT_RANGE_KEY,
        (String) valueMap.get(TILT_RANGE_KEY), commentMap);
  }

  /**
   * Adds or changes the value of an name/value pair in the file.
   * @param valueMap
   * @param autodoc
   * @param commentMap
   */
  private void setIterationNameValuePairValues(BaseManager manager, final Map valueMap,
      final WritableAutodoc autodoc, final Map commentMap) {
    setNameValuePairValue(manager, autodoc, D_PHI_KEY, (String) valueMap.get(D_PHI_KEY),
        commentMap);
    setNameValuePairValue(manager, autodoc, D_THETA_KEY,
        (String) valueMap.get(D_THETA_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, D_PSI_KEY, (String) valueMap.get(D_PSI_KEY),
        commentMap);
    setNameValuePairValue(manager, autodoc, SEARCH_RADIUS_KEY,
        (String) valueMap.get(SEARCH_RADIUS_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, LOW_CUTOFF_KEY,
        (String) valueMap.get(LOW_CUTOFF_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, HI_CUTOFF_KEY,
        (String) valueMap.get(HI_CUTOFF_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, REF_THRESHOLD_KEY,
        (String) valueMap.get(REF_THRESHOLD_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, DUPLICATE_SHIFT_TOLERANCE_KEY,
        (String) valueMap.get(DUPLICATE_SHIFT_TOLERANCE_KEY), commentMap);
    setNameValuePairValue(manager, autodoc, DUPLICATE_ANGULAR_TOLERANCE_KEY,
        (String) valueMap.get(DUPLICATE_ANGULAR_TOLERANCE_KEY), commentMap);
  }

  /**
   * Gets the attribute.  If the attribute doesn't exist, it adds the attribute.
   * Adds or changes the value of the attribute.
   * @param autodoc
   * @param attributeName
   * @param attributeValue
   */
  private void setNameValuePairValue(BaseManager manager, final WritableAutodoc autodoc,
      final String name, final String value, final Map commentMap) {
    if (value == null) {
      return;
    }
    WritableAttribute attribute = autodoc.getWritableAttribute(name);
    if (attribute == null) {
      if (commentMap == null) {
        // new attribute, so add attribute and name/value pair
        setNameValuePair(manager, autodoc, name, value, (String) null);
      }
      else {
        // new attribute, so add comment, attribute, and name/value pair
        setNameValuePair(manager, autodoc, name, value, (String) commentMap.get(name));
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
  private void setNameValuePair(BaseManager manager, final WritableAutodoc autodoc,
      final String name, final String value, final ReadOnlyAttribute commentAttribute) {
    if (value == null) {
      return;
    }
    if (commentAttribute == null) {
      setNameValuePair(manager, autodoc, name, value, (String) null);
    }
    else {
      setNameValuePair(manager, autodoc, name, value,
          commentAttribute.getMultiLineValue());
    }
  }

  private void removeNameValuePair(final WritableAutodoc autodoc, final String name) {
    WritableStatement previousStatement = autodoc.removeNameValuePair(name);
    // remove the associated comments
    while (previousStatement != null
        && previousStatement.getType() == Statement.Type.COMMENT) {
      previousStatement = autodoc.removeStatement(previousStatement);
    }
    // remove the associated empty line
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
  private void setNameValuePair(BaseManager manager, final WritableAutodoc autodoc,
      final String attributeName, String attributeValue, String comment) {
    WritableAttribute attribute = autodoc.getWritableAttribute(attributeName);
    if (attribute == null) {
      // If the attribute doesn't exist try to add a comment and add the attribute
      if (comment != null) {
        // there's a comment, so add an empty line first
        autodoc.addEmptyLine();
        // Format and add the comment
        String[] commentArray = EtomoAutodoc.format(attributeName + ":\n" + comment);
        for (int i = 0; i < commentArray.length; i++) {
          autodoc.addComment(" " + commentArray[i]);
        }
      }
      // Add the attribute and name/value pair
      autodoc.addNameValuePair(attributeName, attributeValue);
    }
    else {
      // If atttribute does exist, change its value
      attribute.setValue(attributeValue);
    }
  }

  public static final class InitMotlCode implements EnumeratedType {
    public static final InitMotlCode ZERO = new InitMotlCode(0, "Set all angles to 0");
    /**
     * @deprecated convert 1 to 2
     */
    public static final InitMotlCode Z_AXIS = new InitMotlCode(1, null);
    public static final InitMotlCode X_AND_Z_AXIS = new InitMotlCode(2,
        FieldLabels.INIT_MOTL_X_AND_Z_AXIS_LABEL);
    public static final InitMotlCode RANDOM_ROTATIONS = new InitMotlCode(3,
        FieldLabels.INIT_MOTL_RANDOM_ROTATIONS);
    public static final InitMotlCode RANDOM_AXIAL_ROTATIONS = new InitMotlCode(4,
        FieldLabels.INIT_MOTL_RANDOM_AXIAL_ROTATIONS);
    public static final InitMotlCode DEFAULT = ZERO;

    public static final String KEY = "initMOTL";

    private final EtomoNumber value = new EtomoNumber();

    private final String label;

    private InitMotlCode(final int value, final String label) {
      this.label = label;
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

    private static InitMotlCode getInstance(final ReadOnlyAttribute attribute,
        final UIComponent component) {
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
        return X_AND_Z_AXIS;
      }
      if (X_AND_Z_AXIS.value.equals(value)) {
        return X_AND_Z_AXIS;
      }
      if (RANDOM_ROTATIONS.value.equals(value)) {
        return RANDOM_ROTATIONS;
      }
      if (RANDOM_AXIAL_ROTATIONS.value.equals(value)) {
        return RANDOM_AXIAL_ROTATIONS;
      }
      UIHarness.INSTANCE.openProblemValueMessageDialog(component, "Unknown", KEY, null,
          FieldLabels.INIT_MOTL_LABEL, value, DEFAULT.value.toString(), DEFAULT.label);
      return DEFAULT;
    }

    public String getLabel() {
      return label;
    }

    public ConstEtomoNumber getValue() {
      if (this == Z_AXIS) {
        return X_AND_Z_AXIS.getValue();
      }
      return value;
    }
  }

  public static final class MaskType implements EnumeratedType {
    public static final MaskType NONE = new MaskType("none");
    public static final MaskType VOLUME = new MaskType("DUMMY_VOLUME_VALUE");
    public static final MaskType SPHERE = new MaskType("sphere");
    public static final MaskType CYLINDER = new MaskType("cylinder");
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

    public String getLabel() {
      return null;
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
      if (NONE.value.equals(value)) {
        return NONE;
      }
      if (VOLUME.value.equals(value)) {
        return VOLUME;
      }
      if (SPHERE.value.equals(value)) {
        return SPHERE;
      }
      if (CYLINDER.value.equals(value)) {
        return CYLINDER;
      }
      return VOLUME;
    }
  }

  public static final class SampleSphere implements EnumeratedType {
    public static final SampleSphere NONE = new SampleSphere("none");
    public static final SampleSphere FULL = new SampleSphere("full");
    public static final SampleSphere HALF = new SampleSphere("half");
    private static final SampleSphere DEFAULT = NONE;

    private static final String KEY = "sampleSphere";

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

    public String getLabel() {
      return null;
    }

    public boolean isDefault() {
      if (this == DEFAULT) {
        return true;
      }
      return false;
    }

    private static SampleSphere getInstance(final ParsedElement parsedElement,
        final UIComponent component) {
      if (parsedElement.isMissingAttribute()) {
        return DEFAULT;
      }
      String value = parsedElement.getRawString();
      if (value == null) {
        return DEFAULT;
      }
      if (NONE.value.equals(value)) {
        return NONE;
      }
      if (FULL.value.equals(value)) {
        return FULL;
      }
      if (HALF.value.equals(value)) {
        return HALF;
      }
      UIHarness.INSTANCE.openProblemValueMessageDialog(component, "Unknown", KEY, null,
          FieldLabels.SAMPLE_SPHERE_LABEL, value, DEFAULT.value, null);
      return DEFAULT;
    }
  }

  public static final class YAxisType implements EnumeratedType {
    public static final YAxisType Y_AXIS = new YAxisType(new EtomoNumber().set(0),
        FieldLabels.YAXIS_TYPE_Y_AXIS_LABEL);
    public static final YAxisType PARTICLE_MODEL = new YAxisType(
        new EtomoNumber().set(1), FieldLabels.YAXIS_TYPE_PARTICLE_MODEL_LABEL);
    public static final YAxisType CONTOUR = new YAxisType(new EtomoNumber().set(2),
        FieldLabels.YAXIS_TYPE_CONTOUR_LABEL);
    public static final YAxisType DEFAULT = Y_AXIS;

    public static final String KEY = "yaxisType";

    private final ConstEtomoNumber value;
    private final String label;

    private YAxisType(final ConstEtomoNumber value, final String label) {
      this.value = value;
      this.label = label;
    }

    public boolean isDefault() {
      return this == DEFAULT;
    }

    public String getLabel() {
      return label;
    }

    public String toString() {
      return value.toString();
    }

    public ConstEtomoNumber getValue() {
      return null;
    }

    private static YAxisType getInstance(final ReadOnlyAttribute attribute,
        final UIComponent component) {
      if (attribute == null) {
        return DEFAULT;
      }
      String value = attribute.getValue();
      if (value == null) {
        return DEFAULT;
      }
      if (Y_AXIS.value.equals(value)) {
        return Y_AXIS;
      }
      if (PARTICLE_MODEL.value.equals(value)) {
        return PARTICLE_MODEL;
      }
      if (CONTOUR.value.equals(value)) {
        return CONTOUR;
      }
      UIHarness.INSTANCE.openProblemValueMessageDialog(component, "Unknown", KEY, null,
          FieldLabels.YAXIS_TYPE_LABEL, value, DEFAULT.value.toString(), DEFAULT.label);
      return DEFAULT;
    }
  }

  public static final class Volume {
    private static final int START_INDEX = 0;
    private static final int END_INDEX = 1;
    private final ParsedArray tiltRange = ParsedArray.getMatlabInstance(
        EtomoNumber.Type.DOUBLE, TILT_RANGE_KEY);
    /**
     * @deprecated - only for validation
     */
    private final ParsedArray relativeOrient = ParsedArray.getInstance(
        EtomoNumber.Type.DOUBLE, RELATIVE_ORIENT_KEY);
    private final ParsedQuotedString fnVolume = ParsedQuotedString
        .getInstance(FN_VOLUME_KEY);
    private final ParsedQuotedString fnModParticle = ParsedQuotedString
        .getInstance(FN_MOD_PARTICLE_KEY);
    private final ParsedQuotedString initMotl = ParsedQuotedString
        .getInstance(InitMotlCode.KEY);
    private final ParsedQuotedString tiltRangeMultiAxes = ParsedQuotedString
        .getInstance(TILT_RANGE_KEY);

    private final BaseManager manager;
    private final AxisID axisID;

    private Volume(final BaseManager manager, final AxisID axisID) {
      this.manager = manager;
      this.axisID = axisID;
    }

    private boolean validate(final boolean forRun) {
      if (!forRun || relativeOrient.isEmpty()) {
        return true;
      }
      for (int i = 0; i < relativeOrient.size(); i++) {
        if (!relativeOrient.isEmpty(i) && !relativeOrient.getElement(i).equals(0)) {
          UIHarness.INSTANCE.openMessageDialog(manager,
              "Relative orientations are no longer supported in PEET. This "
                  + "functionality is now handled via initial motive lists. If you are "
                  + "already using initial motive list(s), use the PEET program "
                  + "modifyMotiveList to generate new one(s) incorporating the "
                  + "relative orientation rotation(s). If you do not yet have initial "
                  + "motive list(s), use PEET program slicer2MOTL to generate them.",
              "Incompatible .prm File", axisID);
          return false;
        }
      }
      return true;
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

    public String getTiltRangeMultiAxesString() {
      return tiltRangeMultiAxes.getRawString();
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

    public void setTiltRangeMultiAxes(final String input) {
      tiltRangeMultiAxes.setRawString(input);
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

    private ParsedElement getTiltRange(final boolean isTiltRangeMultiAxes) {
      if (!isTiltRangeMultiAxes) {
        return tiltRange;
      }
      else {
        return tiltRangeMultiAxes;
      }
    }

    private void setRelativeOrient(final ParsedElement relativeOrient) {
      this.relativeOrient.set(relativeOrient);
    }

    private void setTiltRange(final ParsedElement tiltRange) {
      this.tiltRange.set(tiltRange);
    }
  }

  /**
   * Class to handle Phi, Theta, Psi.  Sets End to 0 if it is null.  Sets Start
   * to the negation of End.  Sets Increment to 1 if it is 0.
   */
  private static final class SearchAngleArea {
    private final ParsedArrayDescriptor descriptor = ParsedArrayDescriptor.getInstance(
        EtomoNumber.Type.DOUBLE, null);

    private void clear() {
      descriptor.clear();
    }

    /**
     * Sets both the End and Start values.  The Start is set to the negation of
     * the End value.  If End is empty, set it and Start to 0.
     * @param input
     */
    private void setEnd(final String input) {
      EtomoNumber end = new EtomoNumber(EtomoNumber.Type.DOUBLE);
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
      EtomoNumber increment = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      increment.set(input);
      if (increment.equals(0)) {
        increment.set(1);
      }
      descriptor.setRawStringIncrement(increment.toString());
    }

    private String getEnd() {
      return descriptor.getRawStringEnd();
    }

    private void set(final ParsedElement input, final String key, final String label,
        final UIComponent component) {
      descriptor.set(input);
      if (descriptor.validate() == null) {
        checkStart(descriptor.getStart(), descriptor.getEnd(), key, label, component);
      }
    }

    /**
     * Popup a warning if start*-1 != end
     * @param start
     * @param end
     * @param component
     */
    private void checkStart(final ParsedElement start, final ParsedElement end,
        final String key, final String label, final UIComponent component) {
      if (start == null || start.isMissingAttribute() || start.isEmpty() || end == null
          || end.isMissingAttribute() || end.isEmpty()) {
        // ignore validation errors
        return;
      }
      BigDecimal bdStart = new BigDecimal(start.getRawString());
      BigDecimal bdEnd = new BigDecimal(end.getRawString());
      if (bdStart.multiply(new BigDecimal(-1)).compareTo(bdEnd) != 0) {
        UIHarness.INSTANCE.openProblemValueMessageDialog(component, "Incorrect", key,
            "start", label, bdStart.toString(), bdEnd.multiply(new BigDecimal(-1))
                .toString(), "-end");
      }
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

    private final ParsedArray searchRadius = ParsedArray
        .getMatlabInstance(SEARCH_RADIUS_KEY);
    private final ParsedArray lowCutoff = ParsedArray.getMatlabInstance(
        EtomoNumber.Type.DOUBLE, LOW_CUTOFF_KEY);
    private final ParsedArray hiCutoff = ParsedArray.getMatlabInstance(
        EtomoNumber.Type.DOUBLE, HI_CUTOFF_KEY);
    private final ParsedNumber refThreshold = ParsedNumber.getMatlabInstance(
        EtomoNumber.Type.DOUBLE, REF_THRESHOLD_KEY);
    private final ParsedNumber duplicateShiftTolerance = ParsedNumber
        .getMatlabInstance(DUPLICATE_SHIFT_TOLERANCE_KEY);
    private final ParsedNumber duplicateAngularTolerance = ParsedNumber
        .getMatlabInstance(DUPLICATE_ANGULAR_TOLERANCE_KEY);

    // search spaces
    private final SearchAngleArea dPhi = new SearchAngleArea();
    private final SearchAngleArea dTheta = new SearchAngleArea();
    private final SearchAngleArea dPsi = new SearchAngleArea();

    private Iteration() {
    }

    public void clearDPhi() {
      dPhi.clear();
    }

    public void clearDTheta() {
      dTheta.clear();
    }

    public void clearDPsi() {
      dPsi.clear();
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

    public void setDuplicateShiftTolerance(String input) {
      duplicateShiftTolerance.setRawString(input);
    }

    public void setDuplicateAngularTolerance(String input) {
      duplicateAngularTolerance.setRawString(input);
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

    public String getDuplicateShiftToleranceString() {
      return duplicateShiftTolerance.getRawString();
    }

    public String getDuplicateAngularToleranceString() {
      return duplicateAngularTolerance.getRawString();
    }

    private void setDPhi(final ParsedElement input, final UIComponent component) {
      dPhi.set(input, D_PHI_KEY, FieldLabels.D_PHI_LABEL, component);
    }

    private void setDTheta(final ParsedElement input, final UIComponent component) {
      dTheta.set(input, D_THETA_KEY, FieldLabels.D_THETA_LABEL, component);
    }

    private void setDPsi(final ParsedElement input, final UIComponent component) {
      dPsi.set(input, D_PSI_KEY, FieldLabels.D_PSI_LABEL, component);
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

    private void setLowCutoff(final String cutoff, final String sigma) {
      lowCutoff.setRawString(CUTOFF_INDEX, cutoff);
      lowCutoff.setRawString(SIGMA_INDEX, sigma);
    }

    private ParsedElement getLowCutoff() {
      return lowCutoff;
    }

    private ParsedElement getHiCutoff() {
      return hiCutoff;
    }

    private String getLowCutoffCutoffString() {
      if (lowCutoff.isEmpty()) {
        return LOW_CUTOFF_DEFAULT;
      }
      return lowCutoff.getRawString(CUTOFF_INDEX);
    }

    private String getLowCutoffSigmaString() {
      if (lowCutoff.isEmpty() || lowCutoff.isEmpty(SIGMA_INDEX)) {
        return LOW_CUTOFF_SIGMA_DEFAULT;
      }
      return lowCutoff.getRawString(SIGMA_INDEX);
    }

    private ParsedElement getRefThreshold() {
      return refThreshold;
    }

    public ParsedElement getDuplicateShiftTolerance() {
      return duplicateShiftTolerance;
    }

    public ParsedElement getDuplicateAngularTolerance() {
      return duplicateAngularTolerance;
    }

    private void setRefThreshold(final ParsedElement refThreshold) {
      this.refThreshold.setElement(refThreshold);
    }

    private void setDuplicateShiftTolerance(final ParsedElement duplicateShiftTolerance) {
      this.duplicateShiftTolerance.setElement(duplicateShiftTolerance);
    }

    private void setDuplicateAngularTolerance(
        final ParsedElement duplicateAngularTolerance) {
      this.duplicateAngularTolerance.setElement(duplicateAngularTolerance);
    }
  }
}
