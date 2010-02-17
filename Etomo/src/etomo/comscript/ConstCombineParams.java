package etomo.comscript;

import java.io.IOException;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.CombinePatchSize;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.FiducialMatch;
import etomo.type.MatchMode;
import etomo.util.DatasetFiles;
import etomo.util.MRCHeader;

/**
 * <p>Description: A read only model of the parameter interface for the
 * setupcombine script</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.9  2009/03/17 00:31:26  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.8  2009/02/13 02:12:24  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 3.7  2006/07/19 15:15:45  sueh
 * <p> bug# 903 Change patchZMin and Max to EtomoNumbers so they won't generate
 * <p> an exception when they are set to a blank string.
 * <p>
 * <p> Revision 3.6  2006/05/16 21:23:42  sueh
 * <p> bug# 856 Added transfer and useList.  Removed dialogMatchMode from
 * <p> CombineParam.  Letting the screen save the script state, since it already is.
 * <p> Going to revision 1.2.
 * <p>
 * <p> Revision 3.5  2006/03/16 01:48:38  sueh
 * <p> bug# 828 Changed matchBtoA to dialogMatchMode.  Added matchMode.
 * <p> DialogMatchMode reflects the state of the dialog.  MatchMode reflects the
 * <p> state of the script.  MatchMode is set equal to dialogMatchMode when the
 * <p> script is updated.
 * <p>
 * <p> Revision 3.4  2005/07/29 00:44:21  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.3  2005/07/26 17:09:59  sueh
 * <p> bug# 700 Don't respond to MRCHeader exceptions in isValid() because it
 * <p> is run without the user directly requesting it.
 * <p>
 * <p> Revision 3.2  2005/07/20 17:30:52  sueh
 * <p> bug# 700  In isValid() validate the x, y, z values against the tomogram
 * <p> header.  Pass YAndZFlipped.  Get the tomogram file from DatasetFile.
 * <p>
 * <p> Revision 3.1  2004/03/06 00:26:37  sueh
 * <p> bug# 318 add maxPatchZMax - get, use to validate patchZMax
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/10/20 16:51:29  rickg
 * <p> Removed scriptsCreated flag, use existence of combine com scripts instead
 * <p>
 * <p> Revision 2.4  2003/07/28 22:44:56  rickg
 * <p> Added an equals method
 * <p>
 * <p> Revision 2.3  2003/03/18 23:49:40  rickg
 * <p> Added scripts created state variable
 * <p>
 * <p> Revision 2.2  2003/03/18 16:38:18  rickg
 * <p> Added model based boolean
 * <p>
 * <p> Revision 2.1  2003/02/24 23:29:54  rickg
 * <p> Added use patch region model method
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.4  2002/10/09 17:28:37  rickg
 * <p> Fixed javadoc tag
 * <p>
 * <p> Revision 1.3  2002/10/08 23:59:22  rickg
 * <p> Added isPatchBoundarySet method
 * <p> Added basic isValid method
 * <p>
 * <p> Revision 1.2  2002/10/03 03:59:31  rickg
 * <p> Added path X,Y,Z min and max attributes
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ConstCombineParams {
  public static final String rcsid = "$Id$";

  public static final String PATCH_Z_MIN_LABEL = "Z axis min";
  public static final String PATCH_Z_MAX_LABEL = "Z axis max";

  protected String revisionNumber = "1.2";

  //protected MatchMode dialogMatchMode = MatchMode.B_TO_A;
  protected MatchMode matchMode = null;
  protected FiducialMatch fiducialMatch = FiducialMatch.BOTH_SIDES;
  protected StringList useList = new StringList(0);
  protected StringList fiducialMatchListA = new StringList(0);
  protected StringList fiducialMatchListB = new StringList(0);
  protected CombinePatchSize patchSize = CombinePatchSize.MEDIUM;
  protected int patchXMin = 0;
  protected int patchXMax = 0;
  protected int patchYMin = 0;
  protected int patchYMax = 0;
  protected EtomoNumber patchZMin = new EtomoNumber("PatchBoundaryZMin");
  protected EtomoNumber patchZMax = new EtomoNumber("PatchBoundaryZMax");
  protected int maxPatchZMax = 0;

  protected String patchRegionModel = "";
  protected String tempDirectory = "";
  protected boolean manualCleanup = false;
  protected boolean modelBased = false;
  protected boolean transfer = true;

  protected final BaseManager manager;

  protected ArrayList invalidReasons = new ArrayList();

  public ConstCombineParams(BaseManager manager) {
    this.manager = manager;
    patchZMin.set(0);
    patchZMax.set(0);
  }

  public ConstCombineParams(ConstCombineParams src) {
    manager = src.manager;
    patchZMin.set(0);
    patchZMax.set(0);
  }

  public boolean equals(ConstCombineParams cmp) {
    //if (dialogMatchMode != cmp.dialogMatchMode) {
    //  return false;
    //}
    if (matchMode != cmp.matchMode) {
      return false;
    }
    if (!fiducialMatch.equals(cmp.getFiducialMatch())) {
      return false;
    }

    if (!useList.toString().equals(cmp.getUseList().toString())) {
      return false;
    }
    if (!fiducialMatchListA.toString().equals(
        cmp.getFiducialMatchListA().toString())) {
      return false;
    }
    if (!fiducialMatchListB.toString().equals(
        cmp.getFiducialMatchListB().toString())) {
      return false;
    }
    if (!patchSize.equals(cmp.getPatchSize())) {
      return false;
    }
    if (!(patchXMin == cmp.getPatchXMin())) {
      return false;
    }
    if (!(patchXMax == cmp.getPatchXMax())) {
      return false;
    }
    if (!(patchYMin == cmp.getPatchYMin())) {
      return false;
    }
    if (!(patchYMax == cmp.getPatchYMax())) {
      return false;
    }
    if (!patchZMin.equals(cmp.getPatchZMin())) {
      return false;
    }
    if (!patchZMax.equals(cmp.getPatchZMax())) {
      return false;
    }
    if (!(patchRegionModel.equals(cmp.getPatchRegionModel()))) {
      return false;
    }
    if (!(tempDirectory.equals(cmp.getTempDirectory()))) {
      return false;
    }
    if (!(manualCleanup == cmp.getManualCleanup())) {
      return false;
    }
    if (!(modelBased == cmp.isModelBased())) {
      return false;
    }
    return true;
  }

  /**
   * Returns true if the patch boundary values have been modified
   */
  public boolean isPatchBoundarySet() {
    if (patchXMin == 0 && patchXMax == 0 && patchYMin == 0 && patchYMax == 0
        && patchZMin.equals(0) && patchZMax.equals(0)) {
      return false;
    }
    return true;
  }

  /**
   * Checks the validity of the attribute values.
   * @return true if all entries are valid, otherwise the reasons are 
   * available through the method getInvalidReasons.
   */
  public boolean isValid(boolean YAndZflipped) {
    boolean valid = true;
    //  Clear any previous reasons from the list
    invalidReasons.clear();
    if (patchXMin < 1) {
      valid = false;
      invalidReasons.add("X min value is less than 1");
    }
    if (patchXMax < 1) {
      valid = false;
      invalidReasons.add("X max value is less than 1");
    }
    if (patchXMin > patchXMax) {
      valid = false;
      invalidReasons.add("X min value is greater than the X max value");
    }

    if (patchYMin < 1) {
      valid = false;
      invalidReasons.add("Y min value is less than 1");
    }
    if (patchYMax < 1) {
      valid = false;
      invalidReasons.add("Y max value is less than 1");
    }
    if (patchYMin > patchYMax) {
      valid = false;
      invalidReasons.add("Y min value is greater than the Y max value");
    }

    if (patchZMin.getInt() < 1) {
      valid = false;
      invalidReasons.add("Z min value is less than 1");
    }
    if (patchZMax.getInt() < 1) {
      valid = false;
      invalidReasons.add("ZX max value is less than 1");
    }
    if (maxPatchZMax > 0 && patchZMax.gt(maxPatchZMax)) {
      valid = false;
      invalidReasons
          .add("Z max value is greater than the maximum Z max value ("
              + maxPatchZMax + ")");
    }
    if (patchZMin.gt(patchZMax)) {
      valid = false;
      invalidReasons.add("Z min value is greater than the Z max value");
    }
    //get the tomogram header to check x, y, and z
    AxisID axisID;
    //if (dialogMatchMode == null || dialogMatchMode == MatchMode.B_TO_A) {
    //  axisID = AxisID.FIRST;
    //}
    //else {
    //  axisID = AxisID.SECOND;
    //}
    if (matchMode == null || matchMode == MatchMode.B_TO_A) {
      axisID = AxisID.FIRST;
    }
    else {
      axisID = AxisID.SECOND;
    }
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        DatasetFiles.getTomogram(manager, axisID).getAbsolutePath(), axisID);
    try {
      if (!header.read(manager)) {
        return true;
      }
    }
    catch (IOException e) {
      return true;
    }
    catch (Exception e) {
      e.printStackTrace();
      return true;
    }
    int x = header.getNColumns();
    if (x < patchXMin || x < patchXMax) {
      valid = false;
      invalidReasons.add("X values cannot be greater then " + x);
    }
    int y;
    int z;
    if (YAndZflipped) {
      y = header.getNSections();
      z = header.getNRows();
    }
    else {
      y = header.getNRows();
      z = header.getNSections();
    }
    if (y < patchYMin || y < patchYMax) {
      valid = false;
      invalidReasons.add("Y values cannot be greater then " + y);
    }

    if (patchZMin.gt(z) || patchZMax.gt(z)) {
      valid = false;
      invalidReasons.add("Z values cannot be greater then " + z);
    }
    return valid;
  }

  /**
   * Returns the reasons the attribute values are invalid as a string array.
   */
  public String[] getInvalidReasons() {
    return (String[]) invalidReasons.toArray(new String[invalidReasons.size()]);
  }

  public String getRevisionNumber() {
    return revisionNumber;
  }

  //public MatchMode getDialogMatchMode() {
  //  return dialogMatchMode;
  //}

  public MatchMode getMatchMode() {
    return matchMode;
  }

  public boolean isTransfer() {
    return transfer;
  }

  public FiducialMatch getFiducialMatch() {
    return fiducialMatch;
  }

  public String getUseList() {
    return useList.toString();
  }

  public String getFiducialMatchListA() {
    return fiducialMatchListA.toString();
  }

  public String getFiducialMatchListB() {
    return fiducialMatchListB.toString();
  }

  public CombinePatchSize getPatchSize() {
    return patchSize;
  }

  public String getPatchRegionModel() {
    return patchRegionModel;
  }

  public String getTempDirectory() {
    return tempDirectory;
  }

  public boolean getManualCleanup() {
    return manualCleanup;
  }

  /**
   * Returns the patchXMax.
   * @return int
   */
  public int getPatchXMax() {
    return patchXMax;
  }

  /**
   * Returns the patchXMin.
   * @return int
   */
  public int getPatchXMin() {
    return patchXMin;
  }

  /**
   * Returns the patchYMax.
   * @return int
   */
  public int getPatchYMax() {
    return patchYMax;
  }

  /**
   * Returns the patchYMin.
   * @return int
   */
  public int getPatchYMin() {
    return patchYMin;
  }

  /**
   * Returns the patchZMax.
   * @return int
   */
  public ConstEtomoNumber getPatchZMax() {
    return patchZMax;
  }

  /**
   * Returns the patchZMin.
   * @return int
   */
  public ConstEtomoNumber getPatchZMin() {
    return patchZMin;
  }

  public int getMaxPatchZMax() {
    return maxPatchZMax;
  }

  /**
   * Returns true if a patch region model has been specified.
   * @return boolean
   */
  public boolean usePatchRegionModel() {

    return !patchRegionModel.matches("^\\s*$");
  }

  /**
   * @return boolean
   */
  public boolean isModelBased() {
    return modelBased;
  }
}