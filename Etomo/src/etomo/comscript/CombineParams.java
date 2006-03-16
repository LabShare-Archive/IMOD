package etomo.comscript;

import java.io.IOException;
import java.util.Properties;

import etomo.BaseManager;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.CombinePatchSize;
import etomo.type.FiducialMatch;
import etomo.type.MatchMode;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

/**
 * <p>Description: </p>
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
 * <p> Revision 3.7  2005/10/19 19:49:29  mast
 * <p> Limited borderindex by the number of xyborder values
 * <p>
 * <p> Revision 3.6  2005/07/29 00:43:05  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.5  2005/06/20 16:40:15  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in setDefaultPatchBoundardies().
 * <p>
 * <p> Revision 3.4  2005/04/25 20:35:19  sueh
 * <p> bug# 615 Passing the axis where the command originated to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.3  2004/06/24 18:32:36  sueh
 * <p> bug# 482 handling USE_MODEL_USE should also cause
 * <p> modelBased to be set to true
 * <p>
 * <p> Revision 3.2  2004/03/22 23:19:30  sueh
 * <p> bug# 250 synchronizing fiducialMatch and modelBased
 * <p>
 * <p> Revision 3.1  2004/03/06 00:25:31  sueh
 * <p> bug# 318 add maxPatchZMax - set, setDefault, load, store
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/10/20 16:51:41  rickg
 * <p> Removed scriptsCreated flag, use existence of combine com scripts instead
 * <p>
 * <p> Revision 2.4  2003/03/18 23:50:08  rickg
 * <p> Added scripts created state variable
 * <p>
 * <p> Revision 2.3  2003/03/18 16:38:04  rickg
 * <p> Added model based boolean
 * <p>
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/02/24 23:28:15  rickg
 * <p> Added default patch region model setter
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.8.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.8  2003/01/06 05:49:40  rickg
 * <p> Fixed fiducial list bug
 * <p>
 * <p> Revision 1.7  2002/10/09 00:01:57  rickg
 * <p> added copy constructor
 * <p> convert whitespace strings to zero length string in set methods
 * <p> added patchBoundaries to load and store
 * <p> added revision number
 * <p>
 * <p> Revision 1.6  2002/10/08 04:39:59  rickg
 * <p> Added setDefaultPatchBoundaryMethod
 * <p>
 * <p> Revision 1.5  2002/10/07 22:23:14  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p> started defaultPatchSize
 * <p>
 * <p> Revision 1.4  2002/10/03 04:00:13  rickg
 * <p> Added path X,Y,Z min and max attributes
 * <p>
 * <p> Revision 1.3  2002/10/01 21:46:35  rickg
 * <p> Implemented load and save methods
 * <p>
 * <p> Revision 1.2  2002/09/30 23:45:12  rickg
 * <p> Reformatted after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class CombineParams extends ConstCombineParams implements Storable {
  public static final String rcsid = "$Id$";

  private static final String MATCH_B_TO_A_KEY = "MatchBtoA";
  private static final String MATCH_MODE_KEY = "MatchMode";
  private static final String DIALOG_MATCH_MODE_KEY = "DialogMatchMode";

  /**
   * Default constructor
   */
  public CombineParams(BaseManager manager) {
    super(manager);
  }

  /**
   * Copy constructor
   */
  public CombineParams(ConstCombineParams src) {
    super(src);
    dialogMatchMode = src.dialogMatchMode;
    matchMode = src.matchMode;
    fiducialMatch = src.fiducialMatch;
    fiducialMatchListA = new StringList(src.fiducialMatchListA);
    fiducialMatchListB = new StringList(src.fiducialMatchListB);
    patchSize = src.patchSize;
    patchXMin = src.patchXMin;
    patchXMax = src.patchXMax;
    patchYMin = src.patchYMin;
    patchYMax = src.patchYMax;
    patchZMin = src.patchZMin;
    patchZMax = src.patchZMax;
    patchRegionModel = src.patchRegionModel;
    tempDirectory = src.tempDirectory;
    manualCleanup = src.manualCleanup;
  }

  public void setRevisionNumber(String revNumber) {
    revisionNumber = revNumber;
  }

  public void setDialogMatchMode(boolean isBtoA) {
    if (isBtoA) {
      dialogMatchMode = MatchMode.B_TO_A;
    }
    else {
      dialogMatchMode = MatchMode.A_TO_B;
    }
  }
  
  public void setMatchMode(MatchMode dialogMatchMode) {
    matchMode = dialogMatchMode;
  }

  public void setFiducialMatch(FiducialMatch match) {
    fiducialMatch = match;
    if (match == FiducialMatch.USE_MODEL
        || match == FiducialMatch.USE_MODEL_ONLY) {
      modelBased = true;
    }
    else {
      modelBased = false;
    }
  }

  public void setFiducialMatchListA(String list) {
    fiducialMatchListA.parseString(list);
  }

  public void setFiducialMatchListB(String list) {
    fiducialMatchListB.parseString(list);
  }

  public void setPatchSize(CombinePatchSize size) {
    patchSize = size;
  }

  public void setPatchRegionModel(String modelFileName) {
    if (modelFileName.matches("^\\s+$")) {
      patchRegionModel = "";
    }
    else {
      patchRegionModel = modelFileName;
    }
  }

  public void setDefaultPatchRegionModel() {
    patchRegionModel = ConstMatchorwarpParam.getDefaultPatchRegionModel();
  }

  /**
   * Sets the patchXMax.
   * @param patchXMax The patchXMax to set
   */
  public void setPatchXMax(int patchXMax) {
    this.patchXMax = patchXMax;
  }

  /**
   * Sets the patchXMin.
   * @param patchXMin The patchXMin to set
   */
  public void setPatchXMin(int patchXMin) {
    this.patchXMin = patchXMin;
  }

  /**
   * Sets the patchYMax.
   * @param patchYMax The patchYMax to set
   */
  public void setPatchYMax(int patchYMax) {
    this.patchYMax = patchYMax;
  }

  /**
   * Sets the patchYMin.
   * @param patchYMin The patchYMin to set
   */
  public void setPatchYMin(int patchYMin) {
    this.patchYMin = patchYMin;
  }

  /**
   * Sets the patchZMax.
   * @param patchZMax The patchZMax to set
   */
  public void setPatchZMax(int patchZMax) {
    this.patchZMax = patchZMax;
  }

  /**
   * Sets the patchZMin.
   * @param patchZMin The patchZMin to set
   */
  public void setPatchZMin(int patchZMin) {
    this.patchZMin = patchZMin;
  }
  

  public void setMaxPatchZMax(String fileName) throws InvalidParameterException,
      IOException {

    // Get the data size limits from the image stack
    MRCHeader mrcHeader = MRCHeader.getInstance(manager.getPropertyUserDir(),
        fileName, AxisID.ONLY);
    mrcHeader.read();
    maxPatchZMax = mrcHeader.getNRows();
  }

  public void setMaxPatchZMax(int maxPatchZMax) {
    this.maxPatchZMax = maxPatchZMax;
  }

  public void setTempDirectory(String directoryName) {
    if (directoryName.matches("^\\s+$")) {
      tempDirectory = "";
    }
    else {
      tempDirectory = directoryName;
    }

  }

  public void setManualCleanup(boolean isManual) {
    manualCleanup = isManual;
  }

  /**
   * Sets the modelBased state.
   * @param modelBased True if a model based combine is being used.
   */
  public void setModelBased(boolean modelBased) {
    this.modelBased = modelBased;
    if (modelBased) {
      fiducialMatch = FiducialMatch.USE_MODEL;
    }
    else {
      fiducialMatch = FiducialMatch.BOTH_SIDES;
    }
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "Combine.";
    }
    else {
      group = prepend + "Combine.";
    }
    props.setProperty(group + "RevisionNumber", revisionNumber);

    //Start backwards compatibility with RevisionNumber = 1.0
    props.remove(group + MATCH_B_TO_A_KEY);
    //End backwards compatibility with RevisionNumber = 1.0

    if (dialogMatchMode == null) {
      props.remove(group + DIALOG_MATCH_MODE_KEY);
    }
    else {
      props.setProperty(group + DIALOG_MATCH_MODE_KEY, dialogMatchMode
          .toString());
    }
    if (matchMode == null) {
      props.remove(group + MATCH_MODE_KEY);
    }
    else {
      props.setProperty(group + MATCH_MODE_KEY, matchMode.toString());
    }
    props.setProperty(group + "FiducialMatch", fiducialMatch.toString());
    props.setProperty(group + "FiducialMatchListA", fiducialMatchListA
        .toString());
    props.setProperty(group + "FiducialMatchListB", fiducialMatchListB
        .toString());
    props.setProperty(group + "PatchSize", patchSize.toString());
    props.setProperty(group + "PatchBoundaryXMin", String.valueOf(patchXMin));
    props.setProperty(group + "PatchBoundaryXMax", String.valueOf(patchXMax));
    props.setProperty(group + "PatchBoundaryYMin", String.valueOf(patchYMin));
    props.setProperty(group + "PatchBoundaryYMax", String.valueOf(patchYMax));
    props.setProperty(group + "PatchBoundaryZMin", String.valueOf(patchZMin));
    props.setProperty(group + "PatchBoundaryZMax", String.valueOf(patchZMax));
    props.setProperty(group + "PatchRegionModel", patchRegionModel);
    props.setProperty(group + "TempDirectory", tempDirectory);
    props.setProperty(group + "ManualCleanup", String.valueOf(manualCleanup));
    props.setProperty(group + "ModelBased", String.valueOf(modelBased));
    props.setProperty(group + "MaxPatchBoundaryZMax", String
        .valueOf(maxPatchZMax));
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "Combine.";
    }
    else {
      group = prepend + "Combine.";
    }

    // Load the combine values if they are present, don't change the
    // current value if the property is not present

    revisionNumber = props.getProperty(group + "RevisionNumber", "1.1");

    //Start backwards compatibility with RevisionNumber = 1.0
    //load dialogMatchMode
    //old property was MatchBtoA.  MatchBtoA should be deleted in store()
    String dialogMatchModeString = props.getProperty(group
        + DIALOG_MATCH_MODE_KEY);
    if (dialogMatchModeString == null) {
      String matchBtoA = props.getProperty(group + MATCH_B_TO_A_KEY);
      if (matchBtoA == null) {
        dialogMatchMode = null;
      }
      else if ((Boolean.valueOf(matchBtoA).booleanValue())) {
        dialogMatchMode = MatchMode.B_TO_A;
      }
      else {
        dialogMatchMode = MatchMode.A_TO_B;
      }
    }
    else {
      MatchMode loadDialogMatchMode = MatchMode
          .getInstance(dialogMatchModeString);
      if (loadDialogMatchMode != null) {
        dialogMatchMode = loadDialogMatchMode;
      }
    }
    //End backwards compatibility with RevisionNumber = 1.0

    if (matchMode == null) {
      matchMode = MatchMode.getInstance(props.getProperty(group
          + MATCH_MODE_KEY));
    }
    else {
      matchMode = MatchMode.getInstance(props.getProperty(group
          + MATCH_MODE_KEY, matchMode.toString()));
    }

    fiducialMatch = FiducialMatch.fromString(props.getProperty(group
        + "FiducialMatch", fiducialMatch.toString()));

    fiducialMatchListA.parseString(props.getProperty(group
        + "FiducialMatchListA", fiducialMatchListA.toString()));

    fiducialMatchListB.parseString(props.getProperty(group
        + "FiducialMatchListB", fiducialMatchListB.toString()));

    patchSize = CombinePatchSize.fromString(props.getProperty(group
        + "PatchSize", patchSize.toString()));

    patchRegionModel = props.getProperty(group + "PatchRegionModel",
        patchRegionModel);

    patchXMin = Integer.parseInt(props.getProperty(group + "PatchBoundaryXMin",
        String.valueOf(patchXMin)));

    patchXMax = Integer.parseInt(props.getProperty(group + "PatchBoundaryXMax",
        String.valueOf(patchXMax)));

    patchYMin = Integer.parseInt(props.getProperty(group + "PatchBoundaryYMin",
        String.valueOf(patchYMin)));

    patchYMax = Integer.parseInt(props.getProperty(group + "PatchBoundaryYMax",
        String.valueOf(patchYMax)));

    patchZMin = Integer.parseInt(props.getProperty(group + "PatchBoundaryZMin",
        String.valueOf(patchZMin)));

    patchZMax = Integer.parseInt(props.getProperty(group + "PatchBoundaryZMax",
        String.valueOf(patchZMax)));

    tempDirectory = props.getProperty(group + "TempDirectory", tempDirectory);

    manualCleanup = Boolean.valueOf(
        props.getProperty(group + "ManualCleanup", Boolean
            .toString(manualCleanup))).booleanValue();

    modelBased = Boolean.valueOf(
        props.getProperty(group + "ModelBased", Boolean.toString(modelBased)))
        .booleanValue();

    if (fiducialMatch == FiducialMatch.USE_MODEL) {
      modelBased = true;
    }
    else {
      modelBased = false;
    }
    maxPatchZMax = Integer.parseInt(props.getProperty(group
        + "MaxPatchBoundaryZMax", String.valueOf(maxPatchZMax)));
  }

  /**
   * Sets the patch boundaries to the default value that matches the logic in
   * the setupcombine script.
   * @param fileName The MRC iamge stack file name used to set the patch
   * boundaries.
   */
  public void setDefaultPatchBoundaries(String fileName)
      throws InvalidParameterException, IOException {

    // Get the data size limits from the image stack
    MRCHeader mrcHeader = MRCHeader.getInstance(manager.getPropertyUserDir(),
        fileName, AxisID.ONLY);
    mrcHeader.read();

    // Logic from setupcombine to provide the default border size, the variable
    // names used match those from the setupcombine script
    int[] xyborders = { 24, 36, 54, 68, 80 };
    int borderinc = 1000;

    //  Assume that Y and Z domains are swapped
    int minsize = Math.min(mrcHeader.getNColumns(), mrcHeader.getNSections());
    int borderindex = (int) (minsize / borderinc);
    if (borderindex > 4)
      borderindex = 4;
    int xyborder = xyborders[borderindex];
    patchXMin = xyborder;
    patchXMax = mrcHeader.getNColumns() - xyborder;
    patchYMin = xyborder;
    patchYMax = mrcHeader.getNSections() - xyborder;
    patchZMin = 1;
    patchZMax = mrcHeader.getNRows();
    maxPatchZMax = patchZMax;
  }
}