package etomo.ui;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.ManagerKey;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.TiltParam;
import etomo.comscript.TiltalignParam;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.util.FidXyz;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

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
 */
public final class UIExpertUtilities {
  public static final String rcsid = "$Id$";

  public static final UIExpertUtilities INSTANCE = new UIExpertUtilities();

  private UIExpertUtilities() {
  }

  /**
   * Gets the binning that can be used to run tilt against a stack (.preali or
   * .ali).  Function calculates the binning from the stack's pixel spacing and
   * the raw stack's pixel spacing.
   * @param axisID
   * @param stackExtension
   * @return
   */
  public long getStackBinning(BaseManager manager, AxisID axisID,
      String stackExtension) {
    return getStackBinning(manager, axisID, stackExtension, false);
  }

  /**
   * Gets the binning that can be used to run tilt against a stack (.preali or
   * .ali).  Function calculates the binning from the stack's pixel spacing and
   * the raw stack's pixel spacing.
   * @param manager
   * @param axisID
   * @param stackExtension
   * @param nullIfFailed
   * @return
   */
  public long getStackBinning(BaseManager manager, AxisID axisID,
      String stackExtension, boolean nullIfFailed) {
    return getStackBinning(manager, axisID, MRCHeader.getInstance(manager,
        axisID, stackExtension, manager.getManagerKey()), false);
  }

  /**
   * Gets the binning that can be used to run tilt against a stack (.preali or
   * .ali).  Function calculates the binning from the stack's pixel spacing and
   * the raw stack's pixel spacing.
   * @param manager
   * @param axisID
   * @param stackFileType
   * @return
   */
  public long getStackBinning(BaseManager manager, AxisID axisID,
      FileType stackFileType) {
    return getStackBinning(manager, axisID, MRCHeader.getInstance(manager
        .getPropertyUserDir(), stackFileType.getFileName(manager, axisID),
        axisID, manager.getManagerKey()), false);
  }

  /**
   * Gets the binning that can be used to run tilt against a stack (.preali,
   * _3dfind.ali, or .ali).  Function calculates the binning from the stack's
   * pixel spacing and the raw stack's pixel spacing.
   * @param manager
   * @param axisID
   * @param file
   * @param nullIfFailed
   * @return
   */
  public long getStackBinningFromFileName(BaseManager manager, AxisID axisID,
      String fileName, boolean nullIfFailed) {
    if (fileName==null||fileName.matches("\\s*")) {
      throw new IllegalStateException("Empty file name");
    }
    return getStackBinning(manager, axisID, MRCHeader.getInstance(manager
        .getPropertyUserDir(), fileName, axisID, manager.getManagerKey()), false);
  }

  /**
   * Gets the binning that can be used to run tilt against a stack (.preali or
   * .ali).  Function calculates the binning from the stack's pixel spacing and
   * the raw stack's pixel spacing.
   * @param axisID
   * @param stackHeader MRCHeader (.ali or .preali)
   * @param nullIfFailed when true returns null on failure, otherwise returns 1
   * on failure.
   * @return
   */
  public long getStackBinning(BaseManager manager, AxisID axisID,
      MRCHeader stackHeader, boolean nullIfFailed) {
    MRCHeader rawstackHeader = MRCHeader.getInstance(manager, axisID, ".st",
        manager.getManagerKey());
    long defaultValue = nullIfFailed ? EtomoNumber.LONG_NULL_VALUE : 1;
    try {
      if (!rawstackHeader.read() || !stackHeader.read()) {
        return defaultValue;
      }
    }
    catch (InvalidParameterException e) {
      //missing file
      e.printStackTrace();
      return defaultValue;
    }
    catch (IOException e) {
      return defaultValue;
    }
    long binning = defaultValue;
    double rawstackXPixelSpacing = rawstackHeader.getXPixelSpacing();
    if (rawstackXPixelSpacing > 0) {
      binning = Math.round(stackHeader.getXPixelSpacing()
          / rawstackXPixelSpacing);
    }
    if (binning != defaultValue && binning < 1) {
      return 1;
    }
    return binning;
  }

  /**
   * Get the fiducialess parameters from the specified dialog and set the
   * metaData and rotationXF script
   * 
   * @param axisID
   * @param dialog
   * @return
   */
  public boolean updateFiducialessParams(ApplicationManager manager,
      FiducialessParams dialog, AxisID axisID) {
    float tiltAxisAngle;
    try {
      return updateFiducialessParams(manager, dialog.getImageRotation(), dialog
          .isFiducialess(), axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tilt axis rotation format error";
      errorMessage[1] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt axis rotation syntax error", axisID, manager.getManagerKey());
      return false;
    }
  }

  /**
   * Set the metaData and rotationXF script.
   * 
   * @param axisID
   * @param dialog
   * @return
   */
  public boolean updateFiducialessParams(ApplicationManager manager,
      float imageRotation, boolean fiducialess, AxisID axisID) {
    float tiltAxisAngle;
    try {
      tiltAxisAngle = imageRotation;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tilt axis rotation format error";
      errorMessage[1] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt axis rotation syntax error", axisID, manager.getManagerKey());
      return false;
    }
    manager.getMetaData().setFiducialessAlignment(axisID, fiducialess);
    manager.getMetaData().setImageRotation(tiltAxisAngle, axisID);
    updateRotationXF(manager.getPropertyUserDir(), tiltAxisAngle, axisID,
        manager.getManagerKey());
    return true;
  }

  /**
   * Write out the rotation transform for the specified axis
   * 
   * @param axisID
   */
  private void updateRotationXF(String propertyUserDir, float angle,
      AxisID axisID, ManagerKey managerKey) {
    //  Open the appropriate rotation file
    String fnRotationXF = propertyUserDir + File.separator + "rotation"
        + axisID.getExtension() + ".xf";
    File rotationXF = new File(fnRotationXF);
    try {
      BufferedWriter out = new BufferedWriter(new FileWriter(rotationXF));
      //  Write out the transform to perform the rotation
      double rads = -1 * angle * Math.PI / 180;
      out.write(String.valueOf(Math.cos(rads)) + "   "
          + String.valueOf(Math.sin(-rads)) + "   "
          + String.valueOf(Math.sin(rads)) + "   "
          + String.valueOf(Math.cos(rads)) + "   0   0");
      out.newLine();
      //  Close the file
      out.close();
    }
    catch (IOException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Rotation Transform IO Exception";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = fnRotationXF;
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Rotation Transform IO Exception", axisID, managerKey);
    }
  }

  /**
   *  
   */
  public boolean areScriptsCreated(ConstMetaData metaData, AxisID axisID,
      ManagerKey managerKey) {
    if (metaData.getComScriptCreated()) {
      return true;
    }
    String[] message = new String[2];
    message[0] = "The setup process has not been completed";
    message[1] = "Complete the Setup process before opening other process dialogs";
    UIHarness.INSTANCE.openMessageDialog(message, "Program Operation Error",
        axisID, managerKey);
    return false;
  }

  /**
   * If tiltalignParam is an old version, upgrade it to the new version and save
   * it to align.com.
   * @param axisID
   * @param tiltalignParam
   */
  public void upgradeOldAlignCom(ApplicationManager manager, AxisID axisID,
      TiltalignParam tiltalignParam) {
    if (!tiltalignParam.isOldVersion()) {
      return;
    }
    long correctionBinning = getBackwardCompatibleAlignBinning(manager, axisID);
    long currentBinning = getStackBinning(manager, axisID, ".preali");
    if (tiltalignParam.upgradeOldVersion(correctionBinning, currentBinning)) {
      rollAlignComAngles(manager, axisID);
      manager.getComScriptManager().saveAlign(tiltalignParam, axisID);
    }
  }

  /**
   * If tiltParam is an old version, upgrade it to the new version and save it
   * to tilt.com.
   * @param axisID
   * @param tiltParam
   */
  public void upgradeOldTiltCom(ApplicationManager manager, AxisID axisID,
      TiltParam tiltParam) {
    if (!tiltParam.isOldVersion()) {
      return;
    }
    int correctionBinning = getBackwardCompatibleTiltBinning(manager, axisID,
        tiltParam);
    long currentBinning = getStackBinning(manager, axisID, ".ali");
    if (tiltParam.upgradeOldVersion(correctionBinning, currentBinning)) {
      rollTiltComAngles(manager, axisID);
      manager.getComScriptManager().saveTilt(tiltParam, axisID);
      manager.getMetaData().setFiducialess(axisID, tiltParam.isFiducialess());
    }
  }

  /**
   * Gets the binning that can be used to repair a older align.com file.  Older
   * align.com file contain a binned zshift parameter.  And they do not contain
   * the binning.  Function calculates the binning from the raw stack pixel
   * spacing and the fid.xyz pixel spacing (if it exists) or the .preali pixel
   * spacing.  The fid.xyz pixel spacing is more accurate because the file is
   * created by align when align succeeds.  If align fails, then the .preali
   * pixel spacing will be inaccurate if the binning has changed since an
   * the last time the .preali was built.
   * @param axisID
   * @param tiltParam
   * @return
   */
  private long getBackwardCompatibleAlignBinning(ApplicationManager manager,
      AxisID axisID) {
    MRCHeader rawstackHeader = MRCHeader.getInstance(manager, axisID, ".st",
        manager.getManagerKey());
    try {
      if (!rawstackHeader.read()) {
        return 1;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      return 1;
    }
    catch (IOException e) {
      return 1;
    }
    FidXyz fidXyz = getFidXyz(manager, axisID);
    boolean fidXyzFailed = false;
    try {
      fidXyz.read();
    }
    catch (IOException e) {
      e.printStackTrace();
      fidXyzFailed = true;
    }
    if (!fidXyzFailed) {
      //Another layer of backward compatibility.  Handle the time before fid.xyz
      //files where created.  This also handles the situation where align.com has
      //not been run and has the original values from copytomocoms.
      if (!fidXyz.exists()) {
        return 1;
      }
      //Align.com must have failed.  The fallback is to use pixel spacing from
      //.preali.
      if (fidXyz.isEmpty()) {
        fidXyzFailed = true;
      }
      //Another layer of backward compatibility.  There was a small period of
      //time when binning existed but the pixel spacing was not added to fid.xyz
      //(3.2.7 (3/16/04) - 3.2.20 (6/19/04).  If this fid.xyz was created before
      //binning we could return 1, but we don't know that and we can't trust
      //change times on old files that could have been copied, so we need to use
      //the fallback in this case.
      else if (!fidXyz.isPixelSizeSet()) {
        fidXyzFailed = true;
      }
    }
    double rawstackXPixelSpacing = rawstackHeader.getXPixelSpacing();
    //Unable to calculate binning
    if (rawstackXPixelSpacing <= 0) {
      return 1;
    }
    //calculate binning from fid.xyz
    if (!fidXyzFailed) {
      long binning = Math.round(fidXyz.getPixelSize() / rawstackXPixelSpacing);
      if (binning < 1) {
        return 1;
      }
      return binning;
    }
    //fallback to .preali
    MRCHeader stackHeader = MRCHeader.getInstance(manager, axisID, ".preali",
        manager.getManagerKey());
    try {
      if (!stackHeader.read()) {
        return 1;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      return 1;
    }
    catch (IOException e) {
      e.printStackTrace();
      return 1;
    }
    long binning = Math.round(stackHeader.getXPixelSpacing()
        / rawstackXPixelSpacing);
    if (binning < 1) {
      return 1;
    }
    return binning;
  }

  /**
   * Gets the binning that can be used to repair a older tilt.com file.  Older
   * tilt.com file contain parameters, including full image size, which have
   * been binned.  And they do not contain the binning.  Function calculates the
   * binning from the raw stack full image size and the full image size in 
   * tilt.com.
   * @param axisID
   * @param tiltParam
   * @return
   */
  private int getBackwardCompatibleTiltBinning(ApplicationManager manager,
      AxisID axisID, ConstTiltParam tiltParam) {
    MRCHeader rawstackHeader = MRCHeader.getInstance(manager, axisID, ".st",
        manager.getManagerKey());
    try {
      if (!rawstackHeader.read()) {
        return 1;
      }
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      return 1;
    }
    catch (IOException e) {
      return 1;
    }
    int binning = 1;
    int tiltFullImageX = tiltParam.getFullImageX();
    //defaults to Integer.MIN_VALUE in ConstTiltParam
    if (tiltFullImageX > 0) {
      binning = Math.round(rawstackHeader.getNColumns() / tiltFullImageX);
    }
    if (binning < 1) {
      return 1;
    }
    return binning;
  }

  public FidXyz getFidXyz(ApplicationManager manager, AxisID axisID) {
    return new FidXyz(manager.getPropertyUserDir(), manager.getMetaData()
        .getDatasetName()
        + axisID.getExtension() + "fid.xyz");
  }

  public void rollAlignComAngles(ApplicationManager manager, AxisID axisID) {
    TomogramPositioningExpert expert = (TomogramPositioningExpert) manager
        .getUIExpert(DialogType.TOMOGRAM_POSITIONING, axisID);
    if (expert != null) {
      expert.rollAlignComAngles();
    }
  }

  public void rollTiltComAngles(ApplicationManager manager, AxisID axisID) {
    TomogramPositioningExpert expert = (TomogramPositioningExpert) manager
        .getUIExpert(DialogType.TOMOGRAM_POSITIONING, axisID);
    if (expert != null) {
      expert.rollTiltComAngles();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.7  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.6  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.5  2009/02/13 02:38:12  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 1.4  2007/12/13 01:14:48  sueh
 * <p> bug# 1056 Removed the Storables inner class from TiltParam.
 * <p>
 * <p> Revision 1.3  2006/09/19 22:39:13  sueh
 * <p> bug# 920 Refreshing and saving meta data values in TiltParam.
 * <p>
 * <p> Revision 1.2  2006/07/28 20:15:07  sueh
 * <p> bug# 868 Changed isFiduciallessAlignment to isFiducialess
 * <p>
 * <p> Revision 1.1  2006/05/19 19:53:50  sueh
 * <p> bug# 866 Class to contain functionality details shared between dialogs.
 * <p> </p>
 */
