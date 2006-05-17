package etomo.type;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TomopitchParam;
import etomo.comscript.TrimvolParam;
import etomo.util.MRCHeader;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.17  2006/05/11 19:59:04  sueh
 * <p> bug# 838 Added angle offset, z shift, and x axis tilt.
 * <p>
 * <p> Revision 1.16  2006/03/20 18:00:42  sueh
 * <p> bug# 835 Added getName (a convenience function) to managers.
 * <p>
 * <p> Revision 1.15  2005/08/24 22:40:18  sueh
 * <p> bug# 715 Added invalidEdgeFunctionsA and B.
 * <p>
 * <p> Revision 1.14  2005/07/29 00:53:47  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.13  2005/07/20 17:53:43  sueh
 * <p> bug# 705 Stop printing the stack trace for IOException bugs coming from
 * <p> MRCHeader, because its filling up the error log with exceptions that are
 * <p> related to real problems.
 * <p>
 * <p> Revision 1.12  2005/06/20 16:54:26  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in setBackwardCompatibleTrimvolFlipped().
 * <p>
 * <p> Revision 1.11  2005/06/11 02:43:22  sueh
 * <p> Changed initialize() to not require a parameter.
 * <p>
 * <p> Revision 1.10  2005/04/25 20:51:43  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.9  2005/02/17 19:27:05  sueh
 * <p> bug# 606 Removed makeZFactors, newstFiducialessAlignment, and
 * <p> usedLocalAlignments.  Add makeZFactors, newstFiducialessAlignment,
 * <p> and usedLocalAlignments for A and B.
 * <p>
 * <p> Revision 1.8  2005/01/12 00:44:36  sueh
 * <p> bug# 579 Adding usedLocalAlignments.
 * <p>
 * <p> Revision 1.7  2005/01/10 23:54:23  sueh
 * <p> bug# 578 Switched member variables from EtomoBoolean to EtomoState.
 * <p> Added initialize(int) to initialize EtomoState variables to
 * <p> NO_RESULTS_VALUE.  Removed backward compatibility function for
 * <p> newstFiducialAlignment.  EtomoState variables are necessary when the
 * <p> state information was not saved in the past.
 * <p>
 * <p> Revision 1.6  2005/01/08 01:54:43  sueh
 * <p> bug# 578 Removed alignSkewOption and alignXStretchOption.  Added
 * <p> madeZFactors and newstFiducialAlignment.  Added
 * <p> getBackwordCompatible functions for madeZFactors and
 * <p> newstFiducialessAlignment.
 * <p>
 * <p> Revision 1.5  2005/01/06 18:19:05  sueh
 * <p> bug# 578 added alignSkewOption and alignXStretchOption.
 * <p>
 * <p> Revision 1.4  2004/12/16 02:31:24  sueh
 * <p> bug# 564 Manage trimvol flipped state and squeezevol flipped state
 * <p> separately.  If trimvol flipped state changes and squeezevol is not rerun,
 * <p> the squeezevol parameters with still load onto the screen correctly.
 * <p>
 * <p> Revision 1.3  2004/12/14 21:49:04  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 1.2  2004/12/08 21:32:04  sueh
 * <p> bug# 564 Added access to flipped.
 * <p>
 * <p> Revision 1.1  2004/12/07 22:54:07  sueh
 * <p> bug# 564 Contains state variables to be saved in the .edf file.
 * <p> </p>
 */
public class TomogramState implements BaseState {
  public static final String rcsid = "$Id$";

  private static final String groupString = "ReconstructionState";

  EtomoState trimvolFlipped = new EtomoState("TrimvolFlipped");
  EtomoState squeezevolFlipped = new EtomoState("SqueezevolFlipped");
  EtomoState madeZFactorsA = new EtomoState("MadeZFactorsA");
  EtomoState madeZFactorsB = new EtomoState("MadeZFactorsB");
  EtomoState newstFiducialessAlignmentA = new EtomoState(
      "NewstFiducialessAlignmentA");
  EtomoState newstFiducialessAlignmentB = new EtomoState(
      "NewstFiducialessAlignmentB");
  EtomoState usedLocalAlignmentsA = new EtomoState("UsedLocalAlignmentsA");
  EtomoState usedLocalAlignmentsB = new EtomoState("UsedLocalAlignmentsB");
  EtomoState invalidEdgeFunctionsA = new EtomoState("InvalidEdgeFunctionsA");
  EtomoState invalidEdgeFunctionsB = new EtomoState("InvalidEdgeFunctionsB");
  private final EtomoNumber angleOffsetA = new EtomoNumber(
      EtomoNumber.DOUBLE_TYPE, AxisID.FIRST.getExtension() + '.'
          + ProcessName.ALIGN + '.' + ConstTiltalignParam.ANGLE_OFFSET_KEY);
  private final EtomoNumber angleOffsetB = new EtomoNumber(
      EtomoNumber.DOUBLE_TYPE, AxisID.SECOND.getExtension() + '.'
          + ProcessName.ALIGN + '.' + ConstTiltalignParam.ANGLE_OFFSET_KEY);
  private final EtomoNumber axisZShiftA = new EtomoNumber(
      EtomoNumber.DOUBLE_TYPE, AxisID.FIRST.getExtension() + '.'
          + ProcessName.ALIGN + '.' + ConstTiltalignParam.AXIS_Z_SHIFT_KEY);
  private final EtomoNumber axisZShiftB = new EtomoNumber(
      EtomoNumber.DOUBLE_TYPE, AxisID.SECOND.getExtension() + '.'
          + ProcessName.ALIGN + '.' + ConstTiltalignParam.AXIS_Z_SHIFT_KEY);
  private final EtomoNumber xAxisTiltA = new EtomoNumber(
      EtomoNumber.DOUBLE_TYPE, AxisID.FIRST.getExtension() + '.'
          + ProcessName.TILT + '.' + ConstTiltParam.X_AXIS_TILT_KEY);
  private final EtomoNumber xAxisTiltB = new EtomoNumber(
      EtomoNumber.DOUBLE_TYPE, AxisID.SECOND.getExtension() + '.'
          + ProcessName.TILT + '.' + ConstTiltParam.X_AXIS_TILT_KEY);

  private final BaseManager manager;

  public TomogramState(BaseManager manager) {
    this.manager = manager;
    reset();
  }

  public void getParameters(AxisID axisID, TomopitchParam param) {
    if (axisID == AxisID.SECOND) {
      param.setAngleOffsetOld(angleOffsetB);
      param.setZShiftOld(axisZShiftB);
      param.setXAxisTiltOld(xAxisTiltB);
    }
    else {
      param.setAngleOffsetOld(angleOffsetA);
      param.setZShiftOld(axisZShiftA);
      param.setXAxisTiltOld(xAxisTiltA);
    }
  }

  private void reset() {
    trimvolFlipped.reset();
    squeezevolFlipped.reset();
    madeZFactorsA.reset();
    madeZFactorsB.reset();
    newstFiducialessAlignmentA.reset();
    newstFiducialessAlignmentB.reset();
    usedLocalAlignmentsA.reset();
    usedLocalAlignmentsB.reset();
    invalidEdgeFunctionsA.reset();
    invalidEdgeFunctionsB.reset();
    axisZShiftA.reset();
    axisZShiftB.reset();
    angleOffsetA.reset();
    angleOffsetB.reset();
    xAxisTiltA.reset();
    xAxisTiltB.reset();
  }

  public void initialize() {
    trimvolFlipped.set(EtomoState.NO_RESULT_VALUE);
    squeezevolFlipped.set(EtomoState.NO_RESULT_VALUE);
    madeZFactorsA.set(EtomoState.NO_RESULT_VALUE);
    madeZFactorsB.set(EtomoState.NO_RESULT_VALUE);
    newstFiducialessAlignmentA.set(EtomoState.NO_RESULT_VALUE);
    newstFiducialessAlignmentB.set(EtomoState.NO_RESULT_VALUE);
    usedLocalAlignmentsA.set(EtomoState.NO_RESULT_VALUE);
    usedLocalAlignmentsB.set(EtomoState.NO_RESULT_VALUE);
    invalidEdgeFunctionsA.set(EtomoState.NO_RESULT_VALUE);
    invalidEdgeFunctionsB.set(EtomoState.NO_RESULT_VALUE);
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    trimvolFlipped.store(props, prepend);
    squeezevolFlipped.store(props, prepend);
    madeZFactorsA.store(props, prepend);
    madeZFactorsB.store(props, prepend);
    newstFiducialessAlignmentA.store(props, prepend);
    newstFiducialessAlignmentB.store(props, prepend);
    usedLocalAlignmentsA.store(props, prepend);
    usedLocalAlignmentsB.store(props, prepend);
    invalidEdgeFunctionsA.store(props, prepend);
    invalidEdgeFunctionsB.store(props, prepend);
    angleOffsetA.store(props,prepend);
    angleOffsetB.store(props,prepend);
    axisZShiftA.store(props,prepend);
    axisZShiftB.store(props,prepend);
    xAxisTiltA.store(props,prepend);
    xAxisTiltB.store(props,prepend);
  }

  public boolean equals(TomogramState that) {
    if (!trimvolFlipped.equals(that.trimvolFlipped)) {
      return false;
    }
    if (!squeezevolFlipped.equals(that.squeezevolFlipped)) {
      return false;
    }
    if (!madeZFactorsA.equals(that.madeZFactorsA)) {
      return false;
    }
    if (!madeZFactorsB.equals(that.madeZFactorsB)) {
      return false;
    }
    if (!newstFiducialessAlignmentA.equals(that.newstFiducialessAlignmentA)) {
      return false;
    }
    if (!newstFiducialessAlignmentB.equals(that.newstFiducialessAlignmentB)) {
      return false;
    }
    if (!usedLocalAlignmentsA.equals(that.usedLocalAlignmentsA)) {
      return false;
    }
    if (!usedLocalAlignmentsB.equals(that.usedLocalAlignmentsB)) {
      return false;
    }
    if (!invalidEdgeFunctionsA.equals(that.invalidEdgeFunctionsA)) {
      return false;
    }
    if (!invalidEdgeFunctionsB.equals(that.invalidEdgeFunctionsB)) {
      return false;
    }
    return true;
  }

  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    trimvolFlipped.load(props, prepend);
    squeezevolFlipped.load(props, prepend);
    madeZFactorsA.load(props, prepend);
    madeZFactorsB.load(props, prepend);
    newstFiducialessAlignmentA.load(props, prepend);
    newstFiducialessAlignmentB.load(props, prepend);
    usedLocalAlignmentsA.load(props, prepend);
    usedLocalAlignmentsB.load(props, prepend);
    invalidEdgeFunctionsA.load(props, prepend);
    invalidEdgeFunctionsB.load(props, prepend);
    angleOffsetA.load(props,prepend);
    angleOffsetB.load(props,prepend);
    axisZShiftA.load(props,prepend);
    axisZShiftB.load(props,prepend);
    xAxisTiltA.load(props,prepend);
    xAxisTiltB.load(props,prepend);
  }

  public ConstEtomoNumber setTrimvolFlipped(boolean trimvolFlipped) {
    return this.trimvolFlipped.set(trimvolFlipped);
  }

  public ConstEtomoNumber setSqueezevolFlipped(boolean squeezevolFlipped) {
    return this.squeezevolFlipped.set(squeezevolFlipped);
  }

  public ConstEtomoNumber setMadeZFactors(AxisID axisID, boolean madeZFactors) {
    if (axisID == AxisID.SECOND) {
      return this.madeZFactorsB.set(madeZFactors);
    }
    return this.madeZFactorsA.set(madeZFactors);
  }

  public void setAlignAxisZShift(AxisID axisID, double axisZShift) {
    if (axisID == AxisID.SECOND) {
      axisZShiftB.set(axisZShift);
    }
    else {
      axisZShiftA.set(axisZShift);
    }
  }

  public void setAlignAngleOffset(AxisID axisID, double angleOffset) {
    if (axisID == AxisID.SECOND) {
      angleOffsetB.set(angleOffset);
    }
    else {
      angleOffsetA.set(angleOffset);
    }
  }
  
  public void setXAxisTilt(AxisID axisID, double xAxisTilt) {
    if (axisID == AxisID.SECOND) {
      xAxisTiltB.set(xAxisTilt);
    }
    else {
      xAxisTiltA.set(xAxisTilt);
    }
  }

  public ConstEtomoNumber setInvalidEdgeFunctions(AxisID axisID,
      boolean invalidEdgeFunctions) {
    if (axisID == AxisID.SECOND) {
      return this.invalidEdgeFunctionsB.set(invalidEdgeFunctions);
    }
    return this.invalidEdgeFunctionsA.set(invalidEdgeFunctions);
  }

  public ConstEtomoNumber setNewstFiducialessAlignment(AxisID axisID,
      boolean newstFiducialessAlignment) {
    if (axisID == AxisID.SECOND) {
      return this.newstFiducialessAlignmentB.set(newstFiducialessAlignment);
    }
    return this.newstFiducialessAlignmentA.set(newstFiducialessAlignment);
  }

  public ConstEtomoNumber setUsedLocalAlignments(AxisID axisID,
      boolean usedLocalAlignments) {
    if (axisID == AxisID.SECOND) {
      return this.usedLocalAlignmentsB.set(usedLocalAlignments);
    }
    return this.usedLocalAlignmentsA.set(usedLocalAlignments);
  }

  public ConstEtomoNumber getTrimvolFlipped() {
    return trimvolFlipped;
  }

  public ConstEtomoNumber getSqueezevolFlipped() {
    return squeezevolFlipped;
  }

  public ConstEtomoNumber getMadeZFactors(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return madeZFactorsB;
    }
    return madeZFactorsA;
  }

  public ConstEtomoNumber getInvalidEdgeFunctions(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return invalidEdgeFunctionsB;
    }
    return invalidEdgeFunctionsA;
  }

  public ConstEtomoNumber getNewstFiducialessAlignment(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return newstFiducialessAlignmentB;
    }
    return newstFiducialessAlignmentA;
  }

  public ConstEtomoNumber getUsedLocalAlignments(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return usedLocalAlignmentsB;
    }
    return usedLocalAlignmentsA;
  }

  /**
   * Backward compatibility
   * function decide whether trimvol is flipped based on the header
   * @return
   */
  public boolean getBackwardCompatibleTrimvolFlipped() {
    //If trimvol has not been run, then assume that the tomogram has not been
    //flipped.
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    String datasetName = manager.getName();
    File trimvolFile = new File(manager.getPropertyUserDir(), TrimvolParam
        .getOutputFileName(datasetName));
    if (!trimvolFile.exists()) {
      return false;
    }
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        trimvolFile.getAbsolutePath(), AxisID.ONLY);
    try {
      header.read();
    }
    catch (IOException e) {
      return false;
    }
    catch (Exception e) {
      e.printStackTrace();
      return false;
    }
    if (header.getNRows() < header.getNSections()) {
      System.err.println("Assuming that " + trimvolFile.getName()
          + " has not been flipped\n"
          + "because the Y is less then Z in the header.");
      return false;
    }
    System.err.println("Assuming that " + trimvolFile.getName()
        + " has been flipped\n"
        + "because the Y is greater or equal to Z in the header.");
    return true;
  }

  /**
   * Backward compatibility
   * function decide whether tiltalign was run with local alignments based
   * file time.
   * @return
   */
  public boolean getBackwardCompatibleUsedLocalAlignments(AxisID axisID) {
    String userDir = manager.getPropertyUserDir();
    String datasetName = manager.getName();
    File localXfFile = new File(userDir, datasetName + axisID.getExtension()
        + "local.xf");
    File transformFile = new File(userDir, datasetName + axisID.getExtension()
        + ".tltxf");
    if (!localXfFile.exists()) {
      System.err.println("Assuming that local alignments where not used "
          + "\nbecause " + localXfFile.getName() + " does not exist.");
      return false;
    }
    if (!transformFile.exists()) {
      System.err.println("Assuming that local alignments where not used "
          + "\nbecause " + transformFile.getName() + " does not exist.");
      return false;
    }
    if (localXfFile.lastModified() < transformFile.lastModified()) {
      System.err.println("Assuming that local alignments where not used "
          + "\nbecause " + localXfFile.getName() + " was modified before "
          + transformFile.getName() + ".");
      return false;
    }
    System.err.println("Assuming that local alignments where used "
        + "\nbecause " + localXfFile.getName() + " was modified after "
        + transformFile.getName() + ".");
    return true;
  }

  /**
   * Backward compatibility
   * function decide whether z factors where made based on the relationship
   * between .zfac file and the .tltxf file
   * @return
   */
  public boolean getBackwardCompatibleMadeZFactors(AxisID axisID) {
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    String userDir = manager.getPropertyUserDir();
    String datasetName = manager.getName();
    File zFactorFile = new File(userDir, TiltalignParam
        .getOutputZFactorFileName(datasetName, axisID));
    File tltxfFile = new File(userDir, datasetName + axisID.getExtension()
        + ".tltxf");
    if (!zFactorFile.exists()) {
      System.err.println("Assuming that madeZFactors is false\n" + "because "
          + zFactorFile.getName() + " does not exist.");
      return false;
    }
    if (!tltxfFile.exists()) {
      System.err.println("Assuming that madeZFactors is false\n" + "because "
          + tltxfFile.getName() + " does not exist.");
      return false;
    }
    if (zFactorFile.lastModified() < tltxfFile.lastModified()) {
      System.err.println("Assuming that madeZFactors is false\n" + "because "
          + zFactorFile.getName() + " is older then " + tltxfFile.getName()
          + ".");

      return false;
    }
    System.err.println("Assuming that madeZFactors is true\n" + "because "
        + zFactorFile.getName() + " was modified after " + tltxfFile.getName()
        + ".");
    return true;
  }

}