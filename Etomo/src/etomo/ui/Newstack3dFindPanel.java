package etomo.ui;

import java.util.Vector;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */
final class Newstack3dFindPanel extends NewstackOrBlendmont3dFindPanel
    implements NewstackDisplay {
  public static final String rcsid = "$Id$";

  private Newstack3dFindPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, NewstackOrBlendmont3dFindParent parent) {
    super(manager, axisID, dialogType, parent);
  }

  static Newstack3dFindPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType,
      NewstackOrBlendmont3dFindParent parent) {
    Newstack3dFindPanel instance = new Newstack3dFindPanel(manager, axisID,
        dialogType, parent);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  public void setParameters(ConstNewstParam param) {
    setBinning(param.getBinByFactor());
  }

  public void getParameters(NewstParam newstParam)
      throws FortranInputSyntaxException {
    newstParam.setCommandMode(NewstParam.Mode.FULL_ALIGNED_STACK);
    newstParam.setFiducialessAlignment(manager.getMetaData()
        .isFiducialessAlignment(axisID));
    int binning = getBinning();
    // Only explicitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    if (binning > 1) {
      newstParam.setBinByFactor(binning);
    }
    else {
      newstParam.setBinByFactor(Integer.MIN_VALUE);
    }
    //Get the rest of the parameters from current state of the final stack
    TomogramState state = manager.getState();
    newstParam.setLinearInterpolation(state
        .isStackUseLinearInterpolation(axisID));
    newstParam.setSizeToOutputInXandY(state
        .getStackUserSizeToOutputInXandY(axisID), getBinning(), manager.getMetaData().getImageRotation(axisID), manager);
    //Set output file because this file was copied from newst.com
    Vector outputFile = new Vector();
    outputFile.add(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(manager,
        axisID));
    newstParam.setOutputFile(outputFile);
    newstParam.setProcessName(ProcessName.NEWST_3D_FIND);
  }

  void runProcess(final ProcessSeries processSeries,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.newst3dFind(null, processSeries, null, axisID, run3dmodMenuOptions,
        dialogType, this);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  void action(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(get3dmodFullButtonActionCommand())) {
      manager.imodFineAlign3dFind(axisID, run3dmodMenuOptions);
    }
  }
}
