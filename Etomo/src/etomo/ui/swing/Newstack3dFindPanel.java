package etomo.ui.swing;

import java.io.IOException;
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
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.util.InvalidParameterException;

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
 * <p> $Log$
 * <p> Revision 1.3  2011/03/02 00:00:12  sueh
 * <p> bug# 1452 Removing image rotation conversion between float and
 * <p> double.  Using string where possible.
 * <p>
 * <p> Revision 1.2  2011/02/22 18:16:11  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.5  2010/04/28 16:42:38  sueh
 * <p> bug# 1344 In getParameters(NewstParam) passing the FileType to
 * <p> setOutputFile.
 * <p>
 * <p> Revision 3.4  2010/04/09 03:01:37  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.3  2009/09/21 17:55:47  sueh
 * <p> bug# 1267 Reformatted.
 * <p>
 * <p> Revision 3.2  2009/09/17 19:12:58  sueh
 * <p> bug# 1257 In NewstParam.setSizeToOutputInXandY forgot to read the
 * <p> header.  Adding read call and throwing InvalidParameterException and
 * <p> IOException.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class Newstack3dFindPanel extends NewstackOrBlendmont3dFindPanel implements
    NewstackDisplay {
  public static final String rcsid = "$Id$";

  private Newstack3dFindPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, NewstackOrBlendmont3dFindParent parent) {
    super(manager, axisID, dialogType, parent);
  }

  static Newstack3dFindPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, NewstackOrBlendmont3dFindParent parent) {
    Newstack3dFindPanel instance = new Newstack3dFindPanel(manager, axisID, dialogType,
        parent);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  public void setParameters(ConstNewstParam param) {
  }

  public boolean getParameters(NewstParam newstParam,final boolean doValidation) throws FortranInputSyntaxException,
      InvalidParameterException, IOException {
    newstParam.setCommandMode(NewstParam.Mode.FULL_ALIGNED_STACK);
    newstParam.setFiducialessAlignment(manager.getMetaData().isFiducialessAlignment(
        axisID));
    int binning = getBinning();
    // Only explicitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script
    if (binning > 1) {
      newstParam.setBinByFactor(binning);
    }
    else {
      newstParam.setBinByFactor(Integer.MIN_VALUE);
    }
    // Get the rest of the parameters from current state of the final stack
    TomogramState state = manager.getState();
    newstParam.setLinearInterpolation(state.isStackUseLinearInterpolation(axisID));
    // State values should be valid - opt out of validate
    newstParam.setSizeToOutputInXandY(state.getStackUserSizeToOutputInXandY(axisID),
        getBinning(), manager.getMetaData().getImageRotation(axisID).getDouble(), null);
    // Set output file because this file was copied from newst.com
    Vector outputFile = new Vector();
    outputFile.add(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(manager, axisID));
    newstParam.setOutputFile(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT);
    newstParam.setProcessName(ProcessName.NEWST_3D_FIND);
    return true;
  }

  void runProcess(final ProcessResultDisplay processResultDisplay,
      final ProcessSeries processSeries, final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.newst3dFind(processResultDisplay, processSeries, null, axisID,
        run3dmodMenuOptions, dialogType, this);
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
  void action(final String command, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(get3dmodFullButtonActionCommand())) {
      manager.imodFineAlign3dFind(axisID, run3dmodMenuOptions);
    }
  }
}
