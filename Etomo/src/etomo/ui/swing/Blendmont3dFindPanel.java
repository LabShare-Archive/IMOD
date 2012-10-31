package etomo.ui.swing;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.BlendmontParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
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
 * <p> Revision 1.2  2011/02/22 18:02:36  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.4  2010/04/09 03:01:26  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.3  2010/04/08 03:06:09  sueh
 * <p> bug# 1347 In setParameters, calling
 * <p> BlendmontParam.convertToStartingAndEndingXandY with
 * <p> TomogramState.stackUserSizeToOutputInXandY.
 * <p>
 * <p> Revision 3.2  2009/09/21 17:54:13  sueh
 * <p> bug# 1267 Using the blend_3dfind mode instead of the blend mode for
 * <p> 3dfind.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class Blendmont3dFindPanel extends NewstackOrBlendmont3dFindPanel implements
    BlendmontDisplay {
  public static final String rcsid = "$Id$";

  private Blendmont3dFindPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, NewstackOrBlendmont3dFindParent parent) {
    super(manager, axisID, dialogType, parent);
  }

  static Blendmont3dFindPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, NewstackOrBlendmont3dFindParent parent) {
    Blendmont3dFindPanel instance = new Blendmont3dFindPanel(manager, axisID, dialogType,
        parent);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  public void setParameters(BlendmontParam param) {
  }

  public boolean getParameters(BlendmontParam param, final boolean doValidation)
      throws FortranInputSyntaxException, InvalidParameterException, IOException {
    param.setBinByFactor(getBinning());
    param.setMode(BlendmontParam.Mode.BLEND_3DFIND);
    // Opt out of validation because state should be correct sinces it's data is from a
    // process that ran.
    param.convertToStartingAndEndingXandY(manager.getState()
        .getStackUserSizeToOutputInXandY(axisID),
        manager.getMetaData().getImageRotation(axisID).getDouble(), null);
    return true;
  }

  void runProcess(final ProcessResultDisplay processResultDisplay,
      final ProcessSeries processSeries, final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.blend3dFind(processResultDisplay, processSeries, null, axisID,
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
