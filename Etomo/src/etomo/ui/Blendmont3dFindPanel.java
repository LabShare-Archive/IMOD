package etomo.ui;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.BlendmontParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.AxisID;
import etomo.type.DialogType;
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
 * <p> Revision 3.2  2009/09/21 17:54:13  sueh
 * <p> bug# 1267 Using the blend_3dfind mode instead of the blend mode for
 * <p> 3dfind.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class Blendmont3dFindPanel extends NewstackOrBlendmont3dFindPanel
    implements BlendmontDisplay {
  public static final String rcsid = "$Id$";

  private Blendmont3dFindPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, NewstackOrBlendmont3dFindParent parent) {
    super(manager, axisID, dialogType, parent);
  }

  static Blendmont3dFindPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType,
      NewstackOrBlendmont3dFindParent parent) {
    Blendmont3dFindPanel instance = new Blendmont3dFindPanel(manager, axisID,
        dialogType, parent);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  public void setParameters(BlendmontParam param) {
  }

  public void getParameters(BlendmontParam param)
      throws FortranInputSyntaxException, InvalidParameterException,
      IOException {
    param.setBinByFactor(getBinning());
    param.setMode(BlendmontParam.Mode.BLEND_3DFIND);
    param.convertToStartingAndEndingXandY(manager.getState()
        .getStackUserSizeToOutputInXandY(axisID), manager.getMetaData()
        .getImageRotation(axisID));
  }

  void runProcess(final ProcessSeries processSeries,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.blend3dFind(null, processSeries, null, axisID, run3dmodMenuOptions,
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
