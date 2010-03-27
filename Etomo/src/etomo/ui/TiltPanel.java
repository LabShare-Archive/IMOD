package etomo.ui;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.PanelId;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 3.2  2010/03/12 04:27:10  sueh
 * <p> bug# 1325 Changed the label of the delete stacks button.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class TiltPanel extends AbstractTiltPanel {
  public static final String rcsid = "$Id$";

  //backward compatibility functionality - if the metadata binning is missing
  //get binning from newst
  private TiltPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, TiltParent parent,
      GlobalExpandButton globalAdvancedButton) {
    super(manager, axisID, dialogType, parent, globalAdvancedButton,
        PanelId.TILT);
  }

  static TiltPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, TiltParent parent,
      GlobalExpandButton globalAdvancedButton) {
    TiltPanel instance = new TiltPanel(manager, axisID, dialogType, parent,
        globalAdvancedButton);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  Run3dmodButton getTiltButton(ApplicationManager manager, AxisID axisID) {
    return (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getGenerateTomogram();
  }

  public static ProcessResultDisplay getGenerateTomogramResultDisplay(
      DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Generate Tomogram",
        dialogType);
  }

  public static ProcessResultDisplay getDeleteAlignedStackResultDisplay(
      DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance(
        "Delete Intermediate Image Stacks", dialogType);
  }

  /**
   * Z shift is an advanced field.
   */
  void updateAdvanced(final boolean advanced) {
    super.updateAdvanced(advanced);
    ltfZShift.setVisible(advanced);
  }

  void tiltAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.tiltAction(getTiltProcessResultDisplay(), null,
        deferred3dmodButton, run3dmodMenuOptions, this, axisID, dialogType);
  }

  void imodTomogramAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.imodFullVolume(axisID, run3dmodMenuOptions);
  }
}
