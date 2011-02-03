package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.PanelId;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
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
 * <p> Revision 1.2  2010/12/05 05:20:15  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.  Getting rid of some of the panel parents by handling common needs with generic
 * <p> interfaces:  ParallelProcessEnabledDialog.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.4  2010/04/09 03:02:21  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.3  2010/03/27 05:10:49  sueh
 * <p> bug# 1333 Added panel id.
 * <p>
 * <p> Revision 3.2  2010/03/12 04:27:10  sueh
 * <p> bug# 1325 Changed the label of the delete stacks button.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class TiltPanel extends AbstractTiltPanel {
  public static final String rcsid = "$Id$";

  private final JPanel pnlTiltPanelRoot = new JPanel();

  private TiltPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType,
      final GlobalExpandButton globalAdvancedButton, final PanelId panelId) {
    super(manager, axisID, dialogType, globalAdvancedButton, panelId);
    mediator.register(this);
  }

  static TiltPanel getBackProjectionInstance(final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType,
      final GlobalExpandButton globalAdvancedButton) {
    TiltPanel instance = new TiltPanel(manager, axisID, dialogType,
        globalAdvancedButton, PanelId.TILT);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  static TiltPanel getSirtInstance(final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType,
      GlobalExpandButton globalAdvancedButton) {
    TiltPanel instance = new TiltPanel(manager, axisID, dialogType,
        globalAdvancedButton, PanelId.TILT_SIRT);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  void createPanel() {
    super.createPanel();
    pnlTiltPanelRoot
        .setLayout(new BoxLayout(pnlTiltPanelRoot, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlTiltPanelRoot, super.getRoot());
    UIUtilities.alignComponentsX(pnlTiltPanelRoot, Component.CENTER_ALIGNMENT);
  }

  Component getRoot() {
    return pnlTiltPanelRoot;
  }

  /**
   * Z shift is an advanced field.
   */
  void updateAdvanced(final boolean advanced) {
    super.updateAdvanced(advanced);
    ltfZShift.setVisible(advanced);
  }

  void tiltAction(final ProcessResultDisplay processResultDisplay,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions,
      final ProcessingMethod tiltProcessingMethod) {
    manager.tiltAction(processResultDisplay, null, deferred3dmodButton,
        run3dmodMenuOptions, this, axisID, dialogType, tiltProcessingMethod);
  }

  void imodTomogramAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.imodFullVolume(axisID, run3dmodMenuOptions);
  }

  void popUpContextMenu(final String anchor, final Component rootPanel,
      final MouseEvent mouseEvent) {
    String[] manPagelabel = { "Tilt", "3dmod" };
    String[] manPage = { "tilt.html", "3dmod.html" };
    String[] logFileLabel = { "Tilt" };
    String[] logFile = new String[1];
    logFile[0] = "tilt" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent, anchor,
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile,
        manager, axisID);
  }
}
