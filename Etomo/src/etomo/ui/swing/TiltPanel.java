package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
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
 * <p> Revision 1.6  2011/04/26 00:07:01  sueh
 * <p> bug# 1416 Moved responsibility of handling checkpointing and changed state to this class.  Field level calls
 * <p> still handled in the parent.
 * <p>
 * <p> Revision 1.5  2011/04/04 17:37:14  sueh
 * <p> bug# 1416  Added parent, sirtStartFromPanel, allowTiltComSave.
 * <p>
 * <p> Revision 1.4  2011/02/10 04:32:43  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
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
final class TiltPanel extends AbstractTiltPanel implements ResumeObserver {
  public static final String rcsid = "$Id$";

  private final JPanel pnlTiltPanelRoot = new JPanel();
  private final List<FieldObserver> fieldObservers = new ArrayList();

  private final TomogramGenerationDialog parent;

  private boolean resume = false;

  private TiltPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final PanelId panelId, final TomogramGenerationDialog parent) {
    super(manager, axisID, dialogType, globalAdvancedButton, panelId, true);
    this.parent = parent;
    mediator.register(this);
  }

  static TiltPanel getInstance(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final TomogramGenerationDialog parent) {
    TiltPanel instance = new TiltPanel(manager, axisID, dialogType, globalAdvancedButton,
        PanelId.TILT, parent);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  void addListeners() {
    super.addListeners();
  }

  void createPanel() {
    super.createPanel();
    pnlTiltPanelRoot.setLayout(new BoxLayout(pnlTiltPanelRoot, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlTiltPanelRoot, super.getRoot());
    UIUtilities.alignComponentsX(pnlTiltPanelRoot, Component.CENTER_ALIGNMENT);
  }

  Component getRoot() {
    return pnlTiltPanelRoot;
  }

  public boolean allowTiltComSave() {
    return parent.allowTiltComSave();
  }

  void checkpoint(final ConstTiltParam param) {
    super.checkpoint(param);
    fieldChangeAction();
  }

  boolean isResume() {
    return resume;
  }

  void addFieldObserver(FieldObserver fieldObserver) {
    fieldObservers.add(fieldObserver);
    fieldChangeAction();
  }

  final void fieldChangeAction() {
    boolean diff = isDifferentFromCheckpoint();
    Iterator<FieldObserver> i = fieldObservers.iterator();
    while (i.hasNext()) {
      i.next().msgFieldChanged(diff);
    }
  }

  final boolean isBackProjection() {
    return parent.isBackProjection();
  }

  final boolean isSirt() {
    return parent.isSirt();
  }

  public void msgResumeChanged(boolean resume) {
    this.resume = resume;
    updateDisplay();
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
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
        axisID);
  }
}
