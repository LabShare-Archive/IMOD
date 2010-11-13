package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltParam;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;
import etomo.type.TomogramState;
import etomo.type.ViewType;
import etomo.util.InvalidParameterException;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2010</p>
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
public final class BackProjectionPanel implements TiltParent, ContextMenu {
  public static final String rcsid = "$Id$";

  static final String X_AXIS_TILT_TOOLTIP = "This line allows one to rotate the reconstruction around the X axis, so "
      + "that a section that appears to be tilted around the X axis can be "
      + "made flat to fit into a smaller volume.";

  private final JPanel pnlRoot = new JPanel();

  private final ApplicationManager manager;
  private final AxisID axisID;
  private final TiltPanel tiltPanel;

  private BackProjectionPanel(final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType,
      final GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.axisID = axisID;
    tiltPanel = TiltPanel.getInstance(manager, axisID, dialogType, this,
        globalAdvancedButton);
  }

  static BackProjectionPanel getInstance(final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType,
      final GlobalExpandButton globalAdvancedButton) {
    BackProjectionPanel instance = new BackProjectionPanel(manager, axisID,
        dialogType, globalAdvancedButton);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    pnlRoot.setBorder(new BeveledBorder("Back Projection").getBorder());
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlRoot, tiltPanel.getComponent());
    UIUtilities.alignComponentsX(pnlRoot, Component.CENTER_ALIGNMENT);
  }

  Component getRoot() {
    return pnlRoot;
  }

  private void addListeners() {
    pnlRoot.addMouseListener(new GenericMouseAdapter(this));
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String alignManpageLabel;
    String alignManpage;
    String alignLogfileLabel;
    String alignLogfile;
    if (manager.getMetaData().getViewType() == ViewType.MONTAGE) {
      alignManpageLabel = "Blendmont";
      alignManpage = "blendmont";
      alignLogfileLabel = "Blend";
      alignLogfile = "blend";
    }
    else {
      alignManpageLabel = "Newstack";
      alignManpage = "newstack";
      alignLogfileLabel = "Newst";
      alignLogfile = "newst";
    }
    String[] manPagelabel = { alignManpageLabel, "Tilt", "3dmod" };
    String[] manPage = { alignManpage + ".html", "tilt.html", "3dmod.html" };
    String[] logFileLabel = { alignLogfileLabel, "Tilt" };
    String[] logFile = new String[2];
    logFile[0] = alignLogfile + axisID.getExtension() + ".log";
    logFile[1] = "tilt" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent,
        "TOMOGRAM GENERATION", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, manager, axisID);
  }

  static ProcessResultDisplay getUseTrialTomogramResultDisplay() {
    return TrialTiltPanel
        .getUseTrialTomogramResultDisplay(DialogType.TOMOGRAM_GENERATION);
  }

  static ProcessResultDisplay getGenerateTomogramResultDisplay() {
    return TiltPanel
        .getGenerateTomogramResultDisplay(DialogType.TOMOGRAM_GENERATION);
  }

  static ProcessResultDisplay getDeleteAlignedStackResultDisplay() {
    return TiltPanel
        .getDeleteAlignedStackResultDisplay(DialogType.TOMOGRAM_GENERATION);
  }

  /**
   * Update the dialog with the current advanced state
   */
  void updateAdvanced(final boolean advanced) {
    tiltPanel.updateAdvanced(advanced);
  }

  void getParameters(final MetaData metaData)
      throws FortranInputSyntaxException {
    tiltPanel.getParameters(metaData);
  }

  boolean getParameters(final TiltParam tiltParam)
      throws NumberFormatException, InvalidParameterException, IOException {
    return tiltPanel.getParameters(tiltParam);
  }

  void getParameters(final ReconScreenState screenState) {
    tiltPanel.getParameters(screenState);
  }

  void setParameters(final ConstMetaData metaData) {
    tiltPanel.setParameters(metaData);
  }

  /**
   * Set the UI parameters with the specified tiltParam values
   * WARNING: be sure the setNewstParam is called first so the binning value for
   * the stack is known.  The thickness, first and last slice, width and x,y,z
   * offsets are scaled so that they are represented to the user in unbinned
   * dimensions.
   * @param tiltParam
   */
  void setParameters(final ConstTiltParam tiltParam, boolean initialize) {
    tiltPanel.setParameters(tiltParam, initialize);
  }

  void setParameters(final ReconScreenState screenState) {
    tiltPanel.setParameters(screenState);
  }

  boolean usingParallelProcessing() {
    return tiltPanel.usingParallelProcessing();
  }

  void setEnabledTiltParameters(TomogramState state, ConstMetaData metaData) {
    tiltPanel.setEnabledTiltParameters(state, metaData);
  }

  void done() {
    tiltPanel.done();
  }

  TiltDisplay getTiltDisplay() {
    return tiltPanel;
  }

  public void updateParallelProcess() {
    manager.setParallelDialog(axisID, usingParallelProcessing());
  }
}