package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstFindBeads3dParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.type.ViewType;

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
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:02:18  sueh
 * <p> bug# 1420 Getting rid of some of the panel parents by handling common
 * <p> needs with generic interfaces:  ParallelProcessEnabledDialog.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.7  2010/10/11 20:37:27  sueh
 * <p> bug# 1379 Implemented ContextMenu.
 * <p>
 * <p> Revision 1.6  2010/03/27 04:59:52  sueh
 * <p> Reformatted.
 * <p>
 * <p> Revision 1.5  2010/03/19 02:39:28  sueh
 * <p> bug# 1325 Added setParameters(ConstTiltalignParam,boolean).  Changed
 * <p> a tool tip.
 * <p>
 * <p> Revision 1.4  2010/03/12 04:11:06  sueh
 * <p> bug# 1325 Made the erase gold tab label available to the package.
 * <p>
 * <p> Revision 1.3  2009/09/22 23:54:41  sueh
 * <p> bug# 1269 Added setEnabledTiltParameters.
 * <p>
 * <p> Revision 1.2  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class EraseGoldPanel implements ContextMenu {
  public static final String rcsid = "$Id$";

  static final String ERASE_GOLD_TAB_LABEL = "Erase Gold";

  private final JPanel pnlRoot = new JPanel();
  private final EraseGoldPanelActionListener actionListener = new EraseGoldPanelActionListener(
      this);
  private final ButtonGroup bgModel = new ButtonGroup();
  private final RadioButton rbModelUseFid = new RadioButton(
      "Use the existing fiducial model", bgModel);
  private final RadioButton rbModelUseFindBeads3d = new RadioButton("Use findbeads3d",
      bgModel);

  private final XfModelPanel xfModelPanel;
  private final Beads3dFindPanel beads3dFindPanel;
  private final CcdEraserBeadsPanel ccdEraserBeadsPanel;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;

  private EraseGoldPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    xfModelPanel = XfModelPanel.getInstance(manager, axisID, dialogType);
    beads3dFindPanel = Beads3dFindPanel.getInstance(manager, axisID, dialogType,
        globalAdvancedButton);
    ccdEraserBeadsPanel = CcdEraserBeadsPanel.getInstance(manager, axisID, dialogType);
  }

  static EraseGoldPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    EraseGoldPanel instance = new EraseGoldPanel(manager, axisID, dialogType,
        globalAdvancedButton);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  private void addListeners() {
    pnlRoot.addMouseListener(new GenericMouseAdapter(this));
    rbModelUseFid.addActionListener(actionListener);
    rbModelUseFindBeads3d.addActionListener(actionListener);
  }

  void registerProcessingMethodMediator() {
    beads3dFindPanel.registerProcessingMethodMediator();
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
    String[] manPagelabel = { alignManpageLabel, "Tilt", "Findbeads3d", "CcdEraser",
        "3dmod" };
    String[] manPage = { alignManpage + ".html", "tilt.html", "findbeads3d.html",
        "ccderaser.html", "3dmod.html" };
    String[] logFileLabel = { alignLogfileLabel + "_3dfind", "Tilt_3dfind",
        "Findbeads3d", };
    String[] logFile = new String[3];
    logFile[0] = alignLogfile + "_3dfind" + axisID.getExtension() + ".log";
    logFile[1] = "tilt_3dfind" + axisID.getExtension() + ".log";
    logFile[2] = "findbeads3d" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent, "ErasingGold",
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
        axisID);
  }

  private void createPanel() {
    // Local panels
    JPanel pnlModel = new JPanel();
    // Root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new EtchedBorder("Bead Eraser").getBorder());
    pnlRoot.add(pnlModel);
    pnlRoot.add(xfModelPanel.getComponent());
    pnlRoot.add(beads3dFindPanel.getComponent());
    pnlRoot.add(ccdEraserBeadsPanel.getComponent());
    // Model panel
    pnlModel.setLayout(new BoxLayout(pnlModel, BoxLayout.Y_AXIS));
    pnlModel.setBorder(new EtchedBorder("Model Creation Method").getBorder());
    pnlModel.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlModel.add(rbModelUseFid.getComponent());
    pnlModel.add(rbModelUseFindBeads3d.getComponent());
  }

  Component getComponent() {
    return pnlRoot;
  }

  void initializeBeads() {
    ccdEraserBeadsPanel.initialize();
  }

  void updateAdvanced(boolean advanced) {
    beads3dFindPanel.updateAdvanced(advanced);
  }

  BlendmontDisplay getBlendmont3dFindDisplay() {
    return beads3dFindPanel.getBlendmont3dFindDisplay();
  }

  NewstackDisplay getNewstack3dFindDisplay() {
    return beads3dFindPanel.getNewstack3dFindDisplay();
  }

  TiltDisplay getTilt3dFindDisplay() {
    return beads3dFindPanel.getTilt3dFindDisplay();
  }

  FindBeads3dDisplay getFindBeads3dDisplay() {
    return beads3dFindPanel.getFindBeads3dDisplay();
  }

  CcdEraserDisplay getCcdEraserBeadsDisplay() {
    return ccdEraserBeadsPanel;
  }

  void done() {
    xfModelPanel.done();
    beads3dFindPanel.done();
    ccdEraserBeadsPanel.done();
  }

  private void updateDisplay() {
    boolean modelUseFidIsSelected = rbModelUseFid.isSelected();
    xfModelPanel.setVisible(modelUseFidIsSelected);
    beads3dFindPanel.setVisible(!modelUseFidIsSelected);
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    ccdEraserBeadsPanel.getParameters(metaData);
    metaData.setEraseGoldModelUseFid(axisID, rbModelUseFid.isSelected());
    beads3dFindPanel.getParameters(metaData);
  }

  void setParameters(ReconScreenState screenState) {
    beads3dFindPanel.setParameters(screenState);
  }

  void getParameters(ReconScreenState screenState) {
    beads3dFindPanel.getParameters(screenState);
  }

  public void setTiltState(TomogramState state, ConstMetaData metaData) {
    beads3dFindPanel.setTiltState(state, metaData);
  }

  void setParameters(BlendmontParam param) {
    beads3dFindPanel.setParameters(param);
  }

  void setParameters(NewstParam param) {
    beads3dFindPanel.setParameters(param);
  }

  void setParameters(ConstTiltParam param, boolean initialize)
      throws FileNotFoundException, IOException, LogFile.LockException {
    beads3dFindPanel.setParameters(param, initialize);
  }

  void setParameters(ConstMetaData metaData) {
    ccdEraserBeadsPanel.setParameters(metaData);
    if (metaData.getEraseGoldModelUseFid(axisID)) {
      rbModelUseFid.setSelected(true);
    }
    else {
      rbModelUseFindBeads3d.setSelected(true);
    }
    beads3dFindPanel.setParameters(metaData);
    updateDisplay();
  }

  void setParameters(ConstFindBeads3dParam param, boolean initialize) {
    beads3dFindPanel.setParameters(param, initialize);
  }

  void initialize() {
    beads3dFindPanel.initialize();
  }

  void setParameters(ConstTiltalignParam param, boolean initialize) {
    beads3dFindPanel.setParameters(param, initialize);
  }

  void setOverrideParameters(final ConstMetaData metaData) {
    beads3dFindPanel.setOverrideParameters(metaData);
  }

  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(rbModelUseFid.getActionCommand())
        || command.equals(rbModelUseFindBeads3d.getActionCommand())) {
      updateDisplay();
    }
  }

  private void setToolTipText() {
    rbModelUseFid.setToolTipText("Erase the fiducials selected in the fiducial model.");
    rbModelUseFindBeads3d.setToolTipText("Find beads in tomogram and project positions.");
  }

  private final class EraseGoldPanelActionListener implements ActionListener {
    private final EraseGoldPanel adaptee;

    private EraseGoldPanelActionListener(final EraseGoldPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
