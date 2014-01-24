package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.RunraptorParam;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ViewType;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

/**
 * <p>Description:.</p>
 *
 * <p>Copyright: Copyright (c) 2009</p>
 *
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.4  2011/06/24 17:03:16  sueh
 * <p> Bug# 1466 Added the raptor manpage.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:21:50  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:16:25  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.4  2010/10/11 20:39:57  sueh
 * <p> bug# 1379 Implemented ContextMenu.
 * <p>
 * <p> Revision 1.3  2010/03/12 04:25:16  sueh
 * <p> bug# 1325 n setBeadtrackParams(BeadtrackParams) make sure that bead
 * <p> diameter won't override the metadata value if it is empty.
 * <p>
 * <p> Revision 1.2  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.1  2009/06/10 22:17:31  sueh
 * <p> bug# 1221 Factoring RAPTOR into RaptorPanel.
 * <p>
 */
final class RaptorPanel implements Run3dmodButtonContainer, ContextMenu {
  public static final String rcsid = "$Id$";

  private static final String MARK_LABEL = "# of beads to choose";
  private static final String DIAM_LABEL = "Unbinned Bead diameter";
  static final String RUN_RAPTOR_LABEL = "Run RAPTOR";
  static final String USE_RAPTOR_RESULT_LABEL = "Use RAPTOR Result as Fiducial Model";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final JPanel pnlInput = new JPanel();
  private final Run3dmodButton btnOpenStack = Run3dmodButton.get3dmodInstance(
      "Open Stack in 3dmod", this);
  private final LabeledTextField ltfMark = new LabeledTextField(FieldType.INTEGER,
      MARK_LABEL + ": ");
  private final LabeledTextField ltfDiam = new LabeledTextField(FieldType.INTEGER,
      DIAM_LABEL + " (in pixels): ");
  private final ButtonGroup bgInput = new ButtonGroup();
  private final RadioButton rbInputPreali = new RadioButton(
      "Run against the coarse aligned stack", bgInput);
  private final RadioButton rbInputRaw = new RadioButton("Run against the raw stack",
      bgInput);
  private final Run3dmodButton btnRaptor;
  private final Run3dmodButton btnOpenRaptorResult = Run3dmodButton.get3dmodInstance(
      "Open RAPTOR Model in 3dmod", this);
  private final MultiLineButton btnUseRaptorResult;
  private final RaptorPanelActionListener actionListener = new RaptorPanelActionListener(
      this);

  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;

  private RaptorPanel(ApplicationManager manager, AxisID axisID, DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    ProcessResultDisplayFactory displayFactory = manager
        .getProcessResultDisplayFactory(axisID);
    btnRaptor = (Run3dmodButton) displayFactory.getRaptor();
    btnUseRaptorResult = (MultiLineButton) displayFactory.getUseRaptor();
  }

  static RaptorPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    RaptorPanel instance = new RaptorPanel(manager, axisID, dialogType);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  private void addListeners() {
    pnlRoot.addMouseListener(new GenericMouseAdapter(this));
    btnOpenStack.addActionListener(actionListener);
    btnRaptor.addActionListener(actionListener);
    btnOpenRaptorResult.addActionListener(actionListener);
    btnUseRaptorResult.addActionListener(actionListener);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String[] manPagelabel = { "Raptor", "Beadtrack", "3dmod" };
    String[] manPage = { "raptor.html", "beadtrack.html", "3dmod.html" };

    String[] logFileLabel = { "Track" };
    String[] logFile = new String[1];
    logFile[0] = "track" + axisID.getExtension() + ".log";

    new ContextPopup(pnlRoot.getContainer(), mouseEvent, "UsingRaptor",
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
        axisID);
  }

  private void createPanel() {
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new EtchedBorder(RUN_RAPTOR_LABEL).getBorder());
    pnlRoot.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlRoot.add(pnlInput);
    pnlRoot.add(btnOpenStack.getComponent());
    pnlRoot.add(ltfMark.getContainer());
    pnlRoot.add(ltfDiam.getContainer());
    SpacedPanel pnlRaptorButtons = SpacedPanel.getInstance();
    pnlRoot.add(pnlRaptorButtons);
    // RAPTOR input source panel
    pnlInput.setLayout(new BoxLayout(pnlInput, BoxLayout.Y_AXIS));
    pnlInput.setBorder(BorderFactory.createEtchedBorder());
    pnlInput.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlInput.add(rbInputPreali.getComponent());
    pnlInput.add(rbInputRaw.getComponent());
    // RAPTOR button panel
    pnlRaptorButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlRaptorButtons.add(btnRaptor.getComponent());
    pnlRaptorButtons.add(btnOpenRaptorResult.getComponent());
    pnlRaptorButtons.add(btnUseRaptorResult.getComponent());
    // set initial values
    rbInputPreali.setSelected(true);
    btnRaptor.setSize();
    btnOpenRaptorResult.setSize();
    btnUseRaptorResult.setSize();
    btnOpenStack.setSize();
    btnOpenStack.setAlignmentX(Box.CENTER_ALIGNMENT);
    // raptor button
    btnRaptor.setContainer(this);
    btnRaptor.setDeferred3dmodButton(btnOpenRaptorResult);
  }

  void done() {
    btnRaptor.removeActionListener(actionListener);
    btnOpenRaptorResult.removeActionListener(actionListener);
    btnUseRaptorResult.removeActionListener(actionListener);
  }

  public void setBeadtrackParams(final BeadtrackParam beadtrackParams) {
    if (beadtrackParams.isBeadDiameterSet()) {
      ltfDiam.setText(Math.round(beadtrackParams.getBeadDiameter().getDouble()));
    }
  }

  public boolean getParameters(final RunraptorParam param, final boolean doValidation) {
    try {
      param.setUseRawStack(rbInputRaw.isSelected());
      String errorMessage = param.setMark(ltfMark.getText(doValidation));
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Error in " + MARK_LABEL + ": "
            + errorMessage, "Entry Error", axisID);
        return false;
      }
      errorMessage = param.setDiam(ltfDiam.getText(doValidation),
          rbInputPreali.isSelected());
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Error in " + DIAM_LABEL + ": "
            + errorMessage, "Entry Error", axisID);
        return false;
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public void getParameters(final MetaData metaData) {
    if (axisID != AxisID.SECOND) {
      metaData.setTrackRaptorUseRawStack(rbInputRaw.isSelected());
      metaData.setTrackRaptorMark(ltfMark.getText());
      metaData.setTrackRaptorDiam(ltfDiam.getText());
    }
  }

  public void setParameters(final ConstMetaData metaData) {
    if (axisID != AxisID.SECOND) {
      if (metaData.getTrackRaptorUseRawStack()) {
        rbInputRaw.setSelected(true);
      }
      else {
        rbInputPreali.setSelected(true);
      }
      ltfMark.setText(metaData.getTrackRaptorMark());
      ConstEtomoNumber diam = metaData.getTrackRaptorDiam();
      if (!diam.isNull()) {
        ltfDiam.setText(diam);
      }
    }
    if (manager.getMetaData().getViewType() == ViewType.MONTAGE) {
      rbInputPreali.setSelected(true);
      rbInputRaw.setEnabled(false);
      // pnlInput.setVisible(false);
    }
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnOpenStack.getActionCommand())) {
      if (rbInputRaw.isSelected()) {
        manager.imodRawStack(axisID, run3dmodMenuOptions);
      }
      else {
        manager.imodCoarseAlign(axisID, run3dmodMenuOptions, null, false);
      }
    }
    else if (command.equals(btnRaptor.getActionCommand())) {
      manager.runraptor(btnRaptor, null, deferred3dmodButton, run3dmodMenuOptions,
          DialogType.FIDUCIAL_MODEL, axisID);
    }
    else if (command.equals(btnOpenRaptorResult.getActionCommand())) {
      manager.imodRunraptorResult(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnUseRaptorResult.getActionCommand())) {
      manager.useRunraptorResult(btnUseRaptorResult, axisID, DialogType.FIDUCIAL_MODEL);
    }
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void setVisible(boolean visible) {
    pnlRoot.setVisible(visible);
  }

  private void setToolTipText() {
    rbInputPreali.setToolTipText("Run RAPTOR against the coarsely aligned stack.");
    rbInputRaw.setToolTipText("Run RAPTOR against the raw stack.");
    btnOpenStack.setToolTipText("Opens the file that RAPTOR will be run against.");
    ltfMark.setToolTipText("Number of markers to track.");
    ltfDiam.setToolTipText("Bead diameter in pixels.");
    btnRaptor.setToolTipText("Runs the runraptor script");
    btnOpenRaptorResult
        .setToolTipText("Opens the model generated by RAPTOR and the file that RAPTOR was run against.");
    btnUseRaptorResult
        .setToolTipText("Copies the model generated by RAPTOR to the .fid file.");
  }

  private final class RaptorPanelActionListener implements ActionListener {
    private final RaptorPanel adaptee;

    private RaptorPanelActionListener(final RaptorPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
