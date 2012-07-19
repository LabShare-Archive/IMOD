package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

import etomo.AutoAlignmentController;
import etomo.BaseManager;
import etomo.type.AutoAlignmentMetaData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class AutoAlignmentPanel {
  public static final String rcsid = "$Id:$";

  private final SpacedPanel pnlRoot = SpacedPanel.getFocusableInstance();
  private final SpacedPanel pnlParameters = SpacedPanel.getInstance();
  private final LabeledTextField ltfSigmaLowFrequency = new LabeledTextField(
      "Sigma for low-frequency filter: ");
  private final LabeledTextField ltfCutoffHighFrequency = new LabeledTextField(
      "Cutoff for high-frequency filter: ");
  private final LabeledTextField ltfSigmaHighFrequency = new LabeledTextField(
      "Sigma for high-frequency filter: ");
  private final TransformChooserPanel tcAlign = new TransformChooserPanel();
  private final SpacedPanel pnlButtons = SpacedPanel.getInstance();
  private final MultiLineButton btnInitialAutoAlignment = new MultiLineButton(
      "Initial Auto Alignment");
  private final MultiLineButton btnMidas = new MultiLineButton("Midas");
  private final MultiLineButton btnRefineAutoAlignment = new MultiLineButton(
      "Refine Auto Alignment");
  private final MultiLineButton btnRevertToMidas = new MultiLineButton(
      "Revert Auto Alignment to Midas");
  private final MultiLineButton btnRevertToEmpty = new MultiLineButton(
      "Revert to No Transforms");
  private final LabeledSpinner spBinning = new LabeledSpinner("Binning: ",
      new SpinnerNumberModel(2, 1, 50, 1), 1);
  private final LabeledTextField ltfSkipSectionsFrom1 = new LabeledTextField(
      "Sections to skip: ");
  private final CheckBox cbPreCrossCorrelation = new CheckBox(
      "Do cross-correlate in initial alignment");
  private final LabeledTextField ltfEdgeToIgnore = new LabeledTextField(
      "Fraction to ignore on edges: ");
  private final Spinner spMidasBinning = Spinner.getLabeledInstance("Midas binning: ", 1,
      1, 8);

  private final BaseManager manager;
  private final boolean tomogramAverages;

  private AutoAlignmentController controller = null;

  private AutoAlignmentPanel(final BaseManager manager, final boolean tomogramAverages) {
    this.manager = manager;
    this.tomogramAverages = tomogramAverages;
  }

  static AutoAlignmentPanel getJoinInstance(final BaseManager manager) {
    AutoAlignmentPanel instance = new AutoAlignmentPanel(manager, true);
    instance.createPanel(true);
    instance.setTooltips();
    return instance;
  }

  static AutoAlignmentPanel getSerialSectionsInstance(final BaseManager manager) {
    AutoAlignmentPanel instance = new AutoAlignmentPanel(manager, false);
    instance.createPanel(false);
    instance.setTooltips();
    return instance;
  }

  private void createPanel(final boolean joinConfiguration) {
    // panels
    JPanel pnlPreCrossCorrelation = new JPanel();
    JPanel pnlBinning = new JPanel();
    SpacedPanel pnlLeftButtons = SpacedPanel.getInstance();
    SpacedPanel pnlRightButtons = SpacedPanel.getInstance();
    // init
    if (joinConfiguration) {
      spBinning.setVisible(false);
      ltfSkipSectionsFrom1.setVisible(false);
      cbPreCrossCorrelation.setVisible(false);
      ltfEdgeToIgnore.setVisible(false);
      spMidasBinning.setVisible(false);
    }
    ltfEdgeToIgnore.setText(".05");
    btnInitialAutoAlignment.setSize();
    btnMidas.setSize();
    btnRefineAutoAlignment.setSize();
    btnRevertToMidas.setSize();
    btnRevertToEmpty.setSize();
    // root
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.add(pnlParameters.getContainer());
    pnlRoot.add(pnlButtons.getContainer());
    // parameters
    pnlParameters.setBoxLayout(BoxLayout.Y_AXIS);
    pnlParameters.setBorder(new EtchedBorder("Auto Alignment Parameters").getBorder());
    pnlParameters.add(ltfSigmaLowFrequency);
    pnlParameters.add(ltfCutoffHighFrequency);
    pnlParameters.add(ltfSigmaHighFrequency);
    pnlParameters.add(tcAlign.getContainer());
    pnlParameters.add(pnlPreCrossCorrelation);
    pnlParameters.add(ltfSkipSectionsFrom1.getContainer());
    pnlParameters.add(ltfEdgeToIgnore);
    pnlParameters.add(pnlBinning);
    //pre cross correlation
    pnlPreCrossCorrelation.setLayout(new BoxLayout(pnlPreCrossCorrelation,BoxLayout.X_AXIS));
    pnlPreCrossCorrelation.add(cbPreCrossCorrelation);
    pnlPreCrossCorrelation.add(Box.createHorizontalGlue());
    // binning
    pnlBinning.setLayout(new BoxLayout(pnlBinning, BoxLayout.X_AXIS));
    pnlBinning.add(spBinning.getContainer());
    pnlBinning.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlBinning.add(spMidasBinning.getContainer());
    // buttons
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlButtons.add(pnlLeftButtons);
    pnlButtons.add(pnlRightButtons);
    // left buttons
    pnlLeftButtons.setBoxLayout(BoxLayout.Y_AXIS);
    pnlLeftButtons.add(btnInitialAutoAlignment);
    pnlLeftButtons.add(btnMidas);
    pnlLeftButtons.add(btnRefineAutoAlignment);
    // right buttons
    pnlRightButtons.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRightButtons.add(btnRevertToMidas);
    pnlRightButtons.add(btnRevertToEmpty);
  }

  /**
   * Sets the controller and adds listeners
   * @param input
   */
  public void setController(final AutoAlignmentController input) {
    controller = input;
    addListeners();
  }

  private void addListeners() {
    ActionListener listener = new AutoAlignmentActionListener(this);
    btnInitialAutoAlignment.addActionListener(listener);
    btnMidas.addActionListener(listener);
    btnRefineAutoAlignment.addActionListener(listener);
    btnRevertToMidas.addActionListener(listener);
    btnRevertToEmpty.addActionListener(listener);
  }

  Component getRootComponent() {
    return pnlRoot.getContainer();
  }

  void getParameters(final AutoAlignmentMetaData metaData) {
    metaData.setSigmaLowFrequency(ltfSigmaLowFrequency.getText());
    metaData.setCutoffHighFrequency(ltfCutoffHighFrequency.getText());
    metaData.setSigmaHighFrequency(ltfSigmaHighFrequency.getText());
    metaData.setAlignTransform(tcAlign.get());
    metaData.setPreCrossCorrelation(cbPreCrossCorrelation.isSelected());
    metaData.setSkipSectionsFrom1(ltfSkipSectionsFrom1.getText());
    metaData.setEdgeToIgnore(ltfEdgeToIgnore.getText());
    metaData.setBinning(spBinning.getValue());
    metaData.setMidasBinning(spMidasBinning.getValue());
  }

  void setParameters(final AutoAlignmentMetaData metaData) {
    ltfSigmaLowFrequency.setText(metaData.getSigmaLowFrequency().toString());
    ltfCutoffHighFrequency.setText(metaData.getCutoffHighFrequency().toString());
    ltfSigmaHighFrequency.setText(metaData.getSigmaHighFrequency().toString());
    tcAlign.set(metaData.getAlignTransform());
    cbPreCrossCorrelation.setSelected(metaData.isPreCrossCorrelation());
    ltfSkipSectionsFrom1.setText(metaData.getSkipSectionsFrom1());
    ltfEdgeToIgnore.setText(metaData.getEdgeToIgnore());
    spBinning.setValue(metaData.getBinning());
    spMidasBinning.setValue(metaData.getMidasBinning());
  }

  public void enableMidas() {
    btnMidas.setEnabled(true);
  }

  /**
   * checking if panel is equal to meta data.  Set useDefault to match how 
   * useDefault is used in setMetaData()
   * @param metaData
   * @return
   */
  boolean equals(final AutoAlignmentMetaData metaData) {
    if (!metaData.getSigmaLowFrequency().equals(ltfSigmaLowFrequency.getText())) {
      return false;
    }
    if (!metaData.getCutoffHighFrequency().equals(ltfCutoffHighFrequency.getText())) {
      return false;
    }
    if (!metaData.getSigmaHighFrequency().equals(ltfSigmaHighFrequency.getText())) {
      return false;
    }
    if (tcAlign.get() != metaData.getAlignTransform()) {
      return false;
    }
    return true;
  }

  private void action(final String command) {
    if (command.equals(btnInitialAutoAlignment.getActionCommand())) {
      btnMidas.setEnabled(false);
      controller.xfalignInitial(null, tomogramAverages);
    }
    else if (command.equals(btnMidas.getActionCommand())) {
      controller.midasSample(btnMidas.getQuotedLabel());
    }
    else if (command.equals(btnRefineAutoAlignment.getActionCommand())) {
      btnMidas.setEnabled(false);
      controller.xfalignRefine(null, tomogramAverages,
          btnRefineAutoAlignment.getQuotedLabel());
    }
    else if (command.equals(btnRevertToMidas.getActionCommand())) {
      controller.revertXfFileToMidas();
    }
    else if (command.equals(btnRevertToEmpty.getActionCommand())) {
      controller.revertXfFileToEmpty();
    }
  }

  private void setTooltips() {
    ltfSigmaLowFrequency
        .setToolTipText("Sigma of an inverted gaussian for filtering out low frequencies "
            + "before searching for transformation.");
    ltfCutoffHighFrequency
        .setToolTipText("Starting radius of a gaussian for filtering out high frequencies "
            + "before searching for transformation.");
    ltfSigmaHighFrequency
        .setToolTipText("Sigma of gaussian for filtering out high frequencies before "
            + "searching for transformation.");
    btnInitialAutoAlignment
        .setToolTipText("OPTIONAL:  Run xfalign.  Find preliminary translational "
            + "alignments with tiltxcorr rather then using an existing .xf file.");
    btnMidas
        .setToolTipText("Open Midas to check the output of the auto alignment and to make "
            + "transformations by hand.");
    btnRefineAutoAlignment
        .setToolTipText("OPTIONAL:  Run xfalign using preliminary alignments created by "
            + "the most recent use of Midas or xfalign.");
    btnRevertToMidas
        .setToolTipText("Use to ignore xfalign changes.  Returns transformations to the "
            + "state created by the most recent save done in Midas.");
    btnRevertToEmpty.setToolTipText("Use to remove all transformations.");
  }

  private static final class AutoAlignmentActionListener implements ActionListener {
    private final AutoAlignmentPanel panel;

    private AutoAlignmentActionListener(final AutoAlignmentPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(event.getActionCommand());
    }
  }
}
