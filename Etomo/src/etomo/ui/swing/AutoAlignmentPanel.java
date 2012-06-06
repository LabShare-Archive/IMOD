package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import etomo.JoinManager;
import etomo.type.ConstJoinMetaData;
import etomo.type.JoinMetaData;

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

  private final JoinManager manager;

  private AutoAlignmentPanel(final JoinManager manager) {
    this.manager = manager;
  }

  static AutoAlignmentPanel getInstance(final JoinManager manager) {
    AutoAlignmentPanel instance = new AutoAlignmentPanel(manager);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    btnInitialAutoAlignment.setSize();
    btnMidas.setSize();
    btnRefineAutoAlignment.setSize();
    btnRevertToMidas.setSize();
    btnRevertToEmpty.setSize();
    // panels
    SpacedPanel pnlLeftButtons = SpacedPanel.getInstance();
    SpacedPanel pnlRightButtons = SpacedPanel.getInstance();
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

  void getMetaData(JoinMetaData metaData) {
    metaData.setSigmaLowFrequency(ltfSigmaLowFrequency.getText());
    metaData.setCutoffHighFrequency(ltfCutoffHighFrequency.getText());
    metaData.setSigmaHighFrequency(ltfSigmaHighFrequency.getText());
    metaData.setAlignTransform(tcAlign.get());
  }

  void setMetaData(ConstJoinMetaData metaData) {
    ltfSigmaLowFrequency.setText(metaData.getSigmaLowFrequency().toString());
    ltfCutoffHighFrequency.setText(metaData.getCutoffHighFrequency().toString());
    ltfSigmaHighFrequency.setText(metaData.getSigmaHighFrequency().toString());
    tcAlign.set(metaData.getAlignTransform());
  }

  void enableMidas() {
    btnMidas.setEnabled(true);
  }

  /**
   * checking if panel is equal to meta data.  Set useDefault to match how 
   * useDefault is used in setMetaData()
   * @param metaData
   * @return
   */
  boolean equals(ConstJoinMetaData metaData) {
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
      manager.xfalignInitial(null);
    }
    else if (command.equals(btnMidas.getActionCommand())) {
      manager.midasSample(btnMidas.getQuotedLabel());
    }
    else if (command.equals(btnRefineAutoAlignment.getActionCommand())) {
      btnMidas.setEnabled(false);
      manager.xfalignRefine(null, btnRefineAutoAlignment.getQuotedLabel());
    }
    else if (command.equals(btnRevertToMidas.getActionCommand())) {
      manager.revertXfFileToMidas();
    }
    else if (command.equals(btnRevertToEmpty.getActionCommand())) {
      manager.revertXfFileToEmpty();
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
