package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.storage.MatlabParam;
import etomo.type.ConstPeetMetaData;
import etomo.type.PeetMetaData;

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
 * <p> Revision 1.2  2009/12/01 00:26:20  sueh
 * <p> bug# 1285 Gave panel is own action listener.
 * <p>
 * <p> Revision 1.1  2009/11/20 17:32:14  sueh
 * <p> bug# 1282 Reference panel factored out of PeetDialog.
 * <p> </p>
 */
final class ReferencePanel {
  public static final String rcsid = "$Id$";

  private static final String REFERENCE_VOLUME_LABEL = "Volume #";
  static final String REFERENCE_FILE_LABEL = "Reference file";
  private static final String PARTICLE_LABEL = "Particle #";
  private static final String REFERENCE_LABEL = "Reference";

  private final EtomoPanel pnlRoot = new EtomoPanel();
  private final ButtonGroup bgReference = new ButtonGroup();
  private final RadioButton rbReferenceParticle = new RadioButton(
      REFERENCE_VOLUME_LABEL + ": ", bgReference);
  private final Spinner sReferenceVolume = Spinner
      .getInstance(REFERENCE_VOLUME_LABEL + ": ");
  private final RadioButton rbReferenceFile = new RadioButton(
      REFERENCE_FILE_LABEL + " :", bgReference);
  private final LabeledTextField ltfReferenceParticle = new LabeledTextField(
      PARTICLE_LABEL + ": ");
  private final FileTextField ftfReferenceFile = FileTextField
      .getUnlabeledInstance(REFERENCE_FILE_LABEL + " :");

  private final ReferenceParent parent;
  private final BaseManager manager;

  private ReferencePanel(final ReferenceParent parent, final BaseManager manager) {
    this.parent = parent;
    this.manager = manager;
    ftfReferenceFile.setFieldWidth(UIParameters.INSTANCE.getFileWidth());
  }

  static ReferencePanel getInstance(final ReferenceParent parent,
      final BaseManager manager) {
    ReferencePanel instance = new ReferencePanel(parent, manager);
    instance.createPanel();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    ActionListener actionListener = new ReferenceActionListener(this);
    rbReferenceParticle.addActionListener(actionListener);
    rbReferenceFile.addActionListener(actionListener);
    ftfReferenceFile.addActionListener(new ReferenceFileActionListener(this));
  }

  private void createPanel() {
    //local panels
    JPanel pnlVolumeReference = new JPanel();
    JPanel pnlVolumeFile = new JPanel();
    //initalization
    //Root
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new EtchedBorder(REFERENCE_LABEL).getBorder());
    pnlRoot.add(pnlVolumeReference);
    pnlRoot.add(pnlVolumeFile);
    //volume reference panel
    pnlVolumeReference.setLayout(new BoxLayout(pnlVolumeReference,
        BoxLayout.X_AXIS));
    pnlVolumeReference.add(rbReferenceParticle.getComponent());
    pnlVolumeReference.add(sReferenceVolume.getContainer());
    pnlVolumeReference.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlVolumeReference.add(ltfReferenceParticle.getContainer());
    //volume file panel
    pnlVolumeFile.setLayout(new BoxLayout(pnlVolumeFile, BoxLayout.X_AXIS));
    pnlVolumeFile.add(rbReferenceFile.getComponent());
    pnlVolumeFile.add(ftfReferenceFile.getContainer());
  }

  Component getComponent() {
    return pnlRoot;
  }

  boolean isReferenceFileIncorrectPath() {
    return !ftfReferenceFile.isEmpty() && !ftfReferenceFile.exists();
  }

  /**
   * If ftfReferenceFile has an invalid path, call
   * ReferenceParent.fixIncorrectPath(FileTextField,boolean).  Returns true to
   * keep fixing paths.  Returns false to stop fixing paths.
   * @param choosePathEveryRow
   * @return
   */
  boolean fixIncorrectPaths(final boolean choosePathEveryRow) {
    if (isReferenceFileIncorrectPath()) {
      return parent.fixIncorrectPath(ftfReferenceFile, choosePathEveryRow);
    }
    return true;
  }

  /**
   * Send values to PeetMetaData.
   * @param metaData
   */
  void getParameters(final PeetMetaData metaData) {
    metaData.setReferenceVolume(sReferenceVolume.getValue());
    metaData.setReferenceParticle(ltfReferenceParticle.getText());
    metaData.setReferenceFile(ftfReferenceFile.getText());
  }

  /**
   * Load data from ConstPeetMetaData.
   * @param metaData
   */
  void setParameters(final ConstPeetMetaData metaData) {
    ftfReferenceFile.setText(metaData.getReferenceFile());
    sReferenceVolume.setValue(metaData.getReferenceVolume());
    ltfReferenceParticle.setText(metaData.getReferenceParticle());
  }

  /**
   * Load active data from MatlabParam.
   * @param matlabParam
   */
  void setParameters(final MatlabParam matlabParam) {
    if (matlabParam.useReferenceFile()) {
      rbReferenceFile.setSelected(true);
      ftfReferenceFile.setText(matlabParam.getReferenceFile());
    }
    else {
      rbReferenceParticle.setSelected(true);
      sReferenceVolume.setValue(matlabParam.getReferenceVolume());
      ltfReferenceParticle.setText(matlabParam.getReferenceParticle());
    }
  }

  /**
   * Send active data to MatlabParam.
   * @param matlabParam
   */
  void getParameters(final MatlabParam matlabParam) {
    if (rbReferenceParticle.isSelected()) {
      matlabParam.setReferenceVolume(sReferenceVolume.getValue());
      matlabParam.setReferenceParticle(ltfReferenceParticle.getText());
    }
    else if (rbReferenceFile.isSelected()) {
      matlabParam.setReferenceFile(ftfReferenceFile.getText());
    }
  }

  /**
   * @return true if rbReferenceFile is selected.
   */
  boolean isReferenceFileSelected() {
    return rbReferenceFile.isSelected();
  }

  /**
   * @return true if rbReferenceParticle is selected.
   */
  boolean isReferenceParticleSelected() {
    return rbReferenceParticle.isSelected();
  }

  /**
   * Action in response to ftfReferenceFile's file chooser being pressed.
   */
  private void referenceFileAction() {
    chooseReferenceFile(ftfReferenceFile);
  }

  private void chooseReferenceFile(FileTextField fileTextField) {
    JFileChooser chooser = new FileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(pnlRoot);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      fileTextField.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  private void action(final String actionCommand) {
    if (actionCommand.equals(rbReferenceParticle.getActionCommand())
        || actionCommand.equals(rbReferenceFile.getActionCommand())) {
      parent.updateDisplay();
    }
  }

  /**
   * Validatation for fields to be used when prmParser is run.
   * @return error string if invalid
   */
  String validateRun() {
    //Must either have a volume and particle or a reference file.
    //Must have particle number if volume is selected
    if (rbReferenceParticle.isSelected() && ltfReferenceParticle.isEmpty()) {
      return "In " + REFERENCE_LABEL + ", " + PARTICLE_LABEL
          + " is required when " + REFERENCE_VOLUME_LABEL + " is selected.";
    }
    //Must have a reference file if reference file is selected
    if (rbReferenceFile.isSelected() && ftfReferenceFile.isEmpty()) {
      return "In " + REFERENCE_LABEL + ", " + REFERENCE_FILE_LABEL
          + " is required when " + REFERENCE_FILE_LABEL + " is selected.";
    }
    return null;
  }

  /**
   * Reset values and set defaults.
   */
  void reset() {
    ltfReferenceParticle.clear();
    ftfReferenceFile.clear();
    rbReferenceParticle.setSelected(false);
    sReferenceVolume.reset();
    rbReferenceFile.setSelected(false);
  }

  void setDefaults() {
    if (MatlabParam.REFERENCE_FILE_DEFAULT) {
      rbReferenceFile.setSelected(true);
    }
    else {
      rbReferenceParticle.setSelected(true);
    }
  }

  void updateDisplay() {
    int size = parent.getVolumeTableSize();
    //reference
    boolean volumeRows = size > 0;
    rbReferenceParticle.setEnabled(volumeRows);
    sReferenceVolume.setEnabled(volumeRows && rbReferenceParticle.isSelected());
    sReferenceVolume.setMax(size);
    ltfReferenceParticle.setEnabled(volumeRows
        && rbReferenceParticle.isSelected());
    ftfReferenceFile.setEnabled(volumeRows && rbReferenceFile.isSelected());
  }

  /**
   * Set the tooltip in all fields.
   * @param tooltip
   */
  void setTooltip(final String tooltip) {
    rbReferenceParticle.setToolTipText(tooltip);
    rbReferenceFile.setToolTipText(tooltip);
    sReferenceVolume.setToolTipText(tooltip);
    ltfReferenceParticle.setToolTipText(tooltip);
    ftfReferenceFile.setToolTipText(tooltip);
  }

  private static final class ReferenceActionListener implements ActionListener {
    private final ReferencePanel panel;

    private ReferenceActionListener(final ReferencePanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(event.getActionCommand());
    }
  }

  private static final class ReferenceFileActionListener implements
      ActionListener {
    private final ReferencePanel referencePanel;

    private ReferenceFileActionListener(final ReferencePanel referencePanel) {
      this.referencePanel = referencePanel;
    }

    public void actionPerformed(final ActionEvent event) {
      referencePanel.referenceFileAction();
    }
  }
}
