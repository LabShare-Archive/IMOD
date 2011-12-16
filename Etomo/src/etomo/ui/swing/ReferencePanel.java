package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstPeetMetaData;
import etomo.type.EtomoAutodoc;
import etomo.type.PeetMetaData;
import etomo.util.FilePath;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.5  2009/12/23 02:27:06  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.4  2009/12/08 02:49:11  sueh
 * <p> bug# 1286 Independently setting tooltips.
 * <p>
 * <p> Revision 1.3  2009/12/02 00:08:18  sueh
 * <p> bug# 1290 Made the label of the reference file package private.
 * <p>
 * <p> Revision 1.2  2009/12/01 00:26:20  sueh
 * <p> bug# 1285 Gave panel is own action listener.
 * <p>
 * <p> Revision 1.1  2009/11/20 17:32:14  sueh
 * <p> bug# 1282 Reference panel factored out of PeetDialog.
 * <p> </p>
 */
final class ReferencePanel {
  public static final String rcsid = "$Id$";

  private static final String TITLE = "Reference";
  private static final String REFERENCE_FILE_LABEL = "User supplied file: ";

  private final EtomoPanel pnlRoot = new EtomoPanel();
  private final ButtonGroup bgReference = new ButtonGroup();
  private final RadioTextField rtfParticle = RadioTextField.getInstance("Particle ",
      bgReference);
  private final Spinner sVolume = Spinner.getLabeledInstance("In Volume: ");
  private final RadioButton rbFile = new RadioButton(REFERENCE_FILE_LABEL, bgReference);
  private final FileTextField2 ftfFile;
  private final RadioTextField rtfMultiparticleGroups = RadioTextField.getInstance(
      "Multiparticle reference ", bgReference);
  private final LabeledTextField ltfMultiparticleParticles = new LabeledTextField(
      "Groups of: ");
  private final JLabel lMultiparticle = new JLabel("particles");

  private final ReferenceParent parent;
  private final BaseManager manager;

  private ReferencePanel(final ReferenceParent parent, final BaseManager manager) {
    this.parent = parent;
    this.manager = manager;
    ftfFile = FileTextField2.getUnlabeledPeetInstance(manager, REFERENCE_FILE_LABEL);// unlabeled
    ftfFile.setAdjustedFieldWidth(225);
  }

  static ReferencePanel getInstance(final ReferenceParent parent,
      final BaseManager manager) {
    ReferencePanel instance = new ReferencePanel(parent, manager);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    ActionListener actionListener = new ReferenceActionListener(this);
    rtfParticle.addActionListener(actionListener);
    rbFile.addActionListener(actionListener);
    rtfMultiparticleGroups.addActionListener(actionListener);
  }

  private void createPanel() {
    // Init
    rtfMultiparticleGroups
        .setText(MatlabParam.REFERENCE_FLG_FAIR_REFERENCE_GROUPS_DEFAULT);
    ltfMultiparticleParticles
        .setText(MatlabParam.REFERENCE_FLG_FAIR_REFERENCE_PARTICLES_DEFAULT);
    // local panels
    JPanel pnlParticle = new JPanel();
    JPanel pnlFile = new JPanel();
    JPanel pnlMultiparticle = new JPanel();
    // initalization
    // Root
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new EtchedBorder(TITLE).getBorder());
    pnlRoot.add(pnlParticle);
    pnlRoot.add(pnlFile);
    pnlRoot.add(pnlMultiparticle);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y15));
    // particle panel
    pnlParticle.setLayout(new BoxLayout(pnlParticle, BoxLayout.X_AXIS));
    pnlParticle.add(rtfParticle.getContainer());
    pnlParticle.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlParticle.add(sVolume.getContainer());
    pnlParticle.add(Box.createRigidArea(new Dimension(130,0)));
    // file panel
    pnlFile.setLayout(new BoxLayout(pnlFile, BoxLayout.X_AXIS));
    pnlFile.add(rbFile.getComponent());
    pnlFile.add(ftfFile.getRootPanel());
    pnlFile.add(Box.createRigidArea(FixedDim.x3_y0));
    // multiparticle panel
    pnlMultiparticle.setLayout(new BoxLayout(pnlMultiparticle, BoxLayout.X_AXIS));
    pnlMultiparticle.add(rtfMultiparticleGroups.getContainer());
    pnlMultiparticle.add(Box.createRigidArea(FixedDim.x3_y0));
    pnlMultiparticle.add(ltfMultiparticleParticles.getContainer());
    pnlMultiparticle.add(Box.createRigidArea(FixedDim.x3_y0));
    pnlMultiparticle.add(lMultiparticle);
  }

  Component getComponent() {
    return pnlRoot;
  }

  /**
   * Make the copied path relative to this dataset, preserving the location of the files
   * that the old dataset was using.  So if the file was in the original dataset
   * directory, the new path will point (with a relative path if possible) to the file in
   * the original dataset directory.  If the file path is absolute, don't change it.
   * @param rootOfCopiedFilePaths
   */
  void convertCopiedPaths(final String origDatasetDir) {
    String propertyUserDir = manager.getPropertyUserDir();
    if (!ftfFile.isEmpty()) {
      ftfFile.setText(FilePath.getRerootedRelativePath(origDatasetDir, propertyUserDir,
          ftfFile.getText()));
    }
  }

  boolean isIncorrectPaths() {
    return !ftfFile.isEmpty() && !ftfFile.exists();
  }

  /**
   * If ftfReferenceFile has an invalid path, call
   * ReferenceParent.fixIncorrectPath(FileTextField,boolean).  Returns true to
   * keep fixing paths.  Returns false to stop fixing paths.
   * @param choosePathEveryRow
   * @return
   */
  boolean fixIncorrectPaths(final boolean choosePathEveryRow) {
    if (isIncorrectPaths()) {
      return parent.fixIncorrectPath(ftfFile, choosePathEveryRow);
    }
    return true;
  }

  /**
   * Send values to PeetMetaData.
   * @param metaData
   */
  void getParameters(final PeetMetaData metaData) {
    metaData.setReferenceVolume(sVolume.getValue());
    metaData.setReferenceParticle(rtfParticle.getText());
    metaData.setReferenceFile(ftfFile.getText());
    metaData.setReferenceMultiparticleGroups(rtfMultiparticleGroups.getText());
    metaData.setReferenceMultiparticleParticles(ltfMultiparticleParticles.getText());
  }

  /**
   * Load data from ConstPeetMetaData.
   * @param metaData
   */
  void setParameters(final ConstPeetMetaData metaData) {
    ftfFile.setText(metaData.getReferenceFile());
    sVolume.setValue(metaData.getReferenceVolume());
    rtfParticle.setText(metaData.getReferenceParticle());
    rtfMultiparticleGroups.setText(metaData.getReferenceMultiparticleGroups());
    ltfMultiparticleParticles.setText(metaData.getReferenceMultiparticleParticles());
  }

  /**
   * Load active data from MatlabParam.
   * @param matlabParam
   */
  void setParameters(final MatlabParam matlabParam) {
    if (matlabParam.useReferenceFile()) {
      rbFile.setSelected(true);
      ftfFile.setText(matlabParam.getReferenceFile());
    }
    else if (matlabParam.isFlgFairReference()) {
      rtfMultiparticleGroups.setSelected(true);
      rtfMultiparticleGroups.setText(matlabParam.getReferenceVolume());
      ltfMultiparticleParticles.setText(matlabParam.getReferenceParticle());
    }
    else {
      rtfParticle.setSelected(true);
      sVolume.setValue(matlabParam.getReferenceVolume());
      rtfParticle.setText(matlabParam.getReferenceParticle());
    }
  }

  /**
   * Send active data to MatlabParam.
   * @param matlabParam
   */
  void getParameters(final MatlabParam matlabParam) {
    if (rtfParticle.isSelected()) {
      matlabParam.setReferenceVolume(sVolume.getValue());
      matlabParam.setReferenceParticle(rtfParticle.getText());
    }
    else if (rbFile.isSelected()) {
      matlabParam.setReferenceFile(ftfFile.getText());
    }
    else if (rtfMultiparticleGroups.isSelected()) {
      matlabParam.setFlgFairReference(true);
      matlabParam.setReferenceVolume(rtfMultiparticleGroups.getText());
      matlabParam.setReferenceParticle(ltfMultiparticleParticles.getText());
    }
  }

  /**
   * @return true if rbReferenceFile is selected.
   */
  boolean isReferenceFileSelected() {
    return rbFile.isSelected();
  }

  /**
   * @return true if rbReferenceParticle is selected.
   */
  boolean isReferenceParticleSelected() {
    return rtfParticle.isSelected();
  }

  private void action(final String actionCommand) {
    if (actionCommand.equals(rtfParticle.getActionCommand())
        || actionCommand.equals(rbFile.getActionCommand())
        || actionCommand.equals(rtfMultiparticleGroups.getActionCommand())) {
      parent.updateDisplay();
    }
  }

  /**
   * Validatation for fields to be used when prmParser is run.
   * @return error string if invalid
   */
  String validateRun() {
    // Must either have a volume and particle or a reference file.
    // Must have particle number if volume is selected
    if (rtfParticle.isSelected() && rtfParticle.isEmpty()) {
      return "In " + TITLE + ", " + rtfParticle.getLabel() + " is required when "
          + sVolume.getLabel() + " is selected.";
    }
    // Must have a reference file if reference file is selected
    if (rbFile.isSelected() && ftfFile.isEmpty()) {
      return "In " + TITLE + ", a file is required when " + rbFile.getText()
          + " is selected.";
    }
    return null;
  }

  /**
   * Reset values.
   */
  void reset() {
    rtfParticle.setSelected(false);
    sVolume.reset();
    rbFile.setSelected(false);
    ftfFile.clear();
    rtfMultiparticleGroups.setSelected(false);
    rtfMultiparticleGroups.setText("");
    ltfMultiparticleParticles.clear();
  }

  void setDefaults() {
    rtfParticle.setSelected(true);
  }

  void updateDisplay() {
    rtfParticle.setEnabled(parent.getVolumeTableSize() > 0);
    sVolume.setEnabled(rtfParticle.isSelected());
    sVolume.setMax(parent.getVolumeTableSize());
    ftfFile.setEnabled(rbFile.isSelected());
    boolean enable = rtfMultiparticleGroups.isSelected();
    ltfMultiparticleParticles.setEnabled(rtfMultiparticleGroups.isSelected());
    lMultiparticle.setEnabled(rtfMultiparticleGroups.isSelected());
  }

  /**
   * Set the tooltip in all fields.
   * @param tooltip
   */
  private void setTooltips() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.PEET_PRM, AxisID.ONLY);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    sVolume.setToolTipText("The number of the volume containing the reference.");
    rtfParticle
        .setRadioButtonToolTipText("Specify the reference by volume and particle numbers.");
    rtfParticle
        .setTextFieldToolTipText("The number of the particle to use as the reference.");
    rbFile.setToolTipText("Specify the reference by filename.");
    ftfFile.setToolTipText("The name of the file containing the MRC volume to use "
        + "as the reference.");
    rtfMultiparticleGroups.setRadioButtonToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.FLG_FAIR_REFERENCE_KEY));
    rtfMultiparticleGroups
        .setTextFieldToolTipText("Number of groups to be used to generate a multi-particle "
            + "reference");
    ltfMultiparticleParticles
        .setToolTipText("Number of particles to be used to generate a multi-particle "
            + "reference.");
  }

  private static final class ReferenceActionListener implements ActionListener {
    private final ReferencePanel referencePanel;

    private ReferenceActionListener(final ReferencePanel referencePanel) {
      this.referencePanel = referencePanel;
    }

    public void actionPerformed(final ActionEvent event) {
      referencePanel.action(event.getActionCommand());
    }
  }
}
