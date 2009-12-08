package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.ConstPeetMetaData;
import etomo.type.EtomoAutodoc;
import etomo.type.PeetMetaData;
import etomo.type.Run3dmodMenuOptions;

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
 * <p> $Log$ </p>
 */
final class MaskingPanel implements CylinderOrientationParent,
    RadiiOfSphereOrCylinderParent {
  public static final String rcsid = "$Id$";

  static final String MASK_TYPE_LABEL = "Masking";
  private static final String MASK_TYPE_VOLUME_LABEL = "Volume";
  static final String MASK_TYPE_SPHERE_LABEL = "Sphere";
  static final String MASK_TYPE_CYLINDER_LABEL = "Cylinder";

  private final EtomoPanel pnlRoot = new EtomoPanel();
  private final ButtonGroup bgMaskType = new ButtonGroup();
  private final RadioButton rbMaskTypeNone = new RadioButton("None",
      MatlabParam.MaskType.NONE, bgMaskType);
  private final RadioButton rbMaskTypeVolume = new RadioButton(
      MASK_TYPE_VOLUME_LABEL, MatlabParam.MaskType.VOLUME, bgMaskType);
  private final RadioButton rbMaskTypeSphere = new RadioButton(
      MASK_TYPE_SPHERE_LABEL, MatlabParam.MaskType.SPHERE, bgMaskType);
  private final RadioButton rbMaskTypeCylinder = new RadioButton(
      MASK_TYPE_CYLINDER_LABEL, MatlabParam.MaskType.CYLINDER, bgMaskType);
  private final FileTextField ftfMaskTypeVolume = new FileTextField(
      MASK_TYPE_VOLUME_LABEL + ": ");

  private final CylinderOrientationPanel cylinderOrientationPanel;
  private final MaskingParent parent;
  private final RadiiOfSphereOrCylinderPanel radiiOfSphereOrCylinderPanel;
  private final BaseManager manager;

  private MaskingPanel(BaseManager manager, MaskingParent parent) {
    this.manager = manager;
    this.parent = parent;
    cylinderOrientationPanel = CylinderOrientationPanel.getInstance(manager,
        this);
    radiiOfSphereOrCylinderPanel = RadiiOfSphereOrCylinderPanel.getInstance(
        manager, this);
  }

  static MaskingPanel getInstance(BaseManager manager, MaskingParent parent) {
    MaskingPanel instance = new MaskingPanel(manager, parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    ftfMaskTypeVolume.addActionListener(new MaskTypeVolumeActionListener(this));
    MaskingActionListener actionListener = new MaskingActionListener(this);
    rbMaskTypeNone.addActionListener(actionListener);
    rbMaskTypeVolume.addActionListener(actionListener);
    rbMaskTypeSphere.addActionListener(actionListener);
    rbMaskTypeCylinder.addActionListener(actionListener);
  }

  private void createPanel() {
    //local panels
    JPanel pnlMaskType = new JPanel();
    JPanel pnlMaskVolumeRadii = new JPanel();
    //initalization
    ftfMaskTypeVolume.setFieldWidth(UIParameters.INSTANCE.getFileWidth());
    //root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.setBorder(new EtchedBorder(MASK_TYPE_LABEL).getBorder());
    pnlRoot.add(pnlMaskType);
    pnlRoot.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlRoot.add(pnlMaskVolumeRadii);
    pnlRoot.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlRoot.add(cylinderOrientationPanel.getComponent());
    //mask type
    pnlMaskType.setLayout(new BoxLayout(pnlMaskType, BoxLayout.Y_AXIS));
    pnlMaskType.add(rbMaskTypeNone.getComponent());
    pnlMaskType.add(rbMaskTypeVolume.getComponent());
    pnlMaskType.add(rbMaskTypeSphere.getComponent());
    pnlMaskType.add(rbMaskTypeCylinder.getComponent());
    //mask type volume and sphere details
    pnlMaskVolumeRadii.setLayout(new BoxLayout(pnlMaskVolumeRadii,
        BoxLayout.Y_AXIS));
    pnlMaskVolumeRadii.add(ftfMaskTypeVolume.getContainer());
    pnlMaskVolumeRadii.add(radiiOfSphereOrCylinderPanel.getComponent());
  }

  Component getComponent() {
    return pnlRoot;
  }
  
  /**
   * Called by the parent updateDisplay().  Enabled/disables fields.  Calls
   * updateDisplay() in subordinate panels.
   */
   void updateDisplay() {
    ftfMaskTypeVolume.setEnabled(rbMaskTypeVolume.isSelected());
    radiiOfSphereOrCylinderPanel.updateDisplay();
    cylinderOrientationPanel.updateDisplay();
  }

  public boolean isIncorrectPaths() {
    return !ftfMaskTypeVolume.isEmpty() && !ftfMaskTypeVolume.exists();
  }

  /**
   * If ftfMaskTypeVolume has an invalid path, call
   * ReferenceParent.fixIncorrectPath(FileTextField,boolean).  Returns true to
   * keep fixing paths.  Returns false to stop fixing paths.
   * @param choosePathEveryRow
   * @return
   */
  boolean fixIncorrectPaths(final boolean choosePathEveryRow) {
    if (isIncorrectPaths()) {
      return parent.fixIncorrectPath(ftfMaskTypeVolume, choosePathEveryRow);
    }
    return true;
  }

  public void getParameters(final PeetMetaData metaData) {
    cylinderOrientationPanel.getParameters(metaData);
    metaData.setMaskTypeVolume(ftfMaskTypeVolume.getText());
  }

  /**
   * Set parameters from metaData and then overwrite them with parameters from
   * MatlabParamFile.  This allows inactive data to appear on the screen but
   * allows MatlabParamFile's active data to override active metaData.  So if
   * the user changes the .prm file, the active data on the screen will be
   * correct.
   * @param metaData
   */
  public void setParameters(final ConstPeetMetaData metaData,
      boolean parametersOnly) {
    if (!parametersOnly) {
      ftfMaskTypeVolume.setText(metaData.getMaskTypeVolume());
    }
    cylinderOrientationPanel.setParameters(metaData);
  }

  /**
   * Load data from MatlabParamFile.  Load only active data after the meta data
   * has been loaded.
   * Do not rely on whether fields are enabled to make decisions in this
   * function; updateDisplay may not have been run with data in the screen.
   * When looking at the settings of dialog fields, make sure that they have
   * already been loaded in THIS function.  This is sometimes the first data-
   * loading function to be run.
   * @param matlabParamFile
   * @param paramatersOnly 
   */
  public void setParameters(final MatlabParam matlabParam,
      boolean parametersOnly) {
    String maskTypeValue = matlabParam.getMaskType();
    MatlabParam.MaskType maskType = MatlabParam.MaskType
        .getInstance(maskTypeValue);
    if (maskType == MatlabParam.MaskType.NONE) {
      rbMaskTypeNone.setSelected(true);
    }
    else if (maskType == MatlabParam.MaskType.VOLUME) {
      rbMaskTypeVolume.setSelected(true);
      if (!parametersOnly) {
        ftfMaskTypeVolume.setText(maskTypeValue);
      }
    }
    else if (maskType == MatlabParam.MaskType.SPHERE) {
      rbMaskTypeSphere.setSelected(true);
    }
    else if (maskType == MatlabParam.MaskType.CYLINDER) {
      rbMaskTypeCylinder.setSelected(true);
    }
    cylinderOrientationPanel.setParameters(matlabParam);
    radiiOfSphereOrCylinderPanel.setParameters(matlabParam);
  }

  public void getParameters(final MatlabParam matlabParam) {
    if (rbMaskTypeVolume.isSelected()) {
      matlabParam.setMaskType(ftfMaskTypeVolume.getText());
    }
    else {
      matlabParam.setMaskType(((RadioButton.RadioButtonModel) bgMaskType
          .getSelection()).getEnumeratedType());
    }
    cylinderOrientationPanel.getParameters(matlabParam);
    radiiOfSphereOrCylinderPanel.getParameters(matlabParam);
  }

  void setDefaults() {
    rbMaskTypeNone.setSelected(true);
  }

  public boolean isReferenceFileSelected() {
    return parent.isReferenceFileSelected();
  }

  public int getVolumeTableSize() {
    return parent.getVolumeTableSize();
  }

  public boolean isMaskTypeSphereSelected() {
    return rbMaskTypeSphere.isSelected();
  }

  public boolean isMaskTypeCylinderSelected() {
    return rbMaskTypeCylinder.isSelected();
  }

  private void chooseMaskTypeVolume(FileTextField fileTextField) {
    JFileChooser chooser = new FileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(pnlRoot);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      fileTextField.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  /**
   * Validate for running.
   * @return null if valid, error message if invalid.
   */
  String validateRun() {
    //Masking
    //volume
    if (rbMaskTypeVolume.isSelected() && ftfMaskTypeVolume.isEmpty()) {
      return "In " + MaskingPanel.MASK_TYPE_LABEL + ", "
          + MASK_TYPE_VOLUME_LABEL + " is required when "
          + MASK_TYPE_VOLUME_LABEL + " " + MaskingPanel.MASK_TYPE_LABEL
          + " is selected. ";
    }
    //validate radii
    String errorMessage = radiiOfSphereOrCylinderPanel.validateRun();
    if (errorMessage != null) {
      return errorMessage;
    }
    //validate cylinder orientation
    errorMessage = cylinderOrientationPanel.validateRun();
    if (errorMessage != null) {
      return errorMessage;
    }
    return null;
  }

  private void action(final String actionCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(rbMaskTypeNone.getActionCommand())
        || actionCommand.equals(rbMaskTypeVolume.getActionCommand())
        || actionCommand.equals(rbMaskTypeSphere.getActionCommand())
        || actionCommand.equals(rbMaskTypeCylinder.getActionCommand())) {
      parent.updateDisplay();
    }
  }
  
  private void maskTypeVolumeAction() {
    chooseMaskTypeVolume(ftfMaskTypeVolume);
  }

  private void setTooltips() {
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
          AutodocFactory.PEET_PRM, manager.getManagerKey());
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.MASK_TYPE_KEY);
      rbMaskTypeNone.setToolTipText(tooltip);
      rbMaskTypeVolume.setToolTipText(tooltip);
      rbMaskTypeSphere.setToolTipText(tooltip);
      rbMaskTypeCylinder.setToolTipText(tooltip);
      ftfMaskTypeVolume.setToolTipText(tooltip);
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
  }

  private static final class MaskingActionListener implements ActionListener {
    private final MaskingPanel maskingPanel;

    private MaskingActionListener(final MaskingPanel maskingPanel) {
      this.maskingPanel = maskingPanel;
    }

    public void actionPerformed(final ActionEvent event) {
      maskingPanel.action(event.getActionCommand(), null);
    }
  }

  private static final class MaskTypeVolumeActionListener implements
      ActionListener {
    private final MaskingPanel maskingPanel;

    private MaskTypeVolumeActionListener(final MaskingPanel maskingPanel) {
      this.maskingPanel = maskingPanel;
    }

    public void actionPerformed(final ActionEvent event) {
      maskingPanel.maskTypeVolumeAction();
    }
  }
}
