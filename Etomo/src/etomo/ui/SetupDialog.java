/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 */
package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.SpinnerNumberModel;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.storage.MagGradientFileFilter;
import etomo.storage.StackFileFilter;
import etomo.storage.DistortionFileFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.Run3dmodMenuOption;
import etomo.type.ViewType;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

public class SetupDialog extends ProcessDialog implements ContextMenu, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private JPanel pnlDataParameters = new JPanel();
  private UIHarness uiHarness = UIHarness.INSTANCE;

  //  Dataset GUI objects
  private JPanel pnlDataset = new JPanel();
  private ImageIcon iconFolder = new ImageIcon(ClassLoader
    .getSystemResource("images/openFile.gif"));

  private LabeledTextField ltfDataset = new LabeledTextField("Dataset name: ");
  private JButton btnDataset = new JButton(iconFolder);

  private LabeledTextField ltfBackupDirectory = new LabeledTextField(
    "Backup directory: ");
  private JButton btnBackupDirectory = new JButton(iconFolder);

  //  Data type GUI objects
  private JPanel pnlDataType = new JPanel();
  private JPanel pnlAxisType = new JPanel();
  private JRadioButton rbSingleAxis = new JRadioButton("Single axis");
  private JRadioButton rbDualAxis = new JRadioButton("Dual axis");
  private ButtonGroup bgAxisType = new ButtonGroup();

  private JPanel pnlViewType = new JPanel();
  private JRadioButton rbSingleView = new JRadioButton("Single frame");
  private JRadioButton rbMontage = new JRadioButton("Montage");
  private ButtonGroup bgViewType = new ButtonGroup();

  private Run3dmodButton btnViewRawStackA = new Run3dmodButton(
    "View Raw Image Stack", this);
  private Run3dmodButton btnViewRawStackB = new Run3dmodButton(
    "View Raw Image Stack", this);

  //  Image parameter objects
  private JPanel pnlImageParams = new JPanel();
  private MultiLineButton btnScanHeader = new MultiLineButton("Scan Header");
  private JPanel pnlImageRows = new JPanel();

  private JPanel pnlStackInfo = new JPanel();
  private LabeledTextField ltfPixelSize = new LabeledTextField(
    "Pixel size (nm): ");
  private LabeledTextField ltfFiducialDiameter = new LabeledTextField(
    "Fiducial diameter (nm): ");
  private LabeledTextField ltfImageRotation = new LabeledTextField(
    "Image rotation (degrees): ");

  private JPanel pnlDistortionInfo = new JPanel();
  private LabeledTextField ltfDistortionFile = new LabeledTextField(
    "Image distortion field file: ");
  private JButton btnDistortionFile = new JButton(iconFolder);
  private LabeledSpinner spnBinning = new LabeledSpinner("Binning: ",
    new SpinnerNumberModel(1, 1, 50, 1));
  
  private JPanel pnlMagGradientInfo = new JPanel();
  private LabeledTextField ltfMagGradientFile = new LabeledTextField(
    "Mag gradients correction: ");
  private JButton btnMagGradientFile = new JButton(iconFolder);
  
  //  Tilt angle GUI objects
  private JPanel pnlPerAxisInfo = new JPanel();
  private JPanel pnlAxisInfoA = new JPanel();
  private BeveledBorder borderAxisInfoA = new BeveledBorder("Axis A: ");
  private TiltAngleDialogPanel tiltAnglesA = new TiltAngleDialogPanel();
  private LabeledTextField ltfExcludeListA = new LabeledTextField(
    "Exclude views: ");
  private JPanel pnlAdjustedFocusA = new JPanel();
  private JCheckBox cbAdjustedFocusA = new JCheckBox("Focus was adjusted between montage frames");

  private JPanel pnlAxisInfoB = new JPanel();
  private BeveledBorder borderAxisInfoB = new BeveledBorder("Axis B: ");
  private TiltAngleDialogPanel tiltAnglesB = new TiltAngleDialogPanel();
  private LabeledTextField ltfExcludeListB = new LabeledTextField(
    "Exclude views: ");
  private JPanel pnlAdjustedFocusB = new JPanel();
  private JCheckBox cbAdjustedFocusB = new JCheckBox("Focus was adjusted between montage frames");

  //  Construct the setup dialog
  public SetupDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.ONLY, DialogType.SETUP);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    createDatasetPanel();

    createDataTypePanel();

    createPerAxisInfoPanel();

    //  Relabel the postpone button
    btnPostpone.setText("Use Existing Coms");
    btnExecute.setText("Create Com Scripts");

    // There are no advanced settings for this dialog, remove the advanced
    // button
    pnlExitButtons.remove(btnAdvanced.getComponent());

    //  Add the panes to the dialog box
    rootPanel.add(pnlDataParameters);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlPerAxisInfo);
    rootPanel.add(Box.createVerticalGlue());
    addExitButtons();
    UIUtilities.alignComponentsX(rootPanel, Component.CENTER_ALIGNMENT);
    
    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Resize the standard panel buttons
    UIUtilities.setButtonSizeAll(pnlExitButtons, UIParameters.getButtonDimension());
    
    // Calcute the necessary window size
    uiHarness.pack(axisID, applicationManager);
  }

  private void createDatasetPanel() {

    //  Set the preferred and max sizes for the dataset GUI objects
    //  so that the box layout happens correctly
    btnDataset.setPreferredSize(FixedDim.folderButton);
    btnDataset.setMaximumSize(FixedDim.folderButton);
    btnBackupDirectory.setPreferredSize(FixedDim.folderButton);
    btnBackupDirectory.setMaximumSize(FixedDim.folderButton);
    btnDistortionFile.setPreferredSize(FixedDim.folderButton);
    btnDistortionFile.setMaximumSize(FixedDim.folderButton);
    //ltfDistortionFile.setMaximumSize(UIParameters.getFileFieldDimension());
    btnMagGradientFile.setPreferredSize(FixedDim.folderButton);
    btnMagGradientFile.setMaximumSize(FixedDim.folderButton);
    
    pnlDataset.setLayout(new BoxLayout(pnlDataset, BoxLayout.X_AXIS));

    //  Bind the buttons to their adapters
    btnDataset.addActionListener(new DatasetActionListener(this));
    btnBackupDirectory
      .addActionListener(new BackupDirectoryActionListener(this));
    btnDistortionFile.addActionListener(new DistortionFileActionListener(this));
    btnMagGradientFile.addActionListener(new MagGradientFileActionListener(this));
    
    rbSingleAxis.addActionListener(new SingleAxisActionListener(this));
    rbDualAxis.addActionListener(new DualAxisActionListener(this));
    rbSingleView.addActionListener(new SingleViewActionListener(this));
    rbMontage.addActionListener(new MontageActionListener(this));
    btnScanHeader.addActionListener(new ScanHeaderActionListener(this));
    btnViewRawStackA.addActionListener(new ViewRawStackAActionListener(this));
    btnViewRawStackB.addActionListener(new ViewRawStackBActionListener(this));

    //  Add the GUI objects to the pnl
    pnlDataset.add(Box.createRigidArea(FixedDim.x5_y0));

    pnlDataset.add(ltfDataset.getContainer());
    pnlDataset.add(btnDataset);
    pnlDataset.add(Box.createRigidArea(FixedDim.x10_y0));

    pnlDataset.add(ltfBackupDirectory.getContainer());
    pnlDataset.add(btnBackupDirectory);
    pnlDataset.add(Box.createRigidArea(FixedDim.x5_y0));

    //  Add the tooltip text
    setToolTipText();
  }

  private void createDataTypePanel() {

    //  Datatype subpnls: DataSource AxisType Viewtype
    Dimension dimDataTypePref = new Dimension(150, 80);

    bgAxisType.add(rbSingleAxis);
    bgAxisType.add(rbDualAxis);
    pnlAxisType.setLayout(new BoxLayout(pnlAxisType, BoxLayout.Y_AXIS));
    pnlAxisType.setPreferredSize(dimDataTypePref);
    pnlAxisType.setBorder(new EtchedBorder("Axis Type").getBorder());
    pnlAxisType.add(rbSingleAxis);
    pnlAxisType.add(rbDualAxis);
    bgViewType.add(rbSingleView);
    bgViewType.add(rbMontage);
    pnlViewType.setLayout(new BoxLayout(pnlViewType, BoxLayout.Y_AXIS));
    pnlViewType.setPreferredSize(dimDataTypePref);
    pnlViewType.setBorder(new EtchedBorder("Frame Type").getBorder());
    pnlViewType.add(rbSingleView);
    pnlViewType.add(rbMontage);

    //  Datatype panel
    pnlDataType.setLayout(new BoxLayout(pnlDataType, BoxLayout.X_AXIS));
    pnlDataType.setBorder(new EtchedBorder("Data Type").getBorder());
    pnlDataType.add(pnlAxisType);
    pnlDataType.add(Box.createHorizontalGlue());
    pnlDataType.add(pnlViewType);
    pnlDataType.add(Box.createHorizontalGlue());

    //  Pixel & Alignment panel
    ltfPixelSize.setColumns(5);
    ltfFiducialDiameter.setColumns(5);
    ltfImageRotation.setColumns(5);
    btnScanHeader.setSize();
    spnBinning.setTextMaxmimumSize(UIParameters.getSpinnerDimension());
    
    pnlStackInfo.setLayout(new BoxLayout(pnlStackInfo, BoxLayout.X_AXIS));
    btnScanHeader.setAlignmentY(Component.CENTER_ALIGNMENT);
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(btnScanHeader.getComponent());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(ltfPixelSize.getContainer());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(ltfFiducialDiameter.getContainer());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(ltfImageRotation.getContainer());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x5_y0));

    pnlDistortionInfo.setLayout(new BoxLayout(pnlDistortionInfo,
      BoxLayout.X_AXIS));
    pnlDistortionInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlDistortionInfo.add(ltfDistortionFile.getContainer());
    pnlDistortionInfo.add(btnDistortionFile);
    pnlDistortionInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlDistortionInfo.add(spnBinning.getContainer());
    pnlDistortionInfo.add(Box.createRigidArea(FixedDim.x5_y0));

    pnlMagGradientInfo.setLayout(new BoxLayout(pnlMagGradientInfo,
        BoxLayout.X_AXIS));
    pnlMagGradientInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlMagGradientInfo.add(ltfMagGradientFile.getContainer());
    pnlMagGradientInfo.add(btnMagGradientFile);
    pnlMagGradientInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    
    pnlImageRows.setLayout(new BoxLayout(pnlImageRows, BoxLayout.Y_AXIS));
    pnlImageRows.add(pnlStackInfo);
    pnlImageRows.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlImageRows.add(pnlDistortionInfo);
    pnlImageRows.add(pnlMagGradientInfo);


    pnlImageRows.setAlignmentY(Component.CENTER_ALIGNMENT);
    pnlImageParams.setLayout(new BoxLayout(pnlImageParams, BoxLayout.X_AXIS));

    pnlImageParams.add(pnlImageRows);
    pnlImageParams.add(Box.createHorizontalGlue());
    pnlImageParams.add(Box.createRigidArea(FixedDim.x5_y0));

    //  Create Data Parameters panel
    pnlDataParameters.setLayout(new BoxLayout(pnlDataParameters,
      BoxLayout.Y_AXIS));
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlDataset);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlDataType);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlImageParams);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
  }

  private void createPerAxisInfoPanel() {
    // Set the button size
    btnViewRawStackA.setSize();
    btnViewRawStackB.setSize();

    //  Tilt angle specification panels
    pnlAxisInfoA.setBorder(borderAxisInfoA.getBorder());
    pnlAxisInfoA.setLayout(new BoxLayout(pnlAxisInfoA, BoxLayout.Y_AXIS));
    tiltAnglesA.getPanel().setAlignmentX(Component.CENTER_ALIGNMENT);
    ltfExcludeListA.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnViewRawStackA.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlAxisInfoA.add(tiltAnglesA.getPanel());
    pnlAxisInfoA.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAxisInfoA.add(ltfExcludeListA.getContainer());
    pnlAxisInfoA.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAxisInfoA.add(btnViewRawStackA.getComponent());
    //Add adjusted focus checkbox
    pnlAdjustedFocusA.add(cbAdjustedFocusA);
    pnlAdjustedFocusA.setLayout(new BoxLayout(pnlAdjustedFocusA, BoxLayout.X_AXIS));
    cbAdjustedFocusA.setAlignmentX(Component.RIGHT_ALIGNMENT);
    cbAdjustedFocusA.setEnabled(false);
    pnlAxisInfoA.add(pnlAdjustedFocusA);

    pnlAxisInfoB.setBorder(borderAxisInfoB.getBorder());
    pnlAxisInfoB.setLayout(new BoxLayout(pnlAxisInfoB, BoxLayout.Y_AXIS));
    tiltAnglesB.getPanel().setAlignmentX(Component.CENTER_ALIGNMENT);
    ltfExcludeListB.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnViewRawStackB.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlAxisInfoB.add(tiltAnglesB.getPanel());
    pnlAxisInfoB.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAxisInfoB.add(ltfExcludeListB.getContainer());
    pnlAxisInfoB.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAxisInfoB.add(btnViewRawStackB.getComponent());
    cbAdjustedFocusB.setAlignmentX(Component.RIGHT_ALIGNMENT);
    //Add adjusted focus checkbox
    pnlAdjustedFocusB.add(cbAdjustedFocusB);
    pnlAdjustedFocusB.setLayout(new BoxLayout(pnlAdjustedFocusB, BoxLayout.X_AXIS));
    cbAdjustedFocusB.setAlignmentX(Component.RIGHT_ALIGNMENT);
    cbAdjustedFocusB.setEnabled(false);
    pnlAxisInfoB.add(pnlAdjustedFocusB);

    pnlPerAxisInfo.setLayout(new BoxLayout(pnlPerAxisInfo, BoxLayout.X_AXIS));
    pnlPerAxisInfo.add(pnlAxisInfoA);
    pnlPerAxisInfo.add(pnlAxisInfoB);
  }

  public Container getContainer() {
    return rootPanel;
  }

  public void initializeFields(ConstMetaData metaData) {

    if (!metaData.getDatasetName().equals("")) {
      String canonicalPath = applicationManager.getPropertyUserDir() + "/"
          + metaData.getDatasetName();
      ltfDataset.setText(canonicalPath);
    }

    ltfBackupDirectory.setText(metaData.getBackupDirectory());
    ltfDistortionFile.setText(metaData.getDistortionFile());
    ltfMagGradientFile.setText(metaData.getMagGradientFile());
    cbAdjustedFocusA.setSelected(metaData.getAdjustedFocusA().is());
    cbAdjustedFocusB.setSelected(metaData.getAdjustedFocusB().is());
    setAxisType(metaData.getAxisType());
    setViewType(metaData.getViewType());
    if(!Double.isNaN(metaData.getPixelSize())) {
      ltfPixelSize.setText(metaData.getPixelSize());
    }
    if(!Double.isNaN(metaData.getFiducialDiameter())) {
      ltfFiducialDiameter.setText(metaData.getFiducialDiameter());
    }
    if(!Float.isNaN(metaData.getImageRotation(AxisID.ONLY))) {
      ltfImageRotation.setText(metaData.getImageRotation(AxisID.ONLY));
    }
    spnBinning.setValue(new Integer(metaData.getBinning()));

    tiltAnglesA.setFields(metaData.getTiltAngleSpecA());
    ltfExcludeListA.setText(metaData.getExcludeProjectionsA());
    tiltAnglesB.setFields(metaData.getTiltAngleSpecB());
    ltfExcludeListB.setText(metaData.getExcludeProjectionsB());
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      tiltAnglesB.setEnabled(false);
      ltfExcludeListB.setEnabled(false);
      btnViewRawStackB.setEnabled(false);
    }
  }
  
  public String getDatasetString() {
    return ltfDataset.getText();
  }

  public MetaData getFields() {
    MetaData metaData = getDataset();
    metaData.setBackupDirectory(ltfBackupDirectory.getText());
    metaData.setDistortionFile(ltfDistortionFile.getText());
    metaData.setMagGradientFile(ltfMagGradientFile.getText());
    metaData.setAdjustedFocusA(cbAdjustedFocusA.isSelected());
    metaData.setAdjustedFocusB(cbAdjustedFocusB.isSelected());
    metaData.setViewType(getViewType());
    String currentField = "";
    try {
      currentField = "Pixel Size";
      metaData.setPixelSize(Double.parseDouble(ltfPixelSize.getText()));
      currentField = "Fiducial Diameter";
      metaData.setFiducialDiameter(Double.parseDouble(ltfFiducialDiameter
        .getText()));
      currentField = "Image Rotation";
      metaData.setImageRotation(Float.parseFloat(ltfImageRotation.getText()),
      AxisID.FIRST);
      if (getAxisType() == AxisType.DUAL_AXIS) {
        metaData.setImageRotation(Float.parseFloat(ltfImageRotation.getText()),
          AxisID.SECOND);
      } 
      currentField = "Axis A starting and step angles";
      tiltAnglesA.getFields(metaData.getTiltAngleSpecA());
      currentField = "Axis B starting and step angles";
      tiltAnglesB.getFields(metaData.getTiltAngleSpecB());
    }
    catch (NumberFormatException e) {
      uiHarness.openMessageDialog(
          currentField + " must be numeric.", "Setup Dialog Error", AxisID.ONLY);
      return null;
    }
    metaData.setBinning(((Integer) spnBinning.getValue()).intValue());
    metaData.setExcludeProjectionsA(ltfExcludeListA.getText());
    metaData.setExcludeProjectionsB(ltfExcludeListB.getText());
    return metaData;
  }
  
  public MetaData getDataset() {
    MetaData metaData = new MetaData(applicationManager);
    metaData.setAxisType(getAxisType());

    //  The dataset name needs to be set after the axis type so the metadata
    // object modifies the ending correctly
    if (ltfDataset.getText().startsWith("/")) {
      metaData.setDatasetName(ltfDataset.getText());
    }
    else {
      metaData.setDatasetName(applicationManager.getPropertyUserDir() + "/"
          + ltfDataset.getText());
    }
    return metaData;
  }


  public boolean isValid() {
    String errorMessageTitle = new String("Setup Dialog Error");
    String datasetText = ltfDataset.getText();
    String panelErrorMessage;

    if (datasetText.equals("")) {
      uiHarness.openMessageDialog(
          "Dataset name has not been entered.", errorMessageTitle, AxisID.ONLY);
      return false;
    }
    File dataset = new File(datasetText);
    String datasetFileName = dataset.getName();
    if (datasetFileName.equals("a.st") || datasetFileName.equals("b.st")
        || datasetFileName.equals(".")) {
      uiHarness.openMessageDialog(
          "The name " + datasetFileName + " cannot be used as a dataset name.",
          errorMessageTitle, AxisID.ONLY);
      return false;
    }
    //validate image distortion field file name
    //optional
    //file must exist
    String distortionFileText = ltfDistortionFile.getText();
    if (!distortionFileText.equals("")) {
      File distortionFile = new File(distortionFileText);
      if (!distortionFile.exists()) {
        String distortionFileName = distortionFile.getName();
        uiHarness.openMessageDialog(
            "The image distortion field file " + distortionFileName
                + " does not exist.", errorMessageTitle, AxisID.ONLY);
        return false;
      }
    }
    //validate mag gradient field file name
    //optional
    //file must exist
    String magGradientFileText = ltfMagGradientFile.getText();
    if (!magGradientFileText.equals("")) {
      File magGradientFile = new File(magGradientFileText);
      if (!magGradientFile.exists()) {
        String magGradientFileName = magGradientFile.getName();
        uiHarness.openMessageDialog(
            "The mag gradients correction file " + magGradientFileName
                + " does not exist.", errorMessageTitle, AxisID.ONLY);
        return false;
      }
    }
    panelErrorMessage = tiltAnglesA.getErrorMessage();
    if (panelErrorMessage != null) {
      uiHarness.openMessageDialog(
          panelErrorMessage + " in Axis A.", errorMessageTitle, AxisID.ONLY);
      return false;
    }
    panelErrorMessage = tiltAnglesB.getErrorMessage();
    if (panelErrorMessage != null) {
      uiHarness.openMessageDialog(
          panelErrorMessage + " in Axis B.", errorMessageTitle, AxisID.ONLY);
      return false;
    }

    return true;
  }

  // Return the working directory as a File object  
  public File getWorkingDirectory() {
    String datasetText = ltfDataset.getText();
    File dataset = new File(datasetText);
    if (!dataset.isAbsolute()) {

      dataset = new File(applicationManager.getPropertyUserDir() + File.separator
          + datasetText);
    }
    return dataset.getParentFile();
  }

  //  Axis type radio button
  void setAxisType(AxisType axisType) {
    if (axisType == AxisType.SINGLE_AXIS) {
      rbSingleAxis.setSelected(true);
    }
    else {
      rbDualAxis.setSelected(true);
    }
  }

  AxisType getAxisType() {
    if (rbSingleAxis.getSelectedObjects() != null) {
      return AxisType.SINGLE_AXIS;
    }
    else {
      return AxisType.DUAL_AXIS;
    }
  }

  //  View type radio button
  void setViewType(ViewType viewType) {
    if (viewType == ViewType.SINGLE_VIEW) {
      rbSingleView.setSelected(true);
    }
    else {
      rbMontage.setSelected(true);
    }
  }

  ViewType getViewType() {
    if (rbSingleView.getSelectedObjects() != null) {
      return ViewType.SINGLE_VIEW;
    }
    else {
      return ViewType.MONTAGE;
    }
  }

  /**
   * Right mouse button context menu
   **/
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
      "INITIAL STEPS");
  }

  //
  //  Action functions for buttons
  //
  private void btnDatasetAction(ActionEvent event) {
    //  Open up the file chooser in the working directory
    JFileChooser chooser = new JFileChooser(new File(applicationManager.getPropertyUserDir()));
    StackFileFilter stackFilter = new StackFileFilter();
    chooser.setFileFilter(stackFilter);
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File datasetName = chooser.getSelectedFile();
      try {
        ltfDataset.setText(datasetName.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  private void btnBackupDirectoryAction(ActionEvent event) {

    //  Open up the file chooser in the working directory
    String currentBackupDirectory = ltfBackupDirectory.getText();
    if (currentBackupDirectory.equals("")) {
      currentBackupDirectory = applicationManager.getPropertyUserDir();
    }
    JFileChooser chooser = new JFileChooser(new File(currentBackupDirectory));
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File backupDirectory = chooser.getSelectedFile();
      try {
        ltfBackupDirectory.setText(backupDirectory.getCanonicalPath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  private void btnDistortionFileAction(ActionEvent event) {
    //Open up the file chooser in the calibration directory, if available,
    //otherwise open in the working directory
    String currentDistortionDirectory = ltfDistortionFile.getText();
    if (currentDistortionDirectory.equals("")) {
      File calibrationDir = EtomoDirector.getInstance().getIMODCalibDirectory();
      File distortionDir = new File(calibrationDir.getAbsolutePath(),
        "Distortion");
      if (distortionDir.exists()) {
        currentDistortionDirectory = distortionDir.getAbsolutePath();
      }
      else {
        currentDistortionDirectory = applicationManager.getPropertyUserDir();
      }
    }
    JFileChooser chooser = new JFileChooser(
      new File(currentDistortionDirectory));
    DistortionFileFilter distortionFileFilter = new DistortionFileFilter();
    chooser.setFileFilter(distortionFileFilter);
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File distortionFile = chooser.getSelectedFile();
      try {
        ltfDistortionFile.setText(distortionFile.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  /**
   * Lets the user choose the mag gradients correction file.
   * @param event
   */
  private void btnMagGradientFileAction(ActionEvent event) {
    //Open up the file chooser in the calibration directory, if available,
    //otherwise open in the working directory
    String currentMagGradientDirectory = ltfMagGradientFile.getText();
    if (currentMagGradientDirectory.equals("")) {
      File calibrationDir = EtomoDirector.getInstance().getIMODCalibDirectory();
      File magGradientDir = new File(calibrationDir.getAbsolutePath(),
        "Distortion");
      if (magGradientDir.exists()) {
        currentMagGradientDirectory = magGradientDir.getAbsolutePath();
      }
      else {
        currentMagGradientDirectory = applicationManager.getPropertyUserDir();
      }
    }
    JFileChooser chooser = new JFileChooser(
      new File(currentMagGradientDirectory));
    MagGradientFileFilter magGradientFileFilter = new MagGradientFileFilter();
    chooser.setFileFilter(magGradientFileFilter);
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File magGradientFile = chooser.getSelectedFile();
      try {
        ltfMagGradientFile.setText(magGradientFile.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  private void rbSingleAxisAction(ActionEvent event) {
    tiltAnglesB.setEnabled(false);
    ltfExcludeListB.setEnabled(false);
    btnViewRawStackB.setEnabled(false);
  }

  private void rbDualAxisAction(ActionEvent event) {
    tiltAnglesB.setEnabled(true);
    ltfExcludeListB.setEnabled(true);
    btnViewRawStackB.setEnabled(true);
  }
  
  private void rbSingleViewAction(ActionEvent event) {
    cbAdjustedFocusA.setEnabled(false);
    cbAdjustedFocusB.setEnabled(false);
  }

  private void rbMontageAction(ActionEvent event) {
    cbAdjustedFocusA.setEnabled(true);
    cbAdjustedFocusB.setEnabled(true);
  }


  private void btnScanHeaderAction(ActionEvent event) {
    // Get the dataset name from the UI object
    String datasetName = ltfDataset.getText();
    if (datasetName == null || datasetName.equals("")) {
      uiHarness.openMessageDialog(
          "Dataset name has not been entered", "Missing dataset name", AxisID.ONLY);
      return;
    }
    //  Add the appropriate extension onto the filename if necessary 
    if (!datasetName.endsWith(".st")) {
      if (rbDualAxis.isSelected()) {
        datasetName = datasetName + "a.st";
      }
      else {
        datasetName = datasetName + ".st";

      }
    }

    // Run header on the dataset to the extract whatever information is
    // available
    MRCHeader header = MRCHeader.getInstance(applicationManager
        .getPropertyUserDir(), datasetName, AxisID.ONLY);
    try {
      header.read();
    }
    catch (InvalidParameterException except) {
      uiHarness.openMessageDialog(except.getMessage(),
          "Invalid Parameter Exception", AxisID.ONLY);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(),
          "IO Exception", AxisID.ONLY);
    }

    // Set the image rotation if available
    double imageRotation = header.getImageRotation();
    if (!Double.isNaN(imageRotation)) {
      ltfImageRotation.setText(imageRotation);
    }

    // set the pixel size if available
    double xPixelSize = header.getXPixelSize();
    double yPixelSize = header.getYPixelSize();
    if (Double.isNaN(xPixelSize) || Double.isNaN(yPixelSize)) {
      uiHarness.openMessageDialog(
        "Pixel size is not defined in the image file header",
        "Pixel size is missing", AxisID.ONLY);
      
      return;
    }

    if (xPixelSize != yPixelSize) {
      uiHarness.openMessageDialog(
          "X & Y pixels sizes are different, don't know what to do",
          "Pixel sizes are different", AxisID.ONLY);
      return;
    }
    if (xPixelSize == 1.0) {
      uiHarness.openMessageDialog(
          "Pixel size is not defined in the image file header",
          "Pixel size is missing", AxisID.ONLY);
      return;
    }
    ltfPixelSize.setText(xPixelSize / 10.0);
    int binning = header.getBinning();
    if (binning == Integer.MIN_VALUE) {
      binning = 1;
    }
    spnBinning.setValue(new Integer(binning));
  }

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOption menuOption) {
    if (button.equals(btnViewRawStackA)) {
      btnViewRawStackAAction(menuOption);
    }
    else if (button.equals(btnViewRawStackB)) {
      btnViewRawStackBAction(menuOption);
    }
  }
  
  private void btnViewRawStackAAction(Run3dmodMenuOption menuOption) {
    if (getAxisType() == AxisType.SINGLE_AXIS) {
      applicationManager.imodPreview(AxisID.ONLY, menuOption);
    }
    else {
      applicationManager.imodPreview(AxisID.FIRST, menuOption);
    }
  }

  private void btnViewRawStackBAction(Run3dmodMenuOption menuOption) {
    applicationManager.imodPreview(AxisID.SECOND, menuOption);
  }

  /**
   * Action to take when the cancel button is pressed, the default action is
   * to set the exitState attribute to CANCEL.
   */
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneSetupDialog();
  }

  /**
   * Action to take when the postpone button is pressed, the default action is
   * to set the exitState attribute to POSTPONE.
   */
  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneSetupDialog();
  }

  /**
   * Action to take when the execute button is pressed, the default action is
   * to set the exitState attribute to EXECUTE.
   */
  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneSetupDialog();
  }

  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "Enter the name of view data file(s). You can also select the view data file by pressing the folder button.";
    ltfDataset.setToolTipText(tooltipFormatter.setText(text).format());

    text = "This button will open a file chooser dialog box allowing you to select the view data file.";
    btnDataset.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Enter the name of the directory where you want the small data files .com and .log files to be backed up.  You can use the folder button on the right to create a new directory to store the backups.";
    ltfBackupDirectory.setToolTipText(tooltipFormatter.setText(text).format());

    text = "This button will open a file chooser dialog box allowing you to select and/or create the backup directory.";
    btnBackupDirectory.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Attempt to extract pixel size and tilt axis rotation angle from data stack.";
    btnScanHeader.setToolTipText(tooltipFormatter.setText(text).format());

    text = "This radio button selector will choose whether the data consists of one or two tilt axis.";
    pnlAxisType.setToolTipText(tooltipFormatter.setText(text).format());
    rbSingleAxis.setToolTipText(tooltipFormatter.setText(text).format());
    rbDualAxis.setToolTipText(tooltipFormatter.setText(text).format());

    text = "This radio button selector will choose whether the data consists of a single frame per view or multiple frames per view (montaged).";
    pnlViewType.setToolTipText(tooltipFormatter.setText(text).format());
    rbSingleView.setToolTipText(tooltipFormatter.setText(text).format());
    rbMontage.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Enter the view image pixel size in nanometers here.";
    ltfPixelSize.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Enter the fiducial size in nanometers here.";
    ltfFiducialDiameter.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Enter the view image rotation in degrees. This is the rotation (CCW positive) from the Y-axis (the tilt axis after the views are aligned) to the suspected tilt axis in the unaligned views.";
    ltfImageRotation.setToolTipText(tooltipFormatter.setText(text).format());

    text = "OPTIONAL: If you wish to correct for image distortion, enter the name of the appropriate image distortion file in this field and the CCD camera binning in the following spin control.";
    ltfDistortionFile.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Specify the binning in the CCD camera when the raw image stack was acquired.";
    spnBinning.setToolTipText(tooltipFormatter.setText(text).format());

    tiltAnglesA.setToolTipText();
    tiltAnglesB.setToolTipText();

    text = "Enter the view images to <b>exclude</b> from the processing of this axis.  Ranges are allowed, separate ranges by commas.  For example to exclude the first four and last four images of a 60 view stack enter 1-4,57-60.";
    ltfExcludeListA.setToolTipText(tooltipFormatter.setText(text).format());
    ltfExcludeListB.setToolTipText(tooltipFormatter.setText(text).format());

    text = "This button will setup the processing for existing command scripts.  <b>Be sure that parameters entered match the existing command scripts.";
    btnPostpone.setToolTipText(tooltipFormatter.setText(text).format());

    text = "This button will create a new set of command scripts overwriting any of the same name in the specified working directory.  Be sure to save the data file after creating the command script if you wish to keep the results.";
    btnExecute.setToolTipText(tooltipFormatter.setText(text).format());
  }

  //  Button action listener classes
  class DatasetActionListener implements ActionListener {

    SetupDialog adaptee;

    DatasetActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnDatasetAction(event);
    }
  }

  class BackupDirectoryActionListener implements ActionListener {

    SetupDialog adaptee;

    BackupDirectoryActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnBackupDirectoryAction(event);
    }
  }

  class DistortionFileActionListener implements ActionListener {

    SetupDialog adaptee;

    DistortionFileActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnDistortionFileAction(event);
    }
  }
  
  class MagGradientFileActionListener implements ActionListener {

    SetupDialog adaptee;

    MagGradientFileActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnMagGradientFileAction(event);
    }
  }


  class SingleAxisActionListener implements ActionListener {

    SetupDialog adaptee;

    SingleAxisActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.rbSingleAxisAction(event);
    }
  }

  class DualAxisActionListener implements ActionListener {

    SetupDialog adaptee;

    DualAxisActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.rbDualAxisAction(event);
    }
  }
  
  class SingleViewActionListener implements ActionListener {

    SetupDialog adaptee;

    SingleViewActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.rbSingleViewAction(event);
    }
  }

  class MontageActionListener implements ActionListener {

    SetupDialog adaptee;

    MontageActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.rbMontageAction(event);
    }
  }


  class ScanHeaderActionListener implements ActionListener {

    SetupDialog adaptee;

    ScanHeaderActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnScanHeaderAction(event);
    }
  }

  class ViewRawStackAActionListener implements ActionListener {
    SetupDialog adaptee;

    ViewRawStackAActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnViewRawStackAAction(Run3dmodMenuOption.NONE);
    }
  }

  class ViewRawStackBActionListener implements ActionListener {
    SetupDialog adaptee;

    ViewRawStackBActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnViewRawStackBAction(Run3dmodMenuOption.NONE);
    }
  }

}
/**
 * <p> $Log$
 * <p> Revision 3.33  2005/08/09 20:53:31  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * <p> MultiLineButton from JButton.
 * <p>
 * <p> Revision 3.32  2005/08/04 20:17:02  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.31  2005/07/29 00:54:40  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.30  2005/06/20 16:58:00  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in btnScanHeaderAction().
 * <p>
 * <p> Revision 3.29  2005/04/26 17:41:39  sueh
 * <p> bug# 615 Change the name of the UIHarness member variable to
 * <p> uiHarness.
 * <p>
 * <p> Revision 3.28  2005/04/25 21:38:48  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p>
 * <p> Revision 3.27  2005/04/21 20:47:00  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.26  2005/04/16 02:04:12  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.25  2005/03/10 18:50:22  sueh
 * <p> bug# 533 Removed new stuff option from montaging functionality.
 * <p>
 * <p> Revision 3.24  2005/03/02 23:15:56  sueh
 * <p> bug# 533 In setup dialog, put the Scan Header button with the header fields.
 * <p>
 * <p> Revision 3.23  2005/03/02 20:26:56  sueh
 * <p> bug# 533 Temporary:  enabling montaging option when
 * <p> EtomoDirector.newstuff is true.  Bug# 533 Added adjustedFocus.  Only
 * <p> used with montaging and mag gradient correction.
 * <p>
 * <p> Revision 3.22  2005/03/02 00:13:29  sueh
 * <p> bug# 611 Added mag gradients correction file.  Bug# 533 Enabled montaging.
 * <p>
 * <p> Revision 3.21  2005/02/15 21:06:43  sueh
 * <p> bug# 603 Removed SectionType radio buttons (single or serial sections)
 * <p> because serial sections are handled by the join interface.
 * <p>
 * <p> Revision 3.20  2005/02/07 22:55:44  sueh
 * <p> bug# 594 Added setDatasetString() to return the value of the dataset field.
 * <p>
 * <p> Revision 3.19  2005/01/14 03:11:06  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.18  2004/11/20 00:04:14  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.17.2.3  2004/10/11 02:18:09  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.17.2.2  2004/10/08 16:41:27  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 3.17.2.1  2004/09/15 22:47:53  sueh
 * <p> bug# 520 call openMessageDialog in mainPanel instead of mainFrame.
 * <p>
 * <p> Revision 3.17  2004/08/20 23:07:58  sueh
 * <p> bug# 515 add fields names for error messages about tilt angles
 * <p> Changed:
 * <p> getFields()
 * <p>
 * <p> Revision 3.16  2004/08/20 22:56:19  sueh
 * <p> bug# 515 catching exceptions on numeric fields
 * <p> Changed:
 * <p> getFields()
 * <p>
 * <p> Revision 3.15  2004/07/12 17:39:08  sueh
 * <p> bug# 492 add getDataset() to return a MetaData with the
 * <p> minumum fields needed to run 3dmod
 * <p>
 * <p> Revision 3.14  2004/06/17 18:49:38  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.13  2004/04/26 03:17:54  rickg
 * <p> Normalized button size
 * <p>
 * <p> Revision 3.12  2004/04/06 04:13:28  rickg
 * <p> Updated imageRotation to store axis separately
 * <p>
 * <p> Revision 3.11  2004/03/24 03:02:45  rickg
 * <p> Changed spinner size to only specify spinner region.  The
 * <p> panel and label should be handled automatically
 * <p>
 * <p> Revision 3.10  2004/03/16 00:55:55  rickg
 * <p> Bug# 411 re-layout setup page
 * <p> Add tooltips for image distortion and binning
 * <p>
 * <p> Revision 3.9  2004/03/15 23:14:10  sueh
 * <p> progress button names changed to "btn"
 * <p>
 * <p> Revision 3.8  2004/03/11 01:13:09  sueh
 * <p> bug# 386 retrieved binning from MRCHeader
 * <p>
 * <p> Revision 3.7  2004/03/10 00:43:14  sueh
 * <p> bug# 408 opening distortion file chooser in $IMOD_CALIB_DIR/Distortion, if
 * <p> possible
 * <p>
 * <p> Revision 3.6  2004/02/23 18:22:13  sueh
 * <p> bug# 386 Make distortion file optional
 * <p>
 * <p> Revision 3.5  2004/02/21 00:31:22  sueh
 * <p> bug# 386 validate distortion file
 * <p>
 * <p> Revision 3.4  2004/02/20 23:51:01  sueh
 * <p> bug# 386 added distortionFile chooser and binning spinner
 * <p>
 * <p> Revision 3.3  2004/02/08 18:34:40  sueh
 * <p> bug# 169 Calling imodPreview instead of imodRawStack.
 * <p>
 * <p> Revision 3.2  2003/12/08 22:33:51  sueh
 * <p> bug# 169 adding ViewRawStack button for axis A and B
 * <p>
 * <p> Revision 3.1  2003/11/10 18:50:47  sueh
 * <p> bug332 isValid(): Added call to
 * <p> TiltAngleDialogPanel.getErrorMessage() for Axis' A and B.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.16  2003/11/06 22:45:27  sueh
 * <p> cleaning up task tags and prints
 * <p>
 * <p> Revision 2.15  2003/10/24 00:34:28  sueh
 * <p> Bug271 Prevent dataset name from being "."
 * <p>
 * <p> Revision 2.14  2003/10/23 23:07:44  sueh
 * <p> bug271 added isValid() to contain all SetupDialog validation
 * <p>
 * <p> Revision 2.13  2003/10/23 22:08:28  sueh
 * <p> Bug322 changed labels and a tooltip.
 * <p>
 * <p> Revision 2.12  2003/10/10 22:56:59  sueh
 * <p> bug265
 * <p> changed file.pathSeparator (???:???) to file.separator (???/???)
 * <p>
 * <p> Revision 2.11  2003/10/09 20:27:43  sueh
 * <p> bug264
 * <p> UI Changes
 * <p>
 * <p> Revision 2.10  2003/10/08 22:03:21  sueh
 * <p> Bug263
 * <p> UI Changes
 * <p> Removed data source from Setup dialog.  Removed setDataSource() from MetaData.
 * <p> DataSource is always the default (CCD) in ConstMetaData
 * <p> Grayed out ViewType.
 * <p>
 * <p> Revision 2.9  2003/10/08 21:11:41  sueh
 * <p> bug262
 * <p> UI Change
 * <p> Changed View Type radio button choice
 * <p> from Single View to Single Frame on the Setup dialog.
 * <p>
 * <p> Revision 2.8  2003/10/08 19:12:50  sueh
 * <p> bug261 change changes on the screen:
 * <p> projection -> view
 * <p> raw stack data -> raw image stack
 * <p>
 * <p> Revision 2.7  2003/06/03 23:28:26  rickg
 * <p> Fixed font size ltf at 5 columns for text boxes
 * <p>
 * <p> Revision 2.6  2003/05/20 21:32:54  rickg
 * <p> Added scan header button
 * <p>
 * <p> Revision 2.5  2003/05/12 01:32:25  rickg
 * <p> Working directory calculation works both unix and windows now
 * <p>
 * <p> Revision 2.4  2003/05/07 23:36:12  rickg
 * <p> Updated Data Source labels and tooltips
 * <p>
 * <p> Revision 2.3  2003/05/07 17:47:46  rickg
 * <p> System property user.dir now defines the working directory
 * <p> Added method to get working directy name from current dataset
 * <p> Fixed some tooltips
 * <p>
 * <p> Revision 2.2  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.14.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.14  2003/01/06 20:43:26  rickg
 * <p> Fixed direct entry of fileset name, working directory
 * <p> is taken from app manager
 * <p>
 * <p> Revision 1.13  2003/01/06 20:18:33  rickg
 * <p> Changed edit boxes to LabeledTextFields
 * <p>
 * <p> Revision 1.12  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.11  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.10  2002/12/09 04:17:07  rickg
 * <p> Added stack file filter to open dialog
 * <p>
 * <p> Revision 1.9  2002/11/19 02:34:24  rickg
 * <p> Tooltip spelling correction
 * <p>
 * <p> Revision 1.8  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.7  2002/11/14 04:21:47  rickg
 * <p> HTMLPage and ContextPopup now work with URLS
 * <p>
 * <p> Revision 1.6  2002/10/24 21:12:29  rickg
 * <p> Got folder icon working
 * <p>
 * <p> Revision 1.5  2002/10/24 19:54:52  rickg
 * <p> Moved fileset specification to after axis type specification
 * <p>
 * <p> Revision 1.4  2002/10/22 23:26:22  rickg
 * <p> Merged directory and fileset name to a single UI entity
 * <p>
 * <p> Revision 1.3  2002/10/17 22:40:55  rickg
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
*/