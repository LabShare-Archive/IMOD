package etomo.ui;

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
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import etomo.ApplicationManager;
import etomo.storage.StackFileFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstMetaData;
import etomo.type.MetaData;
import etomo.type.SectionType;
import etomo.type.ViewType;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

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
 *
 * <p> $Log$
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
 * <p> changed file.pathSeparator (“:”) to file.separator (“/”)
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
public class SetupDialog extends ProcessDialog implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel pnlDataParameters = new JPanel();

  //  Dataset GUI objects
  private JPanel pnlDataset = new JPanel();
  private ImageIcon iconFolder =
    new ImageIcon(ClassLoader.getSystemResource("images/openFile.gif"));

  private LabeledTextField ltfDataset = new LabeledTextField("Dataset name: ");
  private JButton btnDataset = new JButton(iconFolder);

  private LabeledTextField ltfBackupDirectory =
    new LabeledTextField("Backup directory: ");
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

  private JPanel pnlSectionType = new JPanel();
  private JRadioButton rbSingleSection = new JRadioButton("Single tomogram");
  private JRadioButton rbSerialSection = new JRadioButton("Serial tomogram");
  private ButtonGroup bgSectionType = new ButtonGroup();

  private JPanel pnlPixelAndLocalAlign = new JPanel();
  private JButton btnScanHeader = new JButton("Scan Header");
  private LabeledTextField ltfPixelSize =
    new LabeledTextField("Pixel size (nm): ");
  private LabeledTextField ltfFiducialDiameter =
    new LabeledTextField("Fiducial diameter (nm): ");
  private LabeledTextField ltfImageRotation =
    new LabeledTextField("Image rotation (degrees): ");

  //  Tilt angle GUI objects
  private JPanel pnlPerAxisInfo = new JPanel();
  private JPanel pnlAxisInfoA = new JPanel();
  private BeveledBorder borderAxisInfoA = new BeveledBorder("Axis A: ");
  private TiltAngleDialogPanel tiltAnglesA = new TiltAngleDialogPanel();
  private LabeledTextField ltfExcludeListA =
    new LabeledTextField("Exclude views: ");

  private JPanel pnlAxisInfoB = new JPanel();
  private BeveledBorder borderAxisInfoB = new BeveledBorder("Axis B: ");
  private TiltAngleDialogPanel tiltAnglesB = new TiltAngleDialogPanel();
  private LabeledTextField ltfExcludeListB =
    new LabeledTextField("Exclude views: ");

  //  Construct the setup dialog
  public SetupDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.ONLY);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    createDatasetPanel();

    createDataTypePanel();

    createPerAxisInfoPanel();

    //  Relabel the postpone button
    buttonPostpone.setText("Use Existing Coms");
    buttonExecute.setText("Create Com Scripts");

    // There are no advanced settings for this dialog, remove the advanced
    // button
    panelExitButtons.remove(buttonAdvanced);

    //  Add the panes to the dialog box
    rootPanel.add(pnlDataParameters);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlPerAxisInfo);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Calcute the necessary window size
    applicationManager.packMainWindow();
  }

  private void createDatasetPanel() {

    //  Set the preferred and max sizes for the dataset GUI objects
    //  so that the box layout happens correctly
    btnDataset.setPreferredSize(FixedDim.folderButton);
    btnDataset.setMaximumSize(FixedDim.folderButton);
    btnBackupDirectory.setPreferredSize(FixedDim.folderButton);
    btnBackupDirectory.setMaximumSize(FixedDim.folderButton);

    pnlDataset.setLayout(new BoxLayout(pnlDataset, BoxLayout.X_AXIS));

    //  Bind the buttons to their adapters
    btnDataset.addActionListener(new DatasetActionListener(this));
    btnBackupDirectory.addActionListener(
      new BackupDirectoryActionListener(this));
    rbSingleAxis.addActionListener(new SingleAxisActionListener(this));
    rbDualAxis.addActionListener(new DualAxisActionListener(this));
    btnScanHeader.addActionListener(new ScanHeaderActionListener(this));

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

    //  Datatype subpnls: DataSource AxisType Viewtype SectionType
    Dimension dimDataTypePref = new Dimension(150, 80);

    bgAxisType.add(rbSingleAxis);
    bgAxisType.add(rbDualAxis);
    pnlAxisType.setLayout(new BoxLayout(pnlAxisType, BoxLayout.Y_AXIS));
    pnlAxisType.setPreferredSize(dimDataTypePref);
    pnlAxisType.setBorder(new EtchedBorder("Axis Type").getBorder());
    pnlAxisType.add(rbSingleAxis);
    pnlAxisType.add(rbDualAxis);
    rbSingleView.setEnabled(false);
    rbMontage.setEnabled(false);
    
    bgViewType.add(rbSingleView);
    bgViewType.add(rbMontage);
    pnlViewType.setLayout(new BoxLayout(pnlViewType, BoxLayout.Y_AXIS));
    pnlViewType.setPreferredSize(dimDataTypePref);
    pnlViewType.setBorder(new EtchedBorder("Frame Type").getBorder());
    pnlViewType.add(rbSingleView);
    pnlViewType.add(rbMontage);

    bgSectionType.add(rbSerialSection);
    bgSectionType.add(rbSingleSection);
    pnlSectionType.setLayout(new BoxLayout(pnlSectionType, BoxLayout.Y_AXIS));
    pnlSectionType.setPreferredSize(dimDataTypePref);
    pnlSectionType.setBorder(new EtchedBorder("Tomogram Type").getBorder());
    pnlSectionType.add(rbSingleSection);
    pnlSectionType.add(rbSerialSection);

    rbSingleSection.setEnabled(false);
    rbSerialSection.setEnabled(false);

    //  Datatype panel
    pnlDataType.setLayout(new BoxLayout(pnlDataType, BoxLayout.X_AXIS));
    pnlDataType.setBorder(new EtchedBorder("Data Type").getBorder());
    pnlDataType.add(pnlAxisType);
    pnlDataType.add(Box.createHorizontalGlue());
    pnlDataType.add(pnlViewType);
    pnlDataType.add(Box.createHorizontalGlue());
    pnlDataType.add(pnlSectionType);

    //  Pixel & Alignment panel
    ltfPixelSize.setColumns(5);
    ltfFiducialDiameter.setColumns(5);
    ltfImageRotation.setColumns(5);
    pnlPixelAndLocalAlign.setLayout(
      new BoxLayout(pnlPixelAndLocalAlign, BoxLayout.X_AXIS));
    pnlPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlPixelAndLocalAlign.add(Box.createHorizontalGlue());
    pnlPixelAndLocalAlign.add(btnScanHeader);
    pnlPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPixelAndLocalAlign.add(Box.createHorizontalGlue());
    pnlPixelAndLocalAlign.add(ltfPixelSize.getContainer());
    pnlPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPixelAndLocalAlign.add(Box.createHorizontalGlue());
    pnlPixelAndLocalAlign.add(ltfFiducialDiameter.getContainer());
    pnlPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPixelAndLocalAlign.add(Box.createHorizontalGlue());
    pnlPixelAndLocalAlign.add(ltfImageRotation.getContainer());
    pnlPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPixelAndLocalAlign.add(Box.createHorizontalGlue());
    pnlPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x5_y0));

    //  Create Data Parameters panel
    pnlDataParameters.setLayout(
      new BoxLayout(pnlDataParameters, BoxLayout.Y_AXIS));
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlDataset);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlDataType);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlPixelAndLocalAlign);
    pnlDataParameters.add(Box.createHorizontalGlue());
  }

  private void createPerAxisInfoPanel() {

    //  Tilt angle specification pnl
    pnlAxisInfoA.setBorder(borderAxisInfoA.getBorder());
    pnlAxisInfoA.setLayout(new BoxLayout(pnlAxisInfoA, BoxLayout.Y_AXIS));

    pnlAxisInfoA.add(tiltAnglesA.getPanel());
    pnlAxisInfoA.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAxisInfoA.add(ltfExcludeListA.getContainer());

    pnlAxisInfoB.setBorder(borderAxisInfoB.getBorder());
    pnlAxisInfoB.setLayout(new BoxLayout(pnlAxisInfoB, BoxLayout.Y_AXIS));

    pnlAxisInfoB.add(tiltAnglesB.getPanel());
    pnlAxisInfoB.add(ltfExcludeListB.getContainer());

    pnlPerAxisInfo.setLayout(new BoxLayout(pnlPerAxisInfo, BoxLayout.X_AXIS));
    pnlPerAxisInfo.add(pnlAxisInfoA);
    pnlPerAxisInfo.add(pnlAxisInfoB);
  }

  public Container getContainer() {
    return rootPanel;
  }

  public void initializeFields(ConstMetaData metaData) {

    if (!metaData.getDatasetName().equals("")) {
      String canonicalPath =
        System.getProperty("user.dir") + "/" + metaData.getDatasetName();
      ltfDataset.setText(canonicalPath);
    }

    ltfBackupDirectory.setText(metaData.getBackupDirectory());
    setAxisType(metaData.getAxisType());
    setViewType(metaData.getViewType());
    setSectionType(metaData.getSectionType());
    ltfPixelSize.setText(metaData.getPixelSize());
    ltfFiducialDiameter.setText(metaData.getFiducialDiameter());
    ltfImageRotation.setText(metaData.getImageRotation());

    tiltAnglesA.setFields(metaData.getTiltAngleSpecA());
    ltfExcludeListA.setText(metaData.getExcludeProjectionsA());
    tiltAnglesB.setFields(metaData.getTiltAngleSpecB());
    ltfExcludeListB.setText(metaData.getExcludeProjectionsB());
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      tiltAnglesB.setEnabled(false);
      ltfExcludeListB.setEnabled(false);
    }
  }

  public MetaData getFields() {
    MetaData metaData = new MetaData();

    metaData.setBackupDirectory(ltfBackupDirectory.getText());
    metaData.setAxisType(getAxisType());

    //  The dataset name needs to be set after the axis type so the metadata
    // object modifies the ending correctly
    if (ltfDataset.getText().startsWith("/")) {
      metaData.setDatasetName(ltfDataset.getText());
    }
    else {
      metaData.setDatasetName(
        System.getProperty("user.dir") + "/" + ltfDataset.getText());
    }
    metaData.setViewType(getViewType());
    metaData.setSectionType(getSectionType());
    metaData.setPixelSize(Double.parseDouble(ltfPixelSize.getText()));
    metaData.setFiducialDiameter(
      Double.parseDouble(ltfFiducialDiameter.getText()));
    metaData.setImageRotation(Double.parseDouble(ltfImageRotation.getText()));
    tiltAnglesA.getFields(metaData.getTiltAngleSpecA());
    metaData.setExcludeProjectionsA(ltfExcludeListA.getText());
    tiltAnglesB.getFields(metaData.getTiltAngleSpecB());
    metaData.setExcludeProjectionsB(ltfExcludeListB.getText());
    return metaData;
  }

  public boolean isValid() {
    String errorMessageTitle = new String("Setup Dialog Error");
    String datasetText = ltfDataset.getText();
   
    if (datasetText.equals("")) {
      applicationManager.openMessageDialog(
        "Dataset name has not been entered",
        errorMessageTitle);
      return false;
    }
    File dataset = new File(datasetText);
    String datasetFileName = dataset.getName();
    if (datasetFileName.equals("a.st") || datasetFileName.equals("b.st") ||
        datasetFileName.equals(".")) {
      applicationManager.openMessageDialog(
        "The name " + datasetFileName + " cannot be used as a dataset name",
        errorMessageTitle);
      return false;
    }
    return true;
    
  }
  // Return the working directory as a File object  
  public File getWorkingDirectory() {
    String datasetText = ltfDataset.getText();
    File dataset = new File(datasetText);
    if (!dataset.isAbsolute()) {

      dataset =
        new File(
          System.getProperty("user.dir") + File.separator + datasetText);
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

  //  Section type radio button
  void setSectionType(SectionType sectionType) {
    if (sectionType == SectionType.SINGLE) {
      rbSingleSection.setSelected(true);
    }
    else {
      rbSerialSection.setSelected(true);
    }
  }

  SectionType getSectionType() {
    if (rbSingleSection.getSelectedObjects() != null) {
      return SectionType.SINGLE;
    }
    else {
      return SectionType.SERIAL;
    }
  }

  /**
   * Right mouse button context menu
   **/
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup =
      new ContextPopup(rootPanel, mouseEvent, "INITIAL STEPS");
  }

  //
  //  Action functions for buttons
  //
  private void btnDatasetAction(ActionEvent event) {
    //  Open up the file chooser in the working directory
    JFileChooser chooser =
      new JFileChooser(new File(System.getProperty("user.dir")));
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
      currentBackupDirectory = System.getProperty("user.dir");
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

  private void rbSingleAxisAction(ActionEvent event) {
    tiltAnglesB.setEnabled(false);
    ltfExcludeListB.setEnabled(false);
  }

  private void rbDualAxisAction(ActionEvent event) {
    tiltAnglesB.setEnabled(true);
    ltfExcludeListB.setEnabled(true);
  }

  private void btnScanHeaderAction(ActionEvent event) {
    // Get the dataset name from the UI object
    String datasetName = ltfDataset.getText();
    if (datasetName == null || datasetName.equals("")) {
      applicationManager.openMessageDialog(
        "Dataset name has not been entered",
        "Missing dataset name");
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
    MRCHeader header = new MRCHeader(datasetName);
    try {
      header.read();
    }
    catch (InvalidParameterException except) {
      applicationManager.openMessageDialog(
        except.getMessage(),
        "Invalid Parameter Exception");
    }
    catch (IOException except) {
      applicationManager.openMessageDialog(except.getMessage(), "IO Exception");
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
      applicationManager.openMessageDialog(
        "Pixel size is not defined in the image file header",
        "Pixel size is missing");
      return;
    }

    if (xPixelSize != yPixelSize) {
      applicationManager.openMessageDialog(
        "X & Y pixels sizes are different, don't know what to do",
        "Pixel sizes are different");
      return;
    }
    if (xPixelSize == 1.0) {
      applicationManager.openMessageDialog(
        "X & Y pixels sizes are not defined",
        "Pixel sizes are not defined");
      return;
    }
    ltfPixelSize.setText(xPixelSize / 10.0);
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
    String line1, line2, line3, line4;
    line1 = "<html>Enter the name of view data file(s). You can<br>";
    line2 = "also select the view data file  by pressing the<br>";
    line3 = "folder button.";
    ltfDataset.setToolTipText(line1 + line2 + line3);

    line1 = "<html>This button will open a file chooser dialog box<br>";
    line2 = "allowing you to select the view data file.";
    btnDataset.setToolTipText(line1 + line2);

    line1 = "<html>Enter the name of the directory where you want the<br>";
    line2 = "small data files .com and .log files to be backed up.  You<br>";
    line3 = "can use the folder button on the right to create a new<br>";
    line4 = "directory to store the backups.";
    ltfBackupDirectory.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>This button will open a file chooser dialog box<br>";
    line2 = "allowing you to select and/or create the backup directory.";
    btnBackupDirectory.setToolTipText(line1 + line2);
    
    line1 = "<html>Attempt to extract pixel size and tilt axis rotation<br>";
    line2 = "angle from data stack.";
    btnScanHeader.setToolTipText(line1 + line2);

    line1 = "<html>This radio button selector will choose whether the data<br>";
    line2 = "consists of one or two tilt axis.";
    pnlAxisType.setToolTipText(line1 + line2);
    rbSingleAxis.setToolTipText(line1 + line2);
    rbDualAxis.setToolTipText(line1 + line2);

    line1 = "<html>This radio button selector will choose whether the data<br>";
    line2 = "consists of a single frame per view or multiple frames<br>";
    line3 = "per view (montaged).";
    pnlViewType.setToolTipText(line1 + line2 + line3);
    rbSingleView.setToolTipText(line1 + line2 + line3);
    rbMontage.setToolTipText(line1 + line2 + line3);

    line1 = "<html>This radio button selector will choose whether the data<br>";
    line2 = "consists of a single tomogram or several serial tomograms.";
    pnlSectionType.setToolTipText(line1 + line2);
    rbSingleSection.setToolTipText(line1 + line2);
    rbSerialSection.setToolTipText(line1 + line2);
    
    line1 = "<html>Enter the view image pixel size in nanometers here.";
    ltfPixelSize.setToolTipText(line1);

    line1 = "<html>Enter the fiducial size in nanometers here.";
    ltfFiducialDiameter.setToolTipText(line1);

    line1 = "<html>Enter the view image rotation in degrees. This is<br>";
    line2 = "the rotation (CCW positive) from the Y-axis (the tilt axis<br>";
    line3 = "after the views are aligned) to the suspected tilt axis in<br>";
    line4 = "the unaligned views.";
    ltfImageRotation.setToolTipText(line1 + line2 + line3 + line4);
    
    tiltAnglesA.setToolTipText();
    tiltAnglesB.setToolTipText();
    
    line1 =
      "<html>Enter the view images to <b>exclude</b> from the processing<br>";
    line2 = "of this axis.  Ranges are allowed, separate ranges by<br>";
    line3 = "commas.  For example to exclude the first four and last<br>";
    line4 = "four images of a 60 view stack enter 1-4,57-60.";
    ltfExcludeListA.setToolTipText(line1 + line2 + line3 + line4);
    ltfExcludeListB.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>This button will setup the processing for existing<br>";
    line2 = "command scripts.  <b>Be sure that parameters entered match<br>";
    line3 = "the existing command scripts.</b>";
    buttonPostpone.setToolTipText(line1 + line2 + line3);

    line1 = "<html>This button will create a new set of command scripts<br>";
    line2 = "overwriting any of the same name in the specified working<br>";
    line3 = "directory.  Be sure to save the data file after creating the<br>";
    line4 = "command script if you wish to keep the results.";
    buttonExecute.setToolTipText(line1 + line2 + line3 + line4);
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

  class ScanHeaderActionListener implements ActionListener {

    SetupDialog adaptee;
    ScanHeaderActionListener(SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnScanHeaderAction(event);
    }
  }

}
