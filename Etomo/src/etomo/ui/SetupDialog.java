package etomo.ui;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

import etomo.*;
import etomo.type.*;

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

  private ApplicationManager applicationManager;
  private JPanel panelSetup;
  private JPanel panelDataParameters = new JPanel();

  //
  //  Fileset GUI objects
  //
  private JPanel panelFileset = new JPanel();
  private ImageIcon iconFolder =
    new ImageIcon(ClassLoader.getSystemResource("images/openFile.gif"));

  private JLabel labelFileset = new JLabel("Fileset Name:");
  private JTextField textFieldFileset = new JTextField();
  private JButton buttonFileset = new JButton(iconFolder);

  private JLabel labelBackupDirectory = new JLabel("Backup Directory:");
  private JTextField textFieldBackupDirectory = new JTextField();
  private JButton buttonBackupDirectory = new JButton(iconFolder);

  //
  //  Data type GUI objects
  //
  private JPanel panelDataType = new JPanel();
  private TitledBorder borderDataType;

  private JPanel panelDataSource = new JPanel();
  private TitledBorder borderDataSource;
  private JRadioButton rbCCD = new JRadioButton("CCD");
  private JRadioButton rbFilm = new JRadioButton("Film");
  private ButtonGroup bgDataSource = new ButtonGroup();

  private JPanel panelAxisType = new JPanel();
  private TitledBorder borderAxisType;
  private JRadioButton rbSingleAxis = new JRadioButton("Single Axis");
  private JRadioButton rbDualAxis = new JRadioButton("Dual Axis");
  private ButtonGroup bgAxisType = new ButtonGroup();

  private JPanel panelViewType = new JPanel();
  private TitledBorder borderViewType;
  private JRadioButton rbSingleView = new JRadioButton("Single View");
  private JRadioButton rbMontage = new JRadioButton("Montage");
  private ButtonGroup bgViewType = new ButtonGroup();

  private JPanel panelSectionType = new JPanel();
  private TitledBorder borderSectionType;
  private JRadioButton rbSingleSection = new JRadioButton("Single Tomogram");
  private JRadioButton rbSerialSection = new JRadioButton("Serial Tomogram");
  private ButtonGroup bgSectionType = new ButtonGroup();

  private JPanel panelPixelAndLocalAlign = new JPanel();
  private LabeledTextField ltfPixelSize =
    new LabeledTextField("Pixel size (nm) :");
  private LabeledTextField ltfFiducialDiameter =
    new LabeledTextField("Fiducial marker diameter (nm) :");
  private LabeledTextField ltfImageRotation =
    new LabeledTextField("Image rotation (degrees) :");

  //
  //  Tilt angle GUI objects
  //
  private JPanel panelPerAxisInfo = new JPanel();
  private JPanel panelAxisInfoA = new JPanel();
  private BeveledBorder borderAxisInfoA = new BeveledBorder("Axis A:");
  private TiltAngleDialogPanel tiltAnglesA = new TiltAngleDialogPanel();
  private LabeledTextField ltfExcludeListA =
    new LabeledTextField("Exclude projections :");

  private JPanel panelAxisInfoB = new JPanel();
  private BeveledBorder borderAxisInfoB = new BeveledBorder("Axis B:");
  private TiltAngleDialogPanel tiltAnglesB = new TiltAngleDialogPanel();
  private LabeledTextField ltfExcludeListB =
    new LabeledTextField("Exclude projections :");

  //
  //  Construct the setup dialog
  //
  public SetupDialog(ApplicationManager appMgr) {
    applicationManager = appMgr;

    panelSetup = (JPanel) getContentPane();
    panelSetup.setLayout(new BoxLayout(panelSetup, BoxLayout.Y_AXIS));

    setTitle("eTomo Setup");

    createFilesetPanel();

    createDataTypePanel();

    createPerAxisInfoPanel();

    //  Relabel the postpone button
    buttonPostpone.setText("Use existing coms");
    buttonExecute.setText("Create com scripts");

    //  Add the panes to the dialog box
    panelSetup.add(panelDataParameters);
    panelSetup.add(Box.createVerticalGlue());
    panelSetup.add(Box.createRigidArea(FixedDim.x0_y10));
    panelSetup.add(panelPerAxisInfo);
    panelSetup.add(Box.createVerticalGlue());
    panelSetup.add(Box.createRigidArea(FixedDim.x0_y10));
    panelSetup.add(panelExitButtons);
    panelSetup.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelSetup.addMouseListener(mouseAdapter);

    // Calcute the necessary window size
    pack();
  }

  private void createFilesetPanel() {
    //
    //  Set the preferred and max sizes for the fileset GUI objects
    //  so that the box layout happens correctly
    //
    Dimension dimTextPref = new Dimension(120, 20);
    Dimension dimTextMax = new Dimension(1000, 20);
    textFieldFileset.setPreferredSize(dimTextPref);
    textFieldFileset.setMaximumSize(dimTextMax);
    textFieldBackupDirectory.setPreferredSize(dimTextPref);
    textFieldBackupDirectory.setMaximumSize(dimTextMax);
    buttonFileset.setPreferredSize(FixedDim.folderButton);
    buttonFileset.setMaximumSize(FixedDim.folderButton);
    buttonBackupDirectory.setPreferredSize(FixedDim.folderButton);
    buttonBackupDirectory.setMaximumSize(FixedDim.folderButton);

    panelFileset.setLayout(new BoxLayout(panelFileset, BoxLayout.X_AXIS));

    //
    //  Bind the buttons to their adapters
    //
    buttonFileset.addActionListener(new SetupDialogFilesetActionAdapter(this));
    buttonBackupDirectory.addActionListener(
      new SetupDialogBackupDirectoryActionAdapter(this));
    rbSingleAxis.addActionListener(
      new SetupDialogSingleAxisActionAdapter(this));
    rbDualAxis.addActionListener(new SetupDialogDualAxisActionAdapter(this));

    //
    //  Add the GUI objects to the panel
    //
    panelFileset.add(Box.createRigidArea(FixedDim.x5_y0));

    panelFileset.add(labelFileset);
    panelFileset.add(Box.createRigidArea(FixedDim.x5_y0));
    panelFileset.add(textFieldFileset);
    panelFileset.add(buttonFileset);
    panelFileset.add(Box.createRigidArea(FixedDim.x10_y0));

    panelFileset.add(labelBackupDirectory);
    panelFileset.add(Box.createRigidArea(FixedDim.x5_y0));
    panelFileset.add(textFieldBackupDirectory);
    panelFileset.add(buttonBackupDirectory);
    panelFileset.add(Box.createRigidArea(FixedDim.x5_y0));

    //  Add the tooltip text
    setToolTipText();
  }

  private void createDataTypePanel() {
    //
    //  Datatype subpanels: DataSource AxisType Viewtype SectionType
    //
    Dimension dimDataTypePref = new Dimension(150, 80);
    bgDataSource.add(rbCCD);
    bgDataSource.add(rbFilm);
    panelDataSource.setLayout(new BoxLayout(panelDataSource, BoxLayout.Y_AXIS));
    panelDataSource.setPreferredSize(dimDataTypePref);
    borderDataSource =
      new TitledBorder(
        BorderFactory.createEtchedBorder(
          new Color(248, 254, 255),
          new Color(121, 124, 136)),
        "Data Source");
    panelDataSource.setBorder(borderDataSource);
    panelDataSource.add(rbCCD);
    panelDataSource.add(rbFilm);

    bgAxisType.add(rbSingleAxis);
    bgAxisType.add(rbDualAxis);
    panelAxisType.setLayout(new BoxLayout(panelAxisType, BoxLayout.Y_AXIS));
    borderAxisType =
      new TitledBorder(
        BorderFactory.createEtchedBorder(
          new Color(248, 254, 255),
          new Color(121, 124, 136)),
        "Axis Type");
    panelAxisType.setPreferredSize(dimDataTypePref);
    panelAxisType.setBorder(borderAxisType);
    panelAxisType.add(rbSingleAxis);
    panelAxisType.add(rbDualAxis);

    bgViewType.add(rbSingleView);
    bgViewType.add(rbMontage);
    panelViewType.setLayout(new BoxLayout(panelViewType, BoxLayout.Y_AXIS));
    borderViewType =
      new TitledBorder(
        BorderFactory.createEtchedBorder(
          new Color(248, 254, 255),
          new Color(121, 124, 136)),
        "View Type");
    panelViewType.setPreferredSize(dimDataTypePref);
    panelViewType.setBorder(borderViewType);
    panelViewType.add(rbSingleView);
    panelViewType.add(rbMontage);

    bgSectionType.add(rbSerialSection);
    bgSectionType.add(rbSingleSection);
    panelSectionType.setLayout(
      new BoxLayout(panelSectionType, BoxLayout.Y_AXIS));
    borderSectionType =
      new TitledBorder(
        BorderFactory.createEtchedBorder(
          new Color(248, 254, 255),
          new Color(121, 124, 136)),
        "Tomogram Type");
    panelSectionType.setPreferredSize(dimDataTypePref);
    panelSectionType.setBorder(borderSectionType);
    panelSectionType.add(rbSingleSection);
    panelSectionType.add(rbSerialSection);

    //
    //  Datatype panel
    //
    borderDataType =
      new TitledBorder(
        BorderFactory.createEtchedBorder(
          new Color(248, 254, 255),
          new Color(121, 124, 136)),
        "Data Type");

    panelDataType.setLayout(new BoxLayout(panelDataType, BoxLayout.X_AXIS));
    panelDataType.setBorder(borderDataType);
    panelDataType.add(panelDataSource);
    panelDataType.add(Box.createHorizontalGlue());
    panelDataType.add(panelAxisType);
    panelDataType.add(Box.createHorizontalGlue());
    panelDataType.add(panelViewType);
    panelDataType.add(Box.createHorizontalGlue());
    panelDataType.add(panelSectionType);

    //
    //  Pixel & Alignment panel
    //
    panelPixelAndLocalAlign.setLayout(
      new BoxLayout(panelPixelAndLocalAlign, BoxLayout.X_AXIS));
    panelPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x5_y0));
    panelPixelAndLocalAlign.add(Box.createHorizontalGlue());
    /*    panelPixelAndLocalAlign.add(labelPixelSize);
        panelPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x5_y0));
        panelPixelAndLocalAlign.add(textFieldPixelSize);*/
    panelPixelAndLocalAlign.add(ltfPixelSize.getContainer());
    panelPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    panelPixelAndLocalAlign.add(Box.createHorizontalGlue());
    panelPixelAndLocalAlign.add(ltfFiducialDiameter.getContainer());
    panelPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    panelPixelAndLocalAlign.add(Box.createHorizontalGlue());
    panelPixelAndLocalAlign.add(ltfImageRotation.getContainer());
    panelPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x10_y0));
    panelPixelAndLocalAlign.add(Box.createHorizontalGlue());
    panelPixelAndLocalAlign.add(Box.createRigidArea(FixedDim.x5_y0));

    //
    //  Create Data Parameters panel
    //
    panelDataParameters.setLayout(
      new BoxLayout(panelDataParameters, BoxLayout.Y_AXIS));
    panelDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    panelDataParameters.add(panelFileset);
    panelDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    panelDataParameters.add(panelDataType);
    panelDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    panelDataParameters.add(panelPixelAndLocalAlign);
    panelDataParameters.add(Box.createHorizontalGlue());
  }

  private void createPerAxisInfoPanel() {
    //
    //  Tilt angle specification panel
    //
    panelAxisInfoA.setBorder(borderAxisInfoA.getBorder());
    panelAxisInfoA.setLayout(new BoxLayout(panelAxisInfoA, BoxLayout.Y_AXIS));

    panelAxisInfoA.add(tiltAnglesA.getPanel());
    panelAxisInfoA.add(Box.createRigidArea(FixedDim.x0_y10));
    panelAxisInfoA.add(ltfExcludeListA.getContainer());

    panelAxisInfoB.setBorder(borderAxisInfoB.getBorder());
    panelAxisInfoB.setLayout(new BoxLayout(panelAxisInfoB, BoxLayout.Y_AXIS));

    panelAxisInfoB.add(tiltAnglesB.getPanel());
    panelAxisInfoB.add(ltfExcludeListB.getContainer());

    panelPerAxisInfo.setLayout(
      new BoxLayout(panelPerAxisInfo, BoxLayout.X_AXIS));
    panelPerAxisInfo.add(panelAxisInfoA);
    panelPerAxisInfo.add(panelAxisInfoB);
  }

  public void initializeFields(ConstMetaData metaData) {

    if (!metaData.getFilesetName().equals("")) {
      String canonicalPath =
        metaData.getWorkingDirectory() + "/" + metaData.getFilesetName();
      textFieldFileset.setText(canonicalPath);
    }

    textFieldBackupDirectory.setText(metaData.getBackupDirectory());
    setDataSource(metaData.getDataSource());
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

    metaData.setBackupDirectory(textFieldBackupDirectory.getText());
    metaData.setDataSource(getDataSource());
    metaData.setAxisType(getAxisType());

    //  The fileset name needs to be set after the axis type so the metadata
    // object modifies the ending correctly
    metaData.setFilesetName(textFieldFileset.getText());
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

  //
  // Data source radio button control
  //
  void setDataSource(DataSource dataSource) {
    if (dataSource == DataSource.CCD) {
      rbCCD.setSelected(true);
    }
    else {
      rbFilm.setSelected(true);
    }
  }

  DataSource getDataSource() {
    if (rbCCD.getSelectedObjects() != null) {
      return DataSource.CCD;
    }
    else {
      return DataSource.FILM;
    }
  }

  //
  //  Axis type radio button
  //
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

  //
  //  View type radio button
  //
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

  //
  //  Section type radio button
  //
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
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup =
      new ContextPopup(panelSetup, mouseEvent, "INITIAL STEPS");
  }

  //
  //  Action functions for buttons
  //

  void buttonFilesetAction(ActionEvent event) {
    //
    //  Open up the file chooser in the working directory
    //
    JFileChooser chooser =
      new JFileChooser(new File(applicationManager.getWorkingDirectory()));

    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(this);

    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File filesetName = chooser.getSelectedFile();
      try {
        textFieldFileset.setText(filesetName.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  void buttonBackupDirectoryAction(ActionEvent event) {
    //
    //  Open up the file chooser in the working directory
    //
    JFileChooser chooser =
      new JFileChooser(new File(textFieldBackupDirectory.getText()));
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(this);

    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File backupDirectory = chooser.getSelectedFile();
      try {
        textFieldBackupDirectory.setText(backupDirectory.getCanonicalPath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  void rbSingleAxisAction(ActionEvent event) {
    tiltAnglesB.setEnabled(false);
    ltfExcludeListB.setEnabled(false);
  }

  void rbDualAxisAction(ActionEvent event) {
    tiltAnglesB.setEnabled(true);
    ltfExcludeListB.setEnabled(true);
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
    String line1, line2, line3, line4, line5, line6, line7, line8;

    line1 = "<html>Enter the name of projection data file(s). You can<br>";
    line2 = "also select the projection data file  by pressing the<br>";
    line3 = "folder button. Remember to omit the .st for single axis<br>";
    line4 = "data sets or the a.st for dual axis data sets.";
    labelFileset.setToolTipText(line1 + line2 + line3 + line4);
    textFieldFileset.setToolTipText(labelFileset.getToolTipText());

    line1 = "<html>This button will open a file chooser dialog box<br>";
    line2 = "allowing you to select the projection data file.<br>";
    line3 = "Remember to remove the .st for single axis data sets<br>";
    line4 = "or the a.st for dual axis data sets.";
    buttonFileset.setToolTipText(line1 + line2 + line3);

    line1 = "<html>Enter the name of the directory where you want the<br>";
    line2 = "small data files .com and .log files to be backed up.  You<br>";
    line3 = "can use the folder button on the right to create a new<br>";
    line4 = "directory to store the backups.";
    labelBackupDirectory.setToolTipText(line1 + line2 + line3 + line4);
    textFieldBackupDirectory.setToolTipText(labelFileset.getToolTipText());

    line1 = "<html>This button will open a file chooser dialog box<br>";
    line2 = "allowing you to select and/or create the backup directory.";
    buttonBackupDirectory.setToolTipText(line1 + line2);

    line1 = "<html>This radio button selector will choose whether the data<br>";
    line2 = "has been collected using a CCD or film.";
    panelDataSource.setToolTipText(line1 + line2);

    line1 = "<html>This radio button selector will choose whether the data<br>";
    line2 = "consists of one or two tilt axis.";
    panelAxisType.setToolTipText(line1 + line2);

    line1 = "<html>This radio button selector will choose whether the data<br>";
    line2 = "consists of a single view per projection or multiple views<br>";
    line3 = "per projection (montaged).";
    panelViewType.setToolTipText(line1 + line2 + line3);

    line1 = "<html>This radio button selector will choose whether the data<br>";
    line2 = "consists of a single tomogram or several serial tomograms";
    panelSectionType.setToolTipText(line1 + line2);

    line1 = "<html>Enter the projection image pixel size in nanometers here.";
    ltfPixelSize.setToolTipText(line1);

    line1 = "<html>Enter the fiducial size in nanometers here.";
    ltfFiducialDiameter.setToolTipText(line1);

    line1 = "<html>Enter the projection image rotation in degrees. This is<br>";
    line2 = "the rotation (CCW positive) from the Y-axis (the tilt axis<br>";
    line3 = "after the views are aligned) to the suspected tilt axis in<br>";
    line4 = "the unaligned views.";
    ltfImageRotation.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>Specify the source of the projection tilt angles";
    line2 = "<ul><li>Select the Extract option if the raw stack data<br>";
    line3 = "contains the tilt angle data";
    line4 = "<li>Select the Specify option if you wish to manually<br>";
    line5 = "specify the tilt angles in the edit boxes below";
    line6 = "<li>Select the File option if the tilt angles already exist<br>";
    line7 = "in a *.rawtilt file.</ul>";
    tiltAnglesA.setToolTipText(
      line1 + line2 + line3 + line4 + line5 + line6 + line7);
    tiltAnglesB.setToolTipText(
      line1 + line2 + line3 + line4 + line5 + line6 + line7);

    line1 =
      "<html>Enter the projection images to <b>exclude</b> from the processing<br>";
    line2 = "of this axis.  Ranges are allowed, separate ranges by<br>";
    line3 = "commas.  For example to exclude the first four and last<br>";
    line4 = "four images of a 60 projection stack enter 1-4,57-60.";
    ltfExcludeListA.setToolTipText(line1 + line2 + line3 + line4);
    ltfExcludeListB.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>The button will setup the processing for existing<br>";
    line2 = "command scripts.  <b>Be sure that parameters entered match<br>";
    line3 = "the existing command scripts.</b>";
    buttonPostpone.setToolTipText(line1 + line2 + line3);

    line1 = "<html>This button will create a new set of command scripts<br>";
    line2 = "overwriting any of the same name in the specified working<br>";
    line3 = "directory.  Be sure to save the data file after creating the<br>";
    line4 = "command script if you wish keep the results.";
    buttonExecute.setToolTipText(line1 + line2 + line3 + line4);

  }

}

//
//  Button action listener classes
//
class SetupDialogFilesetActionAdapter implements ActionListener {

  SetupDialog adaptee;

  SetupDialogFilesetActionAdapter(SetupDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonFilesetAction(event);
  }
}

class SetupDialogBackupDirectoryActionAdapter implements ActionListener {

  SetupDialog adaptee;

  SetupDialogBackupDirectoryActionAdapter(SetupDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonBackupDirectoryAction(event);
  }
}

class SetupDialogSingleAxisActionAdapter implements ActionListener {

  SetupDialog adaptee;

  SetupDialogSingleAxisActionAdapter(SetupDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.rbSingleAxisAction(event);
  }
}

class SetupDialogDualAxisActionAdapter implements ActionListener {

  SetupDialog adaptee;

  SetupDialogDualAxisActionAdapter(SetupDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.rbDualAxisAction(event);
  }
}
