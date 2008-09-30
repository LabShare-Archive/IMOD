package etomo.ui;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.BaseManager;
import etomo.PeetManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.ModelFileFilter;
import etomo.storage.MotlFileFilter;
import etomo.storage.TiltFile;
import etomo.storage.TiltFileFilter;
import etomo.storage.TiltLog;
import etomo.storage.TiltLogFileFilter;
import etomo.storage.TomogramFileFilter;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.ConstPeetMetaData;
import etomo.type.EtomoAutodoc;
import etomo.type.PeetMetaData;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.30  2008/09/10 21:37:29  sueh
 * <p> bug# 1135 Handled a null pointer exception in RowList.delete.
 * <p>
 * <p> Revision 1.29  2008/05/03 00:58:47  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.28  2008/04/02 17:35:59  sueh
 * <p> bug# 1098 Improved user error messages.  Handled empty fnModParticle.
 * <p>
 * <p> Revision 1.27  2008/01/31 20:31:41  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.26  2007/07/25 23:01:16  sueh
 * <p> bug# 1027 Change start and end tilt range angles to min and max angles.  Added
 * <p> A filter for tilt log files to openTiltFile().
 * <p>
 * <p> Revision 1.25  2007/07/18 22:38:22  sueh
 * <p> bug# 1024
 * <p>
 * <p> Revision 1.24  2007/06/08 22:22:29  sueh
 * <p> bug# 1014 Added reset().
 * <p>
 * <p> Revision 1.23  2007/06/06 22:06:50  sueh
 * <p> bug# 1013 Made two rows of the buttons.
 * <p>
 * <p> Revision 1.22  2007/06/06 16:09:19  sueh
 * <p> bug# 1013 Changed addVolume() to addVolumeRow().  Added
 * <p> setFnModParticle() and validateRun().  In addVolumeRow() no longer
 * <p> requiring fnModParticle.
 * <p>
 * <p> Revision 1.21  2007/05/31 22:27:16  sueh
 * <p> bug# 1004 Changed PeetDialog.OUTPUT_LABEL to FN_OUTPUT_LABEL.
 * <p>
 * <p> Revision 1.20  2007/05/16 02:38:03  sueh
 * <p> bug# 964 Added btnDeleteRow and deleteRow(VolumeRow).
 * <p>
 * <p> Revision 1.19  2007/05/11 16:07:12  sueh
 * <p> bug# 964 Removed print statement.
 * <p>
 * <p> Revision 1.18  2007/05/08 19:20:11  sueh
 * <p> bug# 964 Adding File importDir to
 * <p> setParameters(MatlabParam,boolean,boolean).  Temporarily setting the
 * <p> user.dir property to importDir, so that files which don't have an absolute
 * <p> path will have the import directory as their parent.
 * <p>
 * <p> Revision 1.17  2007/05/07 17:24:08  sueh
 * <p> bug# 964 Changed MatlabParamFile to MatlabParam.
 * <p>
 * <p> Revision 1.16  2007/04/09 21:25:20  sueh
 * <p> bug# 964 Toggling the tilt range file button.
 * <p>
 * <p> Revision 1.15  2007/04/02 21:53:58  sueh
 * <p> bug# 964 Disabling initMotlFile column and button when PeetDialog.rbInitMotlFile
 * <p> is not selected.  Disabling tiltRange column when PeetDialog.cbTiltRange is not
 * <p> selected.
 * <p>
 * <p> Revision 1.14  2007/04/02 16:04:54  sueh
 * <p> bug# 964 Made pnlButtons local.
 * <p>
 * <p> Revision 1.13  2007/03/30 23:56:09  sueh
 * <p> bug# 964 Informing PeetDialog when the number of rows changes.
 * <p>
 * <p> Revision 1.12  2007/03/27 19:32:58  sueh
 * <p> bug# 964 Number the rows.
 * <p>
 * <p> Revision 1.11  2007/03/27 00:08:08  sueh
 * <p> bug# 964 Added btnReadTiltFile and r3bVolume (3dmod volume).
 * <p>
 * <p> Revision 1.10  2007/03/26 18:41:52  sueh
 * <p> bug# 964 Removed functionality that shows/hides columns.  Fixed bug in
 * <p> setting initMOTL and relativeOrient.
 * <p>
 * <p> Revision 1.9  2007/03/23 20:45:15  sueh
 * <p> bug# 964 GetParameters(MatlabParamFile):  setting the size of volumeList before
 * <p> running getParameters for each row.
 * <p>
 * <p> Revision 1.8  2007/03/21 19:49:40  sueh
 * <p> bug# 964 Removed some gets/sets and replaced them with get/setParameters.
 * <p>
 * <p> Revision 1.7  2007/03/20 23:13:36  sueh
 * <p> bug# 964 Getting/setting metadata.  Divided RelativeOrient into X, Y, and Z
 * <p> fields.
 * <p>
 * <p> Revision 1.6  2007/03/20 00:47:07  sueh
 * <p> bug# 964 Hiding/showing the Initial MOTL column.
 * <p>
 * <p> Revision 1.5  2007/03/15 21:55:07  sueh
 * <p> bug# 964 Loading data from .prm file.  Changing the field names to match the
 * <p> .prm file format.
 * <p>
 * <p> Revision 1.4  2007/03/01 01:46:48  sueh
 * <p> bug# 964 Added highlighting, model and motl to table.
 * <p>
 * <p> Revision 1.3  2007/02/22 20:40:00  sueh
 * <p> bug# 964 Added addRow().
 * <p>
 * <p> Revision 1.2  2007/02/21 04:25:18  sueh
 * <p> bug# 964 Changed PeetManager.setName() to PeetManager.setParamFile().
 * <p>
 * <p> Revision 1.1  2007/02/20 20:37:32  sueh
 * <p> bug# 964 A list of tomograms for the PEET interface.
 * <p> </p>
 */
final class VolumeTable implements Expandable, Highlightable,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  static final String FN_MOD_PARTICLE_HEADER1 = "Model";
  static final String TABLE_HEADER = "Volume Table";

  private final RowList rowList = new RowList();
  private final JPanel rootPanel = new JPanel();
  private final MultiLineButton btnAddFnVolume = new MultiLineButton(
      "Add Volume and Model");
  private final MultiLineButton btnChangeFnModParticle = new MultiLineButton(
      "Change Model");
  private final MultiLineButton btnSetInitMotlFile = new MultiLineButton(
      "Set Initial Motive List File");
  private final MultiLineButton btnReadTiltFile = new MultiLineButton(
      "Read Tilt File");
  private final Run3dmodButton r3bVolume;
  private final HeaderCell header1VolumeNumber = new HeaderCell();
  private final HeaderCell header1FnVolume = new HeaderCell("Volume");
  private final HeaderCell header1FnModParticle = new HeaderCell(
      FN_MOD_PARTICLE_HEADER1);
  private final HeaderCell header1InitMotlFile = new HeaderCell("Initial");
  private final HeaderCell header1TiltRange = new HeaderCell("Tilt Range");
  private final HeaderCell header1RelativeOrient = new HeaderCell(
      "Rel. Orient.");
  private final HeaderCell header2VolumeNumber = new HeaderCell();
  private final HeaderCell header2FnVolume = new HeaderCell();
  private final HeaderCell header2FnModParticle = new HeaderCell();
  private final HeaderCell header2InitMotlFile = new HeaderCell("MOTL");
  private final HeaderCell header2TiltRangeStart = new HeaderCell("Min",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header2TiltRangeEnd = new HeaderCell("Max",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header2RelativeOrientX = new HeaderCell("X",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header2RelativeOrientY = new HeaderCell("Y",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header2RelativeOrientZ = new HeaderCell("Z",
      UIParameters.INSTANCE.getNumericWidth());
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final Column initMotlFileColumn = new Column();
  private final Column tiltRangeColumn = new Column();
  private final MultiLineButton btnDeleteRow = new MultiLineButton("Delete Row");
  private final ExpandButton btnExpandFnVolume;
  private final ExpandButton btnExpandFnModParticle;
  private final ExpandButton btnExpandInitMotlFile;
  private final PeetManager manager;
  private final PeetDialog parent;

  private File lastLocation = null;
  private boolean useInitMotlFile = true;
  private boolean useTiltRange = true;

  private VolumeTable(final PeetManager manager, final PeetDialog parent) {
    this.manager = manager;
    this.parent = parent;
    //construction
    btnExpandFnVolume = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandFnModParticle = ExpandButton.getInstance(this,
        ExpandButton.Type.MORE);
    btnExpandInitMotlFile = ExpandButton.getInstance(this,
        ExpandButton.Type.MORE);
    r3bVolume = Run3dmodButton.get3dmodInstance("Open in 3dmod", this);
    createTable();
    updateDisplay();
    setToolTipText();
  }

  static VolumeTable getInstance(final PeetManager manager,
      final PeetDialog parent) {
    VolumeTable instance = new VolumeTable(manager, parent);
    instance.addListeners();
    return instance;
  }

  public void expand(final ExpandButton button) {
    if (button == btnExpandFnVolume) {
      rowList.expandFnVolume(btnExpandFnVolume.isExpanded());
    }
    else if (button == btnExpandFnModParticle) {
      rowList.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    }
    else if (button == btnExpandInitMotlFile) {
      rowList.expandInitMotl(btnExpandInitMotlFile.isExpanded());
    }
    UIHarness.INSTANCE.pack(manager);
  }

  public void highlight(final boolean highlight) {
    updateDisplay();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), run3dmodMenuOptions);
  }

  Container getContainer() {
    return rootPanel;
  }

  void reset() {
    rowList.remove();
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  void getParameters(final PeetMetaData metaData) {
    rowList.getParameters(metaData);
  }

  void setParameters(final ConstPeetMetaData metaData) {
    rowList.setParameters(metaData);
  }

  void setParameters(final MatlabParam matlabParamFile,
      boolean useInitMotlFile, boolean useTiltRange, File importDir) {
    boolean initMotlFileIsExpanded = btnExpandInitMotlFile.isExpanded();
    String userDir = null;
    if (importDir != null) {
      userDir = System.setProperty("user.dir", importDir.getAbsolutePath());
    }
    for (int i = 0; i < matlabParamFile.getVolumeListSize(); i++) {
      File fnModParticleFile = null;
      String fnModParticle = matlabParamFile.getFnModParticle(i);
      if (!fnModParticle.matches("\\s*")) {
        fnModParticleFile = new File(fnModParticle);
      }
      VolumeRow row = addRow(new File(matlabParamFile.getFnVolume(i)),
          fnModParticleFile);
      row.setParameters(matlabParamFile, useInitMotlFile, useTiltRange);
      row.expandInitMotlFile(initMotlFileIsExpanded);
    }
    if (importDir != null) {
      System.setProperty("user.dir", userDir);
    }
    rowList.doneSettingParameters();
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  void getParameters(final MatlabParam matlabParamFile) {
    rowList.getParameters(matlabParamFile);
  }

  int size() {
    return rowList.size();
  }

  void updateDisplay(boolean useInitMotlFile, boolean useTiltRange) {
    this.useInitMotlFile = useInitMotlFile;
    this.useTiltRange = useTiltRange;
    initMotlFileColumn.setEnabled(useInitMotlFile);
    tiltRangeColumn.setEnabled(useTiltRange);
    updateDisplay();
  }

  private void createTable() {
    //columns
    initMotlFileColumn.add(header1InitMotlFile);
    initMotlFileColumn.add(header2InitMotlFile);
    tiltRangeColumn.add(header1TiltRange);
    tiltRangeColumn.add(header2TiltRangeStart);
    tiltRangeColumn.add(header2TiltRangeEnd);
    //table
    pnlTable.setLayout(layout);
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    constraints.fill = GridBagConstraints.BOTH;
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.gridheight = 1;
    display();
    //border
    SpacedPanel pnlBorder = SpacedPanel.getInstance();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(new EtchedBorder(TABLE_HEADER).getBorder());
    pnlBorder.add(pnlTable);
    //buttons 1
    JPanel pnlButtons1 = new JPanel();
    pnlButtons1.setLayout(new BoxLayout(pnlButtons1, BoxLayout.X_AXIS));
    btnAddFnVolume.setSize();
    pnlButtons1.add(btnAddFnVolume.getComponent());
    btnChangeFnModParticle.setSize();
    pnlButtons1.add(btnChangeFnModParticle.getComponent());
    btnSetInitMotlFile.setSize();
    pnlButtons1.add(btnSetInitMotlFile.getComponent());
    btnReadTiltFile.setSize();
    pnlButtons1.add(btnReadTiltFile.getComponent());
    btnDeleteRow.setSize();
    pnlButtons1.add(btnDeleteRow.getComponent());
    //buttons 2
    JPanel pnlButtons2 = new JPanel();
    pnlButtons2.setLayout(new BoxLayout(pnlButtons2, BoxLayout.X_AXIS));
    r3bVolume.setSize();
    pnlButtons2.add(r3bVolume.getComponent());
    //root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    rootPanel.add(pnlBorder.getContainer());
    rootPanel.add(pnlButtons1);
    rootPanel.add(pnlButtons2);
  }

  private void imodVolume(Run3dmodMenuOptions menuOptions) {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      throw new IllegalStateException(
          "r3bVolume enabled when no row is highlighted");
    }
    row.imodVolume(menuOptions);
  }

  private void display() {
    constraints.weighty = 0.0;
    //First header row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header1VolumeNumber.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    constraints.weightx = 0.1;
    header1FnVolume.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandFnVolume.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header1FnModParticle.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandFnModParticle.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header1InitMotlFile.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandInitMotlFile.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 2;
    header1TiltRange.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1RelativeOrient.add(pnlTable, layout, constraints);
    //Second header row
    constraints.weightx = 0.0;
    constraints.gridwidth = 2;
    header2VolumeNumber.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header2FnVolume.add(pnlTable, layout, constraints);
    header2FnModParticle.add(pnlTable, layout, constraints);
    header2InitMotlFile.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header2TiltRangeStart.add(pnlTable, layout, constraints);
    header2TiltRangeEnd.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header2RelativeOrientX.add(pnlTable, layout, constraints);
    header2RelativeOrientY.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2RelativeOrientZ.add(pnlTable, layout, constraints);
    rowList.display();
  }

  private void action(final String command,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnAddFnVolume.getActionCommand())) {
      addVolumeRow();
    }
    else if (command.equals(btnSetInitMotlFile.getActionCommand())) {
      setInitMotlFile();
    }
    else if (command.equals(btnReadTiltFile.getActionCommand())) {
      openTiltFile();
    }
    else if (command.equals(btnDeleteRow.getActionCommand())) {
      deleteRow(rowList.getHighlightedRow());
    }
    else if (command.equals(btnChangeFnModParticle.getActionCommand())) {
      setFnModParticle();
    }
    else if (command.equals(r3bVolume.getActionCommand())) {
      imodVolume(run3dmodMenuOptions);
    }
  }

  boolean validateRun() {
    return rowList.validateRun();
  }

  private void deleteRow(VolumeRow row) {
    rowList.remove();
    rowList.delete(row, this, pnlTable, layout, constraints);
    rowList.display();
    UIHarness.INSTANCE.pack(manager);
  }

  private void setToolTipText() {
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory
          .getInstance(AutodocFactory.PEET_PRM);
      String tooltip1 = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.FN_VOLUME_KEY);
      header1FnVolume.setToolTipText(tooltip1);
      header2FnVolume.setToolTipText(tooltip1);
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.FN_MOD_PARTICLE_KEY);
      btnAddFnVolume.setToolTipText(tooltip1 + "  " + tooltip);
      header1FnModParticle.setToolTipText(tooltip);
      header2FnModParticle.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.INIT_MOTL_KEY);
      btnSetInitMotlFile.setToolTipText(tooltip);
      header1InitMotlFile.setToolTipText(tooltip);
      header2InitMotlFile.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.TILT_RANGE_KEY);
      btnReadTiltFile.setToolTipText(tooltip);
      header1TiltRange.setToolTipText(tooltip);
      header2TiltRangeStart.setToolTipText(tooltip);
      header2TiltRangeEnd.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.RELATIVE_ORIENT_KEY);
      header1RelativeOrient.setToolTipText(tooltip);
      header2RelativeOrientX.setToolTipText(tooltip);
      header2RelativeOrientY.setToolTipText(tooltip);
      header2RelativeOrientZ.setToolTipText(tooltip);
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
  }

  private JFileChooser getFileChooserInstance() {
    return new JFileChooser(lastLocation == null ? new File(manager
        .getPropertyUserDir()) : lastLocation);
  }

  /**
   * Allow the user to choose a tomogram and a model and add them to the table
   * in a new row.  Only works if they choose both.
   */
  private void addVolumeRow() {
    if (!manager.setParamFile()) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + " and " + PeetDialog.FN_OUTPUT_LABEL
          + " fields before adding tomograms.", "Entry Error");
      return;
    }
    File fnVolume = null;
    JFileChooser chooser = getFileChooserInstance();
    chooser.setFileFilter(new TomogramFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    fnVolume = chooser.getSelectedFile();
    lastLocation = fnVolume.getParentFile();
    chooser = getFileChooserInstance();
    chooser.setFileFilter(new ModelFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    returnVal = chooser.showOpenDialog(rootPanel);
    File fnModParticle = null;
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      fnModParticle = chooser.getSelectedFile();
      lastLocation = fnModParticle.getParentFile();
    }
    if (fnVolume == null) {
      UIHarness.INSTANCE.openMessageDialog("Please choose a tomogram",
          "Entry Error");
    }
    else {
      addRow(fnVolume, fnModParticle);
      updateDisplay();
      parent.msgVolumeTableSizeChanged();
      UIHarness.INSTANCE.pack(manager);
    }
  }

  private void setInitMotlFile() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error");
      return;
    }
    JFileChooser chooser = getFileChooserInstance();
    chooser.setFileFilter(new MotlFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File initMotlFile = chooser.getSelectedFile();
      row.setInitMotlFile(initMotlFile);
      lastLocation = initMotlFile.getParentFile();
      row.expandInitMotlFile(btnExpandInitMotlFile.isExpanded());
      UIHarness.INSTANCE.pack(manager);
    }
  }

  private void setFnModParticle() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error");
      return;
    }
    JFileChooser chooser = getFileChooserInstance();
    chooser.setFileFilter(new ModelFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File fnModParticle = chooser.getSelectedFile();
      row.setFnModParticle(fnModParticle);
      lastLocation = fnModParticle.getParentFile();
      row.expandFnModParticle(btnExpandInitMotlFile.isExpanded());
      UIHarness.INSTANCE.pack(manager);
    }
  }

  private void openTiltFile() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error");
      return;
    }
    JFileChooser chooser = getFileChooserInstance();
    chooser.addChoosableFileFilter(new TiltFileFilter());
    //Add the default file filter (tilt log)
    TiltLogFileFilter tiltLogFileFilter = new TiltLogFileFilter();
    chooser.addChoosableFileFilter(tiltLogFileFilter);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File file = chooser.getSelectedFile();
      lastLocation = file.getParentFile();
      if (tiltLogFileFilter.accept(file)) {
        try {
          TiltLog tiltLog = TiltLog.getInstance(file);
          tiltLog.read();
          row.setTiltRangeMin(tiltLog.getMinAngle());
          row.setTiltRangeMax(tiltLog.getMaxAngle());
        }
        catch (LogFile.FileException e) {
          e.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog("Unable to open tilt log "
              + file.getAbsolutePath() + "\n" + e.getMessage(),
              "File Open Failure");
        }
      }
      else {
        TiltFile tiltFile = new TiltFile(file);
        row.setTiltRangeMin(tiltFile.getMinAngle().toString());
        row.setTiltRangeMax(tiltFile.getMaxAngle().toString());
      }
    }
  }

  private VolumeRow addRow(final File fnVolume, final File fnModParticle) {
    VolumeRow row = rowList.add(manager, fnVolume, fnModParticle, this,
        pnlTable, layout, constraints, initMotlFileColumn, tiltRangeColumn);
    row.display();
    row.expandFnVolume(btnExpandFnVolume.isExpanded());
    row.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    return row;
  }

  private void updateDisplay() {
    boolean enable = rowList.size() > 0;
    btnExpandFnVolume.setEnabled(enable);
    btnExpandFnModParticle.setEnabled(enable);
    btnExpandInitMotlFile.setEnabled(enable);
    btnSetInitMotlFile.setEnabled(enable && rowList.isHighlighted()
        && useInitMotlFile);
    btnChangeFnModParticle.setEnabled(enable && rowList.isHighlighted());
    btnReadTiltFile.setEnabled(enable && rowList.isHighlighted()
        && useTiltRange);
    r3bVolume.setEnabled(enable && rowList.isHighlighted());
    btnDeleteRow.setEnabled(enable && rowList.isHighlighted());
  }

  private void addListeners() {
    VTActionListener actionListener = new VTActionListener(this);
    btnAddFnVolume.addActionListener(actionListener);
    btnSetInitMotlFile.addActionListener(actionListener);
    btnReadTiltFile.addActionListener(actionListener);
    r3bVolume.addActionListener(actionListener);
    btnDeleteRow.addActionListener(actionListener);
    btnChangeFnModParticle.addActionListener(actionListener);
  }

  private static final class RowList {
    private final List list = new ArrayList();
    private ConstPeetMetaData metaData = null;

    private int size() {
      return list.size();
    }

    private void remove() {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).remove();
      }
    }

    private synchronized void delete(VolumeRow row, final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      if (row == null) {
        int index = row.getIndex();
        list.remove(index);
        for (int i = index; i < list.size(); i++) {
          ((VolumeRow) list.get(i)).setIndex(i);
        }
      }
    }

    private synchronized VolumeRow add(final BaseManager manager,
        final File fnVolume, final File fnModParticle, final VolumeTable table,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints, Column initMotlFileColumn,
        Column tiltRangeColumn) {
      VolumeRow row = VolumeRow.getInstance(manager, fnVolume, fnModParticle,
          list.size(), table, panel, layout, constraints);
      list.add(row);
      row.registerInitMotlFileColumn(initMotlFileColumn);
      row.registerTiltRangeColumn(tiltRangeColumn);
      //When this function is used to load from the .epe and .prm files,
      //metadata must be set before MatlabParamFile data.  Wait until row is
      //added, then set from metadata.
      row.setParameters(metaData);
      return row;
    }

    private boolean validateRun() {
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = (VolumeRow) list.get(i);
        if (!row.validateRun()) {
          return false;
        }
      }
      return true;
    }

    private void getParameters(final PeetMetaData metaData) {
      metaData.resetInitMotlFile();
      metaData.resetTiltRangeMin();
      metaData.resetTiltRangeMax();
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = (VolumeRow) list.get(i);
        row.getParameters(metaData);
      }
    }

    private void getParameters(final MatlabParam matlabParamFile) {
      matlabParamFile.setVolumeListSize(list.size());
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).getParameters(matlabParamFile);
      }
    }

    /**
     * Save metaData until the rows are created
     * @param metaData
     */
    private void setParameters(final ConstPeetMetaData metaData) {
      this.metaData = metaData;
    }

    private void doneSettingParameters() {
      metaData = null;
    }

    private void display() {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).display();
      }
    }

    private void expandFnVolume(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandFnVolume(expanded);
      }
    }

    private void expandFnModParticle(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandFnModParticle(expanded);
      }
    }

    private void expandInitMotl(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandInitMotlFile(expanded);
      }
    }

    private boolean isHighlighted() {
      for (int i = 0; i < list.size(); i++) {
        if (((VolumeRow) list.get(i)).isHighlighted()) {
          return true;
        }
      }
      return false;
    }

    private VolumeRow getHighlightedRow() {
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = (VolumeRow) list.get(i);
        if (row.isHighlighted()) {
          return row;
        }
      }
      return null;
    }
  }

  private static final class VTActionListener implements ActionListener {
    private final VolumeTable volumeTable;

    private VTActionListener(final VolumeTable volumeTable) {
      this.volumeTable = volumeTable;
    }

    public void actionPerformed(final ActionEvent event) {
      volumeTable.action(event.getActionCommand(), null);
    }
  }
}
