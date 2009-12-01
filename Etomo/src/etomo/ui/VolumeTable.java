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
import etomo.EtomoDirector;
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
import etomo.type.AxisID;
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
 * <p> Revision 1.44  2009/11/20 23:07:37  sueh
 * <p> bug# 1280 Changed btnAddWithCopy to btnCopyRow.  Removed boolean
 * <p> from addVolumeRow.  Removed copyData.  Added copyRow.  Added
 * <p> RowList.addRow(VolumeRow).
 * <p>
 * <p> Revision 1.43  2009/10/29 20:02:36  sueh
 * <p> bug# Added btnMoveDown, btnMoveUp, copyData,
 * <p> getFnModMParticleHeaderCell, getFnVolumeHeaderCell, moveRowDown,
 * <p> and moveRowUp.
 * <p>
 * <p> Revision 1.42  2009/10/16 23:57:31  sueh
 * <p> bug# 1234 Added tiltRangeRequired parameter to validateRun.
 * <p>
 * <p> Revision 1.41  2009/10/15 23:41:04  sueh
 * <p> bug# 1274 In validateRun returning the error string instead of boolean
 * <p> because the tab must be changed before the error message can be
 * <p> popped up.
 * <p>
 * <p> Revision 1.40  2009/09/28 18:35:55  sueh
 * <p> bug# 1235 Added VolumeRow.setNames.
 * <p>
 * <p> Revision 1.39  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.38  2009/04/27 18:07:10  sueh
 * <p> bug# 1211 Moved last location functionality from VolumeTable to
 * <p> PeetDialog.  Added checkIncorrectPaths, fixIncorrectPaths, and
 * <p> fixIncorrectPath.
 * <p>
 * <p> Revision 1.37  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.36  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.35  2009/01/20 20:33:19  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 1.34  2008/10/07 16:44:55  sueh
 * <p> bug# 1113 Improved names:  changed Viewport.msgViewportMoved to
 * <p> msgViewportPaged.
 * <p>
 * <p> Revision 1.33  2008/10/06 22:49:36  sueh
 * <p> bug# 1113 Got the table size from UserConfiguration.
 * <p>
 * <p> Revision 1.32  2008/10/01 22:56:53  sueh
 * <p> bug# 1113 Implemented Viewable.  Moved display call outside of addRow.  Removing and adding everything after add.
 * <p>
 * <p> Revision 1.31  2008/09/30 22:55:29  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
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
    Run3dmodButtonContainer, Viewable {
  public static final String rcsid = "$Id$";

  static final String FN_MOD_PARTICLE_HEADER1 = "Model";
  static final String LABEL = "Volume Table";
  static final String TILT_RANGE_HEADER1_LABEL = "Tilt Range";

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
  private final HeaderCell header1VolumeNumber = new HeaderCell("Vol #");
  private final HeaderCell header1FnVolume = new HeaderCell("Volume");
  private final HeaderCell header1FnModParticle = new HeaderCell(
      FN_MOD_PARTICLE_HEADER1);
  private final HeaderCell header1InitMotlFile = new HeaderCell("Initial");
  private final HeaderCell header1TiltRange = new HeaderCell(
      TILT_RANGE_HEADER1_LABEL);
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
  private final TomogramFileFilter tomogramFileFilter = new TomogramFileFilter();
  private final MultiLineButton btnMoveUp = new MultiLineButton("Move Up");
  private final MultiLineButton btnMoveDown = new MultiLineButton("Move Down");
  private final MultiLineButton btnCopyRow = new MultiLineButton("Copy Row");

  private Viewport viewport;
  private final ExpandButton btnExpandFnVolume;
  private final ExpandButton btnExpandFnModParticle;
  private final ExpandButton btnExpandInitMotlFile;
  private final PeetManager manager;
  private final PeetDialog parent;

  private boolean useInitMotlFile = true;
  private boolean useTiltRange = true;

  private VolumeTable(final PeetManager manager, final PeetDialog parent) {
    this.manager = manager;
    this.parent = parent;
    //construction
    viewport = new Viewport(this, EtomoDirector.INSTANCE.getUserConfiguration()
        .getPeetTableSize().getInt(), parent.getSetupJComponent(), null, null,
        "Volume");
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

  boolean isFnVolumeExpanded() {
    return btnExpandFnVolume.isExpanded();
  }

  boolean isFnModParticleExpanded() {
    return btnExpandFnModParticle.isExpanded();
  }

  boolean isInitMotlFileExpanded() {
    return btnExpandInitMotlFile.isExpanded();
  }

  public void expand(final GlobalExpandButton button) {
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

  boolean isIncorrectPaths() {
    return rowList.isIncorrectPaths();
  }

  boolean fixIncorrectPaths(boolean choosePathEveryRow) {
    return rowList.fixIncorrectPaths(choosePathEveryRow);
  }

  boolean isCorrectPathNull() {
    return parent.isCorrectPathNull();
  }

  JFileChooser getFileChooserInstance() {
    return parent.getFileChooserInstance();
  }

  void setCorrectPath(String correctPath) {
    parent.setCorrectPath(correctPath);
  }

  String getCorrectPath() {
    return parent.getCorrectPath();
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
    viewport.adjustViewport(0);
    rowList.remove();
    rowList.display(viewport);
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  void getParameters(final MatlabParam matlabParamFile) {
    rowList.getParameters(matlabParamFile);
  }

  public int size() {
    return rowList.size();
  }
  
  boolean isEmpty() {
    return rowList.isEmpty();
  }

  void updateDisplay(boolean useInitMotlFile, boolean useTiltRange) {
    this.useInitMotlFile = useInitMotlFile;
    this.useTiltRange = useTiltRange;
    initMotlFileColumn.setEnabled(useInitMotlFile);
    tiltRangeColumn.setEnabled(useTiltRange);
    updateDisplay();
  }

  private void createTable() {
    //initialize
    btnCopyRow.setSize();
    btnMoveUp.setSize();
    btnMoveDown.setSize();
    btnDeleteRow.setSize();
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
    EtomoPanel pnlBorder = new EtomoPanel();
    pnlBorder.setLayout(new BoxLayout(pnlBorder, BoxLayout.X_AXIS));
    pnlBorder.setBorder(new EtchedBorder(LABEL).getBorder());
    pnlBorder.add(pnlTable);
    pnlBorder.add(viewport.getPagingPanel());
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
    pnlButtons1.add(r3bVolume.getComponent());
    //buttons 2
    JPanel pnlButtons2 = new JPanel();
    pnlButtons2.setLayout(new BoxLayout(pnlButtons2, BoxLayout.X_AXIS));
    r3bVolume.setSize();
    pnlButtons2.add(btnCopyRow.getComponent());
    pnlButtons2.add(btnMoveUp.getComponent());
    pnlButtons2.add(btnMoveDown.getComponent());
    pnlButtons2.add(btnDeleteRow.getComponent());

    //root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    rootPanel.add(pnlBorder);
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
  }

  HeaderCell getVolumeNumberHeaderCell() {
    return header1VolumeNumber;
  }

  HeaderCell getFnVolumeHeaderCell() {
    return header1FnVolume;
  }

  HeaderCell getFnModParticleHeaderCell() {
    return header1FnModParticle;
  }

  HeaderCell getRelativeOrientHeaderCell() {
    return header1RelativeOrient;
  }

  HeaderCell getTiltRangeHeaderCell() {
    return header1TiltRange;
  }

  public void msgViewportPaged() {
    rowList.remove();
    rowList.display(viewport);
    UIHarness.INSTANCE.pack(manager);
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
    else if (command.equals(btnMoveUp.getActionCommand())) {
      moveRowUp();
    }
    else if (command.equals(btnMoveDown.getActionCommand())) {
      moveRowDown();
    }
    else if (command.equals(btnCopyRow.getActionCommand())) {
      copyRow();
    }
  }

  /**
   * Validate for running.  Returns error message.
   * @return null if valid
   */
  String validateRun(boolean tiltRangeRequired) {
    return rowList.validateRun(tiltRangeRequired);
  }

  private void deleteRow(VolumeRow row) {
    rowList.remove();
    int index = rowList.delete(row, this, pnlTable, layout, constraints);
    viewport.adjustViewport(index);
    rowList.display(viewport);
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  private void setToolTipText() {
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
          AutodocFactory.PEET_PRM, manager.getManagerKey());
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
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
  }

  /**
   * Made a new that contains data copied from the highlighted row.
   */
  private void copyRow() {
    addRow(rowList.getHighlightedRow());
    viewport.adjustViewport(rowList.size() - 1);
    rowList.remove();
    rowList.display(viewport);
    updateDisplay();
    parent.msgVolumeTableSizeChanged();
    UIHarness.INSTANCE.pack(manager);
  }

  /**
   * Allow the user to choose a tomogram and a model and add them to the table
   * in a new row.  The tomogram is required.  The model is optional.
   */
  private void addVolumeRow() {
    if (!manager.setParamFile()) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + " and " + PeetDialog.FN_OUTPUT_LABEL
          + " fields before adding tomograms.", "Entry Error", manager
          .getManagerKey());
      return;
    }
    File fnVolume = null;
    JFileChooser chooser = parent.getFileChooserInstance();
    chooser.setFileFilter(tomogramFileFilter);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      //The tomogram is required.  Exist if one wasn't chosen.
      return;
    }
    fnVolume = chooser.getSelectedFile();
    parent.setLastLocation(fnVolume.getParentFile());
    chooser = parent.getFileChooserInstance();
    chooser.setFileFilter(new ModelFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    returnVal = chooser.showOpenDialog(rootPanel);
    File fnModParticle = null;
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      fnModParticle = chooser.getSelectedFile();
      parent.setLastLocation(fnModParticle.getParentFile());
    }
    if (fnVolume == null) {
      UIHarness.INSTANCE.openMessageDialog("Please choose a tomogram",
          "Entry Error", manager.getManagerKey());
    }
    else {
      //Add any unusual file extensions to the file filter so that the files are
      //easier to open in the next row.
      tomogramFileFilter.addExtension(fnVolume);
      addRow(fnVolume, fnModParticle);
      viewport.adjustViewport(rowList.size() - 1);
      rowList.remove();
      rowList.display(viewport);
      updateDisplay();
      parent.msgVolumeTableSizeChanged();
      UIHarness.INSTANCE.pack(manager);
    }
  }

  private void setInitMotlFile() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error", manager.getManagerKey());
      return;
    }
    JFileChooser chooser = parent.getFileChooserInstance();
    chooser.setFileFilter(new MotlFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File initMotlFile = chooser.getSelectedFile();
      row.setInitMotlFile(initMotlFile);
      parent.setLastLocation(initMotlFile.getParentFile());
      row.expandInitMotlFile(btnExpandInitMotlFile.isExpanded());
      UIHarness.INSTANCE.pack(manager);
    }
  }

  private void setFnModParticle() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error", manager.getManagerKey());
      return;
    }
    JFileChooser chooser = parent.getFileChooserInstance();
    chooser.setFileFilter(new ModelFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File fnModParticle = chooser.getSelectedFile();
      row.setFnModParticle(fnModParticle);
      parent.setLastLocation(fnModParticle.getParentFile());
      row.expandFnModParticle(btnExpandInitMotlFile.isExpanded());
      UIHarness.INSTANCE.pack(manager);
    }
  }

  private void openTiltFile() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error", manager.getManagerKey());
      return;
    }
    JFileChooser chooser = parent.getFileChooserInstance();
    chooser.addChoosableFileFilter(new TiltFileFilter());
    //Add the default file filter (tilt log)
    TiltLogFileFilter tiltLogFileFilter = new TiltLogFileFilter();
    chooser.addChoosableFileFilter(tiltLogFileFilter);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File file = chooser.getSelectedFile();
      parent.setLastLocation(file.getParentFile());
      if (tiltLogFileFilter.accept(file)) {
        try {
          TiltLog tiltLog = TiltLog.getInstance(file, manager.getManagerKey());
          tiltLog.read();
          row.setTiltRangeMin(tiltLog.getMinAngle());
          row.setTiltRangeMax(tiltLog.getMaxAngle());
        }
        catch (LogFile.LockException e) {
          e.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog("Unable to open tilt log "
              + file.getAbsolutePath() + "\n" + e.getMessage(),
              "File Open Failure", manager.getManagerKey());
        }
      }
      else {
        TiltFile tiltFile = new TiltFile(file, manager.getManagerKey());
        row.setTiltRangeMin(tiltFile.getMinAngle().toString());
        row.setTiltRangeMax(tiltFile.getMaxAngle().toString());
      }
    }
  }

  private VolumeRow addRow(final File fnVolume, final File fnModParticle) {
    VolumeRow row = rowList.add(manager, fnVolume, fnModParticle, this,
        pnlTable, layout, constraints, initMotlFileColumn, tiltRangeColumn);
    row.expandFnVolume(btnExpandFnVolume.isExpanded());
    row.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    return row;
  }

  /**
   * Copy volume, model, initial motl, and tilt range.  Don't copy the rel.
   * orientation.
   */
  private void addRow(VolumeRow fromRow) {
    VolumeRow row = rowList.add(manager, fromRow.getFnVolumeFile(), fromRow
        .getFnModParticleFile(), this, pnlTable, layout, constraints,
        initMotlFileColumn, tiltRangeColumn);
    row.expandFnVolume(btnExpandFnVolume.isExpanded());
    row.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    row.setInitMotlFile(fromRow.getInitMotlFile());
    row.expandInitMotlFile(btnExpandInitMotlFile.isExpanded());
    row.setTiltRangeMin(fromRow.getTiltRangeMin());
    row.setTiltRangeMax(fromRow.getTiltRangeMax());

  }

  /**
   * Swap the highlighted row with the one above it.  Move it in the rows 
   * ArrayList.  Move it in the table by removing and adding the two involved
   * rows and everything below them.  Renumber the row numbers in the table.
   */
  private void moveRowUp() {
    int index = rowList.getHighlightedRowIndex();
    if (index == -1) {
      return;
    }
    if (index == 0) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can't move the row up.  Its at the top.", "Wrong Row", AxisID.ONLY,
          manager.getManagerKey());
      return;
    }
    // rowList.removeRows(index - 1);
    rowList.moveRowUp(index);
    viewport.adjustViewport(index - 1);
    rowList.remove();
    rowList.display(viewport);
    rowList.reindex(index - 1);
    updateDisplay();
    manager.getMainPanel().repaint();
  }

  /**
   * Swap the highlighted row with the one below it.  Move it in the rows 
   * ArrayList.  Move it in the table by removing and adding the two involved
   * rows and everything below them.  Renumber the row numbers in the table.
   */
  private void moveRowDown() {
    int index = rowList.getHighlightedRowIndex();
    if (index == -1) {
      return;
    }
    if (index == rowList.size() - 1) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can't move the row down.  Its at the bottom.", "Wrong Row",
          AxisID.ONLY, manager.getManagerKey());
      return;
    }
    rowList.moveRowDown(index);
    viewport.adjustViewport(index + 1);
    rowList.remove();
    rowList.display(viewport);
    rowList.reindex(index);
    updateDisplay();
    manager.getMainPanel().repaint();
  }

  private void updateDisplay() {
    boolean enable = rowList.size() > 0;
    boolean highlighted = rowList.isHighlighted();
    btnExpandFnVolume.setEnabled(enable);
    btnExpandFnModParticle.setEnabled(enable);
    btnExpandInitMotlFile.setEnabled(enable);
    btnSetInitMotlFile.setEnabled(enable && highlighted && useInitMotlFile);
    btnChangeFnModParticle.setEnabled(enable && highlighted);
    btnReadTiltFile.setEnabled(enable && highlighted && useTiltRange);
    r3bVolume.setEnabled(enable && highlighted);
    btnDeleteRow.setEnabled(enable && highlighted);
    btnMoveUp.setEnabled(enable && highlighted
        && rowList.getHighlightedRowIndex() > 0);
    btnMoveDown.setEnabled(enable && highlighted
        && rowList.getHighlightedRowIndex() < rowList.size() - 1);
    btnCopyRow.setEnabled(enable && highlighted);
  }

  private void addListeners() {
    VTActionListener actionListener = new VTActionListener(this);
    btnAddFnVolume.addActionListener(actionListener);
    btnSetInitMotlFile.addActionListener(actionListener);
    btnReadTiltFile.addActionListener(actionListener);
    r3bVolume.addActionListener(actionListener);
    btnDeleteRow.addActionListener(actionListener);
    btnChangeFnModParticle.addActionListener(actionListener);
    btnMoveUp.addActionListener(actionListener);
    btnMoveDown.addActionListener(actionListener);
    btnCopyRow.addActionListener(actionListener);
  }

  private static final class RowList {
    private final List list = new ArrayList();
    private ConstPeetMetaData metaData = null;

    private int size() {
      return list.size();
    }
    
    private boolean isEmpty() {
      return list.isEmpty();
    }

    private void remove() {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).remove();
      }
    }

    private synchronized int delete(VolumeRow row, final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      int index = -1;
      if (row != null) {
        index = row.getIndex();
        list.remove(index);
        for (int i = index; i < list.size(); i++) {
          ((VolumeRow) list.get(i)).setIndex(i);
        }
      }
      return index;
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
      row.setNames();
      //When this function is used to load from the .epe and .prm files,
      //metadata must be set before MatlabParamFile data.  Wait until row is
      //added, then set from metadata.
      row.setParameters(metaData);
      return row;
    }

    private boolean isIncorrectPaths() {
      for (int i = 0; i < list.size(); i++) {
        if (((VolumeRow) list.get(i)).isIncorrectPaths()) {
          return true;
        }
      }
      return false;
    }

    /**
     * Fixs incorrect paths
     * @param choosePathEveryRow
     * @return false if the user canceled a file chooser.
     */
    private boolean fixIncorrectPaths(boolean choosePathEveryRow) {
      for (int i = 0; i < list.size(); i++) {
        if (!((VolumeRow) list.get(i)).fixIncorrectPaths(choosePathEveryRow)) {
          return false;
        }
      }
      return true;
    }

    /**
     * Swap two rows.
     */
    private void moveRowUp(final int rowIndex) {
      Object rowMoveUp = list.remove(rowIndex);
      Object rowMoveDown = list.remove(rowIndex - 1);
      list.add(rowIndex - 1, rowMoveUp);
      list.add(rowIndex, rowMoveDown);
    }

    private void moveRowDown(final int rowIndex) {
      Object rowMoveUp = list.remove(rowIndex + 1);
      Object rowMoveDown = list.remove(rowIndex);
      list.add(rowIndex, rowMoveUp);
      list.add(rowIndex + 1, rowMoveDown);
    }

    /**
     * Renumber the table starting from the row in the ArrayList at startIndex.
     * @param startIndex
     */
    private void reindex(final int startIndex) {
      for (int i = startIndex; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).setIndex(i);
      }
    }

    /**
     * Validate for running.  Returns error message.
     * @return null if valid
     */
    private String validateRun(boolean tiltRangeRequired) {
      if (list.size() < 1) {
        return "Must enter at least one row in " + LABEL;
      }
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = (VolumeRow) list.get(i);
        String errorMessage = row.validateRun(tiltRangeRequired);
        if (errorMessage != null) {
          return errorMessage;
        }
      }
      return null;
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

    private void display(Viewport viewport) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).display(i, viewport);
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

    private int getHighlightedRowIndex() {
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = (VolumeRow) list.get(i);
        if (row.isHighlighted()) {
          return i;
        }
      }
      return -1;
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
