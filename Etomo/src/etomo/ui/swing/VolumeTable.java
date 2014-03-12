package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.PeetManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.TiltFile;
import etomo.storage.TiltFileFilter;
import etomo.storage.TiltLog;
import etomo.storage.TiltLogFileFilter;
import etomo.storage.VolumeFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstPeetMetaData;
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
 * <p> Revision 1.2  2011/02/22 21:48:15  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.49  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.48  2009/12/23 03:08:43  sueh
 * <p> bug# 1296 Fixed tooltips.
 * <p>
 * <p> Revision 1.47  2009/12/23 02:28:57  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.  Added tooltips to the actual fields in the tables instead of the column headers.
 * <p>
 * <p> Revision 1.46  2009/12/08 02:50:48  sueh
 * <p> bug# 1286 Passing parametersOnly to setParameter functions.
 * <p>
 * <p> Revision 1.45  2009/12/01 00:28:24  sueh
 * <p> bug# 1285 Added isEmpty.
 * <p>
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
final class VolumeTable implements Expandable, Highlightable, Run3dmodButtonContainer,
    Viewable, CurrentDirectory {
  public static final String rcsid = "$Id$";

  static final String FN_VOLUME_HEADER1 = "Volume";
  static final String FN_MOD_PARTICLE_HEADER1 = "Model";
  static final String INIT_MOTL_FILE_HEADER1 = "Initial";
  static final String INIT_MOTL_FILE_HEADER2 = "MOTL";
  static final String LABEL = "Volume Table";
  static final String TILT_RANGE_HEADER1_LABEL = "Tilt Range";
  private static final String TILT_RANGE_MULTI_AXES_HEADER1_LABEL = "Missing Wedge";
  private static final String TILT_RANGE_MULTI_AXES_HEADER2_LABEL = "Mask";

  private final RowList rowList = new RowList();
  private final JPanel rootPanel = new JPanel();
  private final MultiLineButton btnReadTiltFile = new MultiLineButton("Read tilt file");
  private final Run3dmodButton r3bVolume;
  private final HeaderCell header1VolumeNumber = new HeaderCell("Vol #");
  private final HeaderCell header1FnVolume = new HeaderCell(FN_VOLUME_HEADER1);
  private final HeaderCell header1FnModParticle = new HeaderCell(FN_MOD_PARTICLE_HEADER1);
  private final HeaderCell header1InitMotlFile = new HeaderCell(INIT_MOTL_FILE_HEADER1);
  private final HeaderCell header1TiltRange = new HeaderCell(TILT_RANGE_HEADER1_LABEL);
  private final HeaderCell header1TiltRangeMultiAxes = new HeaderCell(
      TILT_RANGE_MULTI_AXES_HEADER1_LABEL);
  private final HeaderCell header2VolumeNumber = new HeaderCell();
  private final HeaderCell header2FnVolume = new HeaderCell();
  private final HeaderCell header2FnModParticle = new HeaderCell();
  private final HeaderCell header2InitMotlFile = new HeaderCell(INIT_MOTL_FILE_HEADER2);
  private final HeaderCell header2TiltRangeStart = new HeaderCell("Min",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header2TiltRangeEnd = new HeaderCell("Max",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header2TiltRangeMultiAxes = new HeaderCell(
      TILT_RANGE_MULTI_AXES_HEADER2_LABEL);

  private final Column initMotlFileColumn = new Column();
  private final Column tiltRangeColumn = new Column();
  private final VolumeFileFilter volumeFileFilter = new VolumeFileFilter();
  private final MultiLineButton btnMoveUp = new MultiLineButton("Up");
  private final MultiLineButton btnMoveDown = new MultiLineButton("Down");
  private final MultiLineButton btnInsertRow = new MultiLineButton("Insert");
  private final MultiLineButton btnDeleteRow = new MultiLineButton("Delete");
  private final MultiLineButton btnCopyRow = new MultiLineButton("Dup");
  private final JPanel pnlTableButtons = new JPanel();
  private final JPanel pnlBottomButtons = new JPanel();
  private final Component verticalRigidArea2 = Box.createRigidArea(FixedDim.x0_y10);
  private final Component horizontalRigidArea2 = Box.createRigidArea(FixedDim.x3_y0);
  private final JPanel pnlSideButtons = new JPanel();
  private final EtomoPanel pnlBorder = new EtomoPanel();
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();

  private Viewport viewport;
  private final ExpandButton btnExpandFnVolume;
  private final ExpandButton btnExpandFnModParticle;
  private final ExpandButton btnExpandInitMotlFile;
  private final ExpandButton btnExpandTiltRangeMultiAxes;
  private final PeetManager manager;
  private final PeetDialog parent;

  private boolean useInitMotlFile = true;
  private boolean useTiltRange = true;
  private File currentDirectory = null;
  private Component verticalRigidArea1 = null;
  private Component horizontalRigidArea1 = null;
  private boolean tiltRangeMultiAxes = false;

  private VolumeTable(final PeetManager manager, final PeetDialog parent) {
    this.manager = manager;
    this.parent = parent;
    // construction
    viewport = new Viewport(this, EtomoDirector.INSTANCE.getUserConfiguration()
        .getPeetTableSize().getInt(), parent.getSetupJComponent(), null, null, "Volume");
    btnExpandFnVolume = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandFnVolume.setName(FN_VOLUME_HEADER1);
    btnExpandFnModParticle = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandFnModParticle.setName(FN_MOD_PARTICLE_HEADER1);
    btnExpandInitMotlFile = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandTiltRangeMultiAxes = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandFnModParticle.setName(INIT_MOTL_FILE_HEADER1);
    r3bVolume = Run3dmodButton.get3dmodInstance("Open in 3dmod", this);
    createPanel();
    updateDisplay();
    setToolTipText();
  }

  static VolumeTable getInstance(final PeetManager manager, final PeetDialog parent) {
    VolumeTable instance = new VolumeTable(manager, parent);
    instance.addListeners();
    return instance;
  }

  boolean isTiltRangeMultiAxes() {
    return tiltRangeMultiAxes;
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

  public File getCurrentDirectory() {
    if (currentDirectory == null) {
      return new File(manager.getPropertyUserDir());
    }
    return currentDirectory;
  }

  public void setCurrentDirectory(File file) {
    currentDirectory = file;
  }

  static String getTiltRangeMultiAxesLabel() {
    return TILT_RANGE_MULTI_AXES_HEADER1_LABEL + " "
        + TILT_RANGE_MULTI_AXES_HEADER2_LABEL;
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
    else if (button == btnExpandTiltRangeMultiAxes) {
      rowList.expandTiltRangeMultiAxes(btnExpandTiltRangeMultiAxes.isExpanded());
    }
    refreshHorizontalPadding();
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

  void getParameters(final PeetMetaData metaData) {
    rowList.getParameters(metaData);
  }

  void setParameters(final ConstPeetMetaData metaData) {
    rowList.setParameters(metaData);
  }

  void convertCopiedPaths(final String origDatasetDir) {
    rowList.convertCopiedPaths(origDatasetDir);
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

  void setParameters(final MatlabParam matlabParamFile, final boolean useInitMotlFile,
      final boolean useTiltRange, final boolean tiltRangeMultiAxes, final File importDir) {
    boolean initMotlFileIsExpanded = btnExpandInitMotlFile.isExpanded();
    String userDir = null;
    if (importDir != null) {
      userDir = System.setProperty("user.dir", importDir.getAbsolutePath());
    }
    for (int i = 0; i < matlabParamFile.getVolumeListSize(); i++) {
      VolumeRow row = addRow(matlabParamFile.getFnVolume(i),
          matlabParamFile.getFnModParticle(i), matlabParamFile.getTiltRangeMultiAxes(i));
      row.setParameters(matlabParamFile, useInitMotlFile, useTiltRange,
          tiltRangeMultiAxes);
      row.expandInitMotlFile(initMotlFileIsExpanded);
    }
    refreshVerticalPadding();
    refreshHorizontalPadding();
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
    rowList.getParameters(matlabParamFile, tiltRangeMultiAxes);
  }

  public int size() {
    return rowList.size();
  }

  boolean isEmpty() {
    return rowList.isEmpty();
  }

  void updateDisplay(final boolean useInitMotlFile, final boolean useTiltRange,
      final boolean tiltRangeMultiAxes) {
    this.useInitMotlFile = useInitMotlFile;
    this.useTiltRange = useTiltRange;
    initMotlFileColumn.setEnabled(useInitMotlFile);
    tiltRangeColumn.setEnabled(useTiltRange);
    if (useTiltRange && tiltRangeMultiAxes != this.tiltRangeMultiAxes) {
      this.tiltRangeMultiAxes = tiltRangeMultiAxes;
      pnlTable.removeAll();
      display();
      rowList.remove();
      rowList.display(viewport);
      refreshHorizontalPadding();
      manager.getMainPanel().repaint();
    }
    updateDisplay();
  }

  private void createPanel() {
    buildTable();
    // border
    pnlBorder.setLayout(new BoxLayout(pnlBorder, BoxLayout.X_AXIS));
    pnlBorder.add(pnlTable);
    pnlBorder.add(viewport.getPagingPanel());
    // buttons -side
    pnlSideButtons.setLayout(new BoxLayout(pnlSideButtons, BoxLayout.Y_AXIS));
    pnlSideButtons.add(btnMoveUp.getComponent());
    pnlSideButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSideButtons.add(btnMoveDown.getComponent());
    pnlSideButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSideButtons.add(btnInsertRow.getComponent());
    pnlSideButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSideButtons.add(btnDeleteRow.getComponent());
    pnlSideButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSideButtons.add(btnCopyRow.getComponent());
    pnlSideButtons.add(Box.createVerticalGlue());
    pnlSideButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    // buttons - bottom
    pnlBottomButtons.setLayout(new GridLayout(1, 2, 0, 0));
    pnlBottomButtons.add(Box.createHorizontalGlue());
    pnlBottomButtons.add(r3bVolume.getComponent());
    pnlBottomButtons.add(Box.createHorizontalGlue());
    pnlBottomButtons.add(btnReadTiltFile.getComponent());
    pnlBottomButtons.add(Box.createHorizontalGlue());
    // Table and side buttons
    pnlTableButtons.setLayout(new BoxLayout(pnlTableButtons, BoxLayout.Y_AXIS));
    pnlTableButtons.add(pnlBorder);
    refreshVerticalPadding();
    // root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.X_AXIS));
    rootPanel.setBorder(new EtchedBorder(LABEL).getBorder());
    rootPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    rootPanel.add(pnlTableButtons);
    refreshHorizontalPadding();
  }

  private void buildTable() {
    // columns
    initMotlFileColumn.add(header1InitMotlFile);
    initMotlFileColumn.add(header2InitMotlFile);
    tiltRangeColumn.add(header1TiltRange);
    tiltRangeColumn.add(header2TiltRangeStart);
    tiltRangeColumn.add(header2TiltRangeEnd);
    tiltRangeColumn.add(header1TiltRangeMultiAxes);
    tiltRangeColumn.add(header2TiltRangeMultiAxes);
    // table
    pnlTable.setLayout(layout);
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    display();
  }

  /**
   * Pad the table on the right side based on the number of character in the three columns
   * that change size.  6.3 is an estimate of how much the table grows on average with
   * each character.
   */
  private void refreshHorizontalPadding() {
    if (horizontalRigidArea1 != null) {
      rootPanel.remove(horizontalRigidArea2);
      rootPanel.remove(pnlSideButtons);
      rootPanel.remove(horizontalRigidArea1);
    }
    int maxRowTextSize = rowList.getMaxRowTextSize(tiltRangeMultiAxes);
    if (maxRowTextSize <= 50) {
      horizontalRigidArea1 = Box.createRigidArea(FixedDim.x181_y0);
    }
    else {
      horizontalRigidArea1 = Box.createRigidArea(new Dimension(Math.max(
          (int) Math.round(181 - (maxRowTextSize - 50) * 6.3), 8), 0));
    }
    rootPanel.add(horizontalRigidArea1);
    rootPanel.add(pnlSideButtons);
    rootPanel.add(horizontalRigidArea2);
  }

  private void refreshVerticalPadding() {
    int size = rowList.size();
    int noPadding = 3;
    if (verticalRigidArea1 != null) {
      pnlTableButtons.remove(verticalRigidArea1);
      pnlTableButtons.remove(pnlBottomButtons);
      pnlTableButtons.remove(verticalRigidArea2);
    }
    int height = Math.max(10 + (noPadding - size) * 20, 10);
    verticalRigidArea1 = Box.createRigidArea(new Dimension(0, height));
    pnlTableButtons.add(verticalRigidArea1);
    pnlTableButtons.add(pnlBottomButtons);
    pnlTableButtons.add(verticalRigidArea2);
  }

  private void imodVolume(Run3dmodMenuOptions menuOptions) {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      throw new IllegalStateException("r3bVolume enabled when no row is highlighted");
    }
    row.imodVolume(menuOptions);
  }

  private void display() {
    constraints.fill = GridBagConstraints.BOTH;
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.gridheight = 1;
    constraints.weighty = 1.0;
    // First header row
    constraints.weightx = 1.0;
    constraints.gridwidth = 2;
    header1VolumeNumber.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header1FnVolume.add(pnlTable, layout, constraints);
    btnExpandFnVolume.add(pnlTable, layout, constraints);
    header1FnModParticle.add(pnlTable, layout, constraints);
    btnExpandFnModParticle.add(pnlTable, layout, constraints);
    header1InitMotlFile.add(pnlTable, layout, constraints);
    btnExpandInitMotlFile.add(pnlTable, layout, constraints);
    if (!tiltRangeMultiAxes) {
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      header1TiltRange.add(pnlTable, layout, constraints);
    }
    else {
      header1TiltRangeMultiAxes.add(pnlTable, layout, constraints);
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      btnExpandTiltRangeMultiAxes.add(pnlTable, layout, constraints);
    }
    // Second header row
    constraints.gridwidth = 2;
    header2VolumeNumber.add(pnlTable, layout, constraints);
    header2FnVolume.add(pnlTable, layout, constraints);
    header2FnModParticle.add(pnlTable, layout, constraints);
    header2InitMotlFile.add(pnlTable, layout, constraints);
    if (!tiltRangeMultiAxes) {
      constraints.gridwidth = 1;
      header2TiltRangeStart.add(pnlTable, layout, constraints);
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      header2TiltRangeEnd.add(pnlTable, layout, constraints);
    }
    else {
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      header2TiltRangeMultiAxes.add(pnlTable, layout, constraints);
    }
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

  HeaderCell getTiltRangeMultiAxesHeaderCell() {
    return header1TiltRangeMultiAxes;
  }

  HeaderCell getInitMotlFileHeaderCell() {
    return header1InitMotlFile;
  }

  HeaderCell getTiltRangeHeaderCell() {
    return header1TiltRange;
  }

  public void msgViewportPaged() {
    rowList.remove();
    rowList.display(viewport);
    UIHarness.INSTANCE.pack(manager);
  }

  private void action(final String command, final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnInsertRow.getActionCommand())) {
      insertRow();
    }
    else if (command.equals(btnReadTiltFile.getActionCommand())) {
      openTiltFile();
    }
    else if (command.equals(btnDeleteRow.getActionCommand())) {
      deleteRow(rowList.getHighlightedRow());
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
    return rowList.validateRun(tiltRangeRequired, tiltRangeMultiAxes);
  }

  private void deleteRow(VolumeRow row) {
    rowList.remove();
    int index = rowList.delete(row, this, pnlTable, layout, constraints);
    rowList.highlight(index);
    viewport.adjustViewport(index);
    rowList.display(viewport);
    refreshVerticalPadding();
    refreshHorizontalPadding();
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  private void setToolTipText() {
    btnInsertRow.setToolTipText("Add a new row to the table.");
    btnReadTiltFile.setToolTipText("Fill in the tilt range for the highlighted row by "
        + "selecting a file with tilt angles.");
    r3bVolume.setToolTipText("Open the volume and model for the highlighted row in "
        + "3dmod.");
    btnCopyRow.setToolTipText("Create a new row that is a duplicate of the highlighted "
        + "row.");
    btnMoveUp.setToolTipText("Move highlighted row up in the table.");
    btnMoveDown.setToolTipText("Move highlighted row down in the table.");
    btnDeleteRow.setToolTipText("Remove highlighted row from table.");
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

  void pack() {
    refreshHorizontalPadding();
  }

  /**
   * Allow the user to choose a tomogram and a model and add them to the table
   * in a new row.  The tomogram is required.  The model is optional.
   */
  private void insertRow() {
    if (!manager.setParamFile()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Please set the "
          + PeetDialog.DIRECTORY_LABEL + " and " + PeetDialog.FN_OUTPUT_LABEL
          + " fields before adding rows.", "Entry Error");
      return;
    }
    addRow();
    viewport.adjustViewport(rowList.size() - 1);
    rowList.remove();
    rowList.display(viewport);
    refreshVerticalPadding();
    refreshHorizontalPadding();
    updateDisplay();
    parent.msgVolumeTableSizeChanged();
    UIHarness.INSTANCE.pack(manager);
  }

  private void openTiltFile() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Please highlight a row.",
          "Entry Error");
      return;
    }
    JFileChooser chooser = parent.getFileChooserInstance();
    chooser.addChoosableFileFilter(new TiltFileFilter());
    // Add the default file filter (tilt log)
    TiltLogFileFilter tiltLogFileFilter = new TiltLogFileFilter();
    chooser.addChoosableFileFilter(tiltLogFileFilter);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File file = chooser.getSelectedFile();
      parent.setLastLocation(file.getParentFile());
      if (tiltLogFileFilter.accept(file) || file.getName().endsWith(".log")) {
        try {
          TiltLog tiltLog = TiltLog.getInstance(file);
          tiltLog.read();
          row.setTiltRangeMin(tiltLog.getMinAngle());
          row.setTiltRangeMax(tiltLog.getMaxAngle());
        }
        catch (LogFile.LockException e) {
          e.printStackTrace();
          UIHarness.INSTANCE
              .openMessageDialog(
                  manager,
                  "Unable to open tilt log " + file.getAbsolutePath() + "\n"
                      + e.getMessage(), "File Open Failure");
        }
      }
      else {
        TiltFile tiltFile = TiltFile.getInstance(manager, file);
        row.setTiltRangeMin(tiltFile.getMinAngle().toString());
        row.setTiltRangeMax(tiltFile.getMaxAngle().toString());
      }
    }
  }

  private VolumeRow addRow() {
    VolumeRow row = rowList.add(manager, this, pnlTable, layout, constraints,
        initMotlFileColumn, tiltRangeColumn, volumeFileFilter);
    row.expandFnVolume(btnExpandFnVolume.isExpanded());
    row.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    return row;
  }

  private VolumeRow addRow(final String fnVolume, final String fnModParticle,
      final String tiltRangeMultiAxes) {
    VolumeRow row = rowList.add(manager, fnVolume, fnModParticle, tiltRangeMultiAxes,
        this, pnlTable, layout, constraints, initMotlFileColumn, tiltRangeColumn,
        volumeFileFilter);
    row.expandFnVolume(btnExpandFnVolume.isExpanded());
    row.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    return row;
  }

  /**
   * Copy volume, model, initial motl, and tilt range.
   */
  private void addRow(VolumeRow fromRow) {
    VolumeRow row = rowList.add(fromRow, initMotlFileColumn, tiltRangeColumn);
    row.expandFnVolume(btnExpandFnVolume.isExpanded());
    row.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    row.setInitMotlFile(fromRow.getExpandedInitMotlFile());
    row.expandInitMotlFile(btnExpandInitMotlFile.isExpanded());
    row.expandTiltRangeMultiAxes(btnExpandTiltRangeMultiAxes.isExpanded());
    // TODO are these duplicate functionality from VolumeRow(VolumeRow...)?
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
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Can't move the row up.  Its at the top.", "Wrong Row", AxisID.ONLY);
      return;
    }
    // rowList.removeRows(index - 1);
    rowList.moveRowUp(index);
    viewport.adjustViewport(index - 1);
    rowList.remove();
    rowList.display(viewport);
    refreshVerticalPadding();
    refreshHorizontalPadding();
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
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Can't move the row down.  Its at the bottom.", "Wrong Row", AxisID.ONLY);
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
    btnExpandTiltRangeMultiAxes.setEnabled(enable && useTiltRange);
    btnReadTiltFile.setEnabled(enable && highlighted && useTiltRange);
    r3bVolume.setEnabled(enable && highlighted);
    btnDeleteRow.setEnabled(enable && highlighted);
    btnMoveUp.setEnabled(enable && highlighted && rowList.getHighlightedRowIndex() > 0);
    btnMoveDown.setEnabled(enable && highlighted
        && rowList.getHighlightedRowIndex() < rowList.size() - 1);
    btnCopyRow.setEnabled(enable && highlighted);
  }

  private void addListeners() {
    VTActionListener actionListener = new VTActionListener(this);
    btnInsertRow.addActionListener(actionListener);
    btnReadTiltFile.addActionListener(actionListener);
    r3bVolume.addActionListener(actionListener);
    btnDeleteRow.addActionListener(actionListener);
    btnMoveUp.addActionListener(actionListener);
    btnMoveDown.addActionListener(actionListener);
    btnCopyRow.addActionListener(actionListener);
  }

  private static final class RowList {
    private final List<VolumeRow> list = new ArrayList<VolumeRow>();
    private ConstPeetMetaData metaData = null;

    private int size() {
      return list.size();
    }

    private boolean isEmpty() {
      return list.isEmpty();
    }

    private void remove() {
      for (int i = 0; i < list.size(); i++) {
        list.get(i).remove();
      }
    }

    /**
     * Returns the index where the row used to be (points to the next row).
     * @param row
     * @param parent
     * @param panel
     * @param layout
     * @param constraints
     * @return
     */
    private synchronized int delete(VolumeRow row, final Highlightable parent,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      int index = -1;
      if (row != null) {
        index = row.getIndex();
        list.remove(index);
        for (int i = index; i < list.size(); i++) {
          list.get(i).setIndex(i);
        }
      }
      return index;
    }

    private synchronized VolumeRow add(final VolumeRow volumeRow,
        Column initMotlFileColumn, Column tiltRangeColumn) {
      VolumeRow row = VolumeRow.getInstance(volumeRow, list.size());
      list.add(row);
      row.registerInitMotlFileColumn(initMotlFileColumn);
      row.registerTiltRangeColumn(tiltRangeColumn);
      row.setNames();
      return row;
    }

    private synchronized VolumeRow add(final BaseManager manager,
        final VolumeTable table, final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints, final Column initMotlFileColumn,
        final Column tiltRangeColumn, final VolumeFileFilter volumeFileFilter) {
      VolumeRow row = VolumeRow.getInstance(manager, list.size(), table, panel, layout,
          constraints, volumeFileFilter);
      list.add(row);
      row.registerInitMotlFileColumn(initMotlFileColumn);
      row.registerTiltRangeColumn(tiltRangeColumn);
      row.setNames();
      // When this function is used to load from the .epe and .prm files,
      // metadata must be set before MatlabParamFile data. Wait until row is
      // added, then set from metadata.
      row.setParameters(metaData);
      return row;
    }

    private synchronized VolumeRow add(final BaseManager manager, final String fnVolume,
        final String fnModParticle, final String tiltRangeMultiAxes,
        final VolumeTable table, final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints, Column initMotlFileColumn,
        Column tiltRangeColumn, final VolumeFileFilter volumeFileFilter) {
      VolumeRow row = VolumeRow.getInstance(manager, fnVolume, fnModParticle,
          tiltRangeMultiAxes, list.size(), table, panel, layout, constraints,
          volumeFileFilter);
      list.add(row);
      row.registerInitMotlFileColumn(initMotlFileColumn);
      row.registerTiltRangeColumn(tiltRangeColumn);
      row.setNames();
      // When this function is used to load from the .epe and .prm files,
      // metadata must be set before MatlabParamFile data. Wait until row is
      // added, then set from metadata.
      row.setParameters(metaData);
      return row;
    }

    private void convertCopiedPaths(final String origDatasetDir) {
      for (int i = 0; i < list.size(); i++) {
        list.get(i).convertCopiedPaths(origDatasetDir);
      }
    }

    private boolean isIncorrectPaths() {
      for (int i = 0; i < list.size(); i++) {
        if (list.get(i).isIncorrectPaths()) {
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
        if (!list.get(i).fixIncorrectPaths(choosePathEveryRow)) {
          return false;
        }
      }
      return true;
    }

    /**
     * Swap two rows.
     */
    private void moveRowUp(final int rowIndex) {
      VolumeRow rowMoveUp = list.remove(rowIndex);
      VolumeRow rowMoveDown = list.remove(rowIndex - 1);
      list.add(rowIndex - 1, rowMoveUp);
      list.add(rowIndex, rowMoveDown);
    }

    private void moveRowDown(final int rowIndex) {
      VolumeRow rowMoveUp = list.remove(rowIndex + 1);
      VolumeRow rowMoveDown = list.remove(rowIndex);
      list.add(rowIndex, rowMoveUp);
      list.add(rowIndex + 1, rowMoveDown);
    }

    /**
     * Highlight the row in list at rowIndex.
     * @param rowIndex
     */
    private void highlight(final int rowIndex) {
      if (rowIndex >= 0 && rowIndex < list.size()) {
        list.get(rowIndex).setHighlighterSelected(true);
      }
    }

    /**
     * Renumber the table starting from the row in the ArrayList at startIndex.
     * @param startIndex
     */
    private void reindex(final int startIndex) {
      for (int i = startIndex; i < list.size(); i++) {
        list.get(i).setIndex(i);
      }
    }

    /**
     * Validate for running.  Returns error message.
     * @return null if valid
     */
    private String validateRun(final boolean tiltRangeRequired,
        final boolean tiltRangeMultiAxes) {
      if (list.size() < 1) {
        return "Must enter at least one row in " + LABEL;
      }
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = list.get(i);
        String errorMessage = row.validateRun(tiltRangeRequired, tiltRangeMultiAxes);
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
        VolumeRow row = list.get(i);
        row.getParameters(metaData);
      }
    }

    private void getParameters(final MatlabParam matlabParamFile,
        final boolean tiltRangeMultiAxes) {
      matlabParamFile.setVolumeListSize(list.size());
      for (int i = 0; i < list.size(); i++) {
        list.get(i).getParameters(matlabParamFile, tiltRangeMultiAxes);
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
        list.get(i).display(i, viewport);
      }
    }

    private void expandFnVolume(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        list.get(i).expandFnVolume(expanded);
      }
    }

    private void expandFnModParticle(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        list.get(i).expandFnModParticle(expanded);
      }
    }

    private void expandInitMotl(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        list.get(i).expandInitMotlFile(expanded);
      }
    }

    private void expandTiltRangeMultiAxes(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        list.get(i).expandTiltRangeMultiAxes(expanded);
      }
    }

    private boolean isHighlighted() {
      for (int i = 0; i < list.size(); i++) {
        if (list.get(i).isHighlighted()) {
          return true;
        }
      }
      return false;
    }

    private VolumeRow getHighlightedRow() {
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = list.get(i);
        if (row.isHighlighted()) {
          return row;
        }
      }
      return null;
    }

    private int getMaxRowTextSize(final boolean tiltRangeMultiAxes) {
      int maxRowTextSize = 0;
      for (int i = 0; i < list.size(); i++) {
        maxRowTextSize = Math.max(list.get(i).getTextSize(tiltRangeMultiAxes),
            maxRowTextSize);
      }
      return maxRowTextSize;
    }

    private int getHighlightedRowIndex() {
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = list.get(i);
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
