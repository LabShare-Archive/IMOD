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
import etomo.storage.MatlabParamFile;
import etomo.storage.ModelFileFilter;
import etomo.storage.MotlFileFilter;
import etomo.storage.TiltFile;
import etomo.storage.TiltFileFilter;
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

  private final RowList rowList = new RowList();
  private final JPanel rootPanel = new JPanel();
  private final MultiLineButton btnAddFnVolume = new MultiLineButton(
      "Add Volume and Model");
  private final MultiLineButton btnSetInitMotlFile = new MultiLineButton(
      "Set Initial Motive List File");
  private final MultiLineButton btnReadTiltFile = new MultiLineButton(
      "Read Tilt File");
  private final Run3dmodButton r3bVolume;
  private final HeaderCell header1VolumeNumber = new HeaderCell();
  private final HeaderCell header1FnVolume = new HeaderCell("Volume");
  private final HeaderCell header1FnModParticle = new HeaderCell("Model");
  private final HeaderCell header1InitMotlFile = new HeaderCell("Initial");
  private final HeaderCell header1TiltRange = new HeaderCell("Tilt Range");
  private final HeaderCell header1RelativeOrient = new HeaderCell(
      "Rel. Orient.");
  private final HeaderCell header2VolumeNumber = new HeaderCell();
  private final HeaderCell header2FnVolume = new HeaderCell();
  private final HeaderCell header2FnModParticle = new HeaderCell();
  private final HeaderCell header2InitMotlFile = new HeaderCell("MOTL");
  private final HeaderCell header2TiltRangeStart = new HeaderCell("Start",
      UIParameters.INSTANCE.getNumericWidth());
  private final HeaderCell header2TiltRangeEnd = new HeaderCell("End",
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
    r3bVolume = new Run3dmodButton("Open in 3dmod", this);
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

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
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

  void setParameters(final MatlabParamFile matlabParamFile,
      boolean useInitMotlFile, boolean useTiltRange) {
    boolean initMotlFileIsExpanded = btnExpandInitMotlFile.isExpanded();
    for (int i = 0; i < matlabParamFile.getVolumeListSize(); i++) {
      VolumeRow row = addRow(new File(matlabParamFile.getFnVolume(i)),
          new File(matlabParamFile.getFnModParticle(i)));
      row.setParameters(matlabParamFile, useInitMotlFile, useTiltRange);
      row.expandInitMotlFile(initMotlFileIsExpanded);
    }
    rowList.doneSettingParameters();
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  void getParameters(final MatlabParamFile matlabParamFile) {
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
    SpacedPanel pnlBorder = new SpacedPanel();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(new EtchedBorder("Volume Table").getBorder());
    pnlBorder.add(pnlTable);
    //buttons
    JPanel pnlButtons = new JPanel();
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    btnAddFnVolume.setSize();
    pnlButtons.add(btnAddFnVolume.getComponent());
    btnSetInitMotlFile.setSize();
    pnlButtons.add(btnSetInitMotlFile.getComponent());
    btnReadTiltFile.setSize();
    pnlButtons.add(btnReadTiltFile.getComponent());
    r3bVolume.setSize();
    pnlButtons.add(r3bVolume.getComponent());
    //root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    rootPanel.add(pnlBorder.getContainer());
    rootPanel.add(pnlButtons);
  }

  private void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(r3bVolume.getActionCommand())) {
      imodVolume(menuOptions);
    }
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

  private void action(final ActionEvent event) {
    String actionCommand = event.getActionCommand();
    if (actionCommand.equals(btnAddFnVolume.getActionCommand())) {
      addVolume();
    }
    else if (actionCommand.equals(btnSetInitMotlFile.getActionCommand())) {
      setInitMotlFile();
    }
    else if (actionCommand.equals(btnReadTiltFile.getActionCommand())) {
      openTiltFile();
    }
    else {
      run3dmod(actionCommand, new Run3dmodMenuOptions());
    }
  }

  private void setToolTipText() {
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory
          .getInstance(AutodocFactory.PEET_PRM);
      String tooltip1 = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.FN_VOLUME_KEY);
      header1FnVolume.setToolTipText(tooltip1);
      header2FnVolume.setToolTipText(tooltip1);
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.FN_MOD_PARTICLE_KEY);
      btnAddFnVolume.setToolTipText(tooltip1 + "  " + tooltip);
      header1FnModParticle.setToolTipText(tooltip);
      header2FnModParticle.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParamFile.INIT_MOTL_KEY);
      btnSetInitMotlFile.setToolTipText(tooltip);
      header1InitMotlFile.setToolTipText(tooltip);
      header2InitMotlFile.setToolTipText(tooltip);
      tooltip = EtomoAutodoc
          .getTooltip(autodoc, MatlabParamFile.TILT_RANGE_KEY);
      btnReadTiltFile.setToolTipText(tooltip);
      header1TiltRange.setToolTipText(tooltip);
      header2TiltRangeStart.setToolTipText(tooltip);
      header2TiltRangeEnd.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.RELATIVE_ORIENT_KEY);
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
   * Allow the user to choose a tomogram and a model.  Only works if they choose
   * both.
   */
  private void addVolume() {
    if (!manager.setParamFile()) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + " and " + PeetDialog.OUTPUT_LABEL
          + " fields before adding tomograms.", "Entry Error");
      return;
    }
    File tomogram = null;
    File model = null;
    JFileChooser chooser = getFileChooserInstance();
    chooser.setFileFilter(new TomogramFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    tomogram = chooser.getSelectedFile();
    lastLocation = tomogram.getParentFile();
    chooser = getFileChooserInstance();
    chooser.setFileFilter(new ModelFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      model = chooser.getSelectedFile();
      lastLocation = model.getParentFile();
    }
    if (tomogram == null || model == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Please choose both a tomogram and a model", "Entry Error");
    }
    else {
      addRow(tomogram, model);
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

  private void openTiltFile() {
    VolumeRow row = rowList.getHighlightedRow();
    if (row == null) {
      UIHarness.INSTANCE.openMessageDialog("Please highlight a row.",
          "Entry Error");
      return;
    }
    JFileChooser chooser = getFileChooserInstance();
    chooser.setFileFilter(new TiltFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File file = chooser.getSelectedFile();
      lastLocation = file.getParentFile();
      TiltFile tiltFile = new TiltFile(file);
      row.setTiltRangeStart(tiltFile.getStartAngle().toString());
      row.setTiltRangeEnd(tiltFile.getEndAngle().toString());
    }
  }

  private VolumeRow addRow(final File fnVolume, final File fnModel) {
    VolumeRow row = rowList.add(manager, fnVolume, fnModel, this, pnlTable,
        layout, constraints, initMotlFileColumn, tiltRangeColumn);
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
    btnReadTiltFile.setEnabled(enable && rowList.isHighlighted()
        && useTiltRange);
    r3bVolume.setEnabled(enable && rowList.isHighlighted());
  }

  private void addListeners() {
    VTActionListener actionListener = new VTActionListener(this);
    btnAddFnVolume.addActionListener(actionListener);
    btnSetInitMotlFile.addActionListener(actionListener);
    btnReadTiltFile.addActionListener(actionListener);
    r3bVolume.addActionListener(actionListener);
  }

  private static final class RowList {
    private final List list = new ArrayList();
    private ConstPeetMetaData metaData = null;

    private int size() {
      return list.size();
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

    private void getParameters(final PeetMetaData metaData) {
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = (VolumeRow) list.get(i);
        row.getParameters(metaData);
      }
    }

    private void getParameters(final MatlabParamFile matlabParamFile) {
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
    VolumeTable volumeTable;

    VTActionListener(final VolumeTable volumeTable) {
      this.volumeTable = volumeTable;
    }

    public void actionPerformed(final ActionEvent event) {
      volumeTable.action(event);
    }
  }
}
