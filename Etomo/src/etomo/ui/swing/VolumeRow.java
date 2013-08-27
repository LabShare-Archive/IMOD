package etomo.ui.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import etomo.BaseManager;
import etomo.process.ImodManager;
import etomo.storage.MatlabParam;
import etomo.storage.ModelFileFilter;
import etomo.storage.MotlFileFilter;
import etomo.storage.VolumeFileFilter;
import etomo.type.ConstPeetMetaData;
import etomo.type.PeetMetaData;
import etomo.type.Run3dmodMenuOptions;
import etomo.util.FilePath;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.32  2009/12/23 02:28:34  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.  Added tooltips to the actual fields in the tables instead of the column headers.
 * <p>
 * <p> Revision 1.31  2009/12/01 00:27:59  sueh
 * <p> bug# 1285 Factored MissingWedgeCompensation out of PeetDialog.
 * <p>
 * <p> Revision 1.30  2009/11/20 23:05:24  sueh
 * <p> bug# 1280 Added getFnVolumeFile and getFnModParticleFile.  Removed
 * <p> getRelativeOrientX, Y, and Z.
 * <p>
 * <p> Revision 1.29  2009/10/29 20:01:10  sueh
 * <p> bug# 1280 Added sets and gets for copying row data.  Named more fields
 * <p> for ui testing.
 * <p>
 * <p> Revision 1.28  2009/10/16 23:56:48  sueh
 * <p> bug# 1234 In validateRun added tiltRange validation.
 * <p>
 * <p> Revision 1.27  2009/10/15 23:40:52  sueh
 * <p> bug# 1274 In validateRun returning the error string instead of boolean
 * <p> because the tab must be changed before the error message can be
 * <p> popped up.
 * <p>
 * <p> Revision 1.26  2009/09/28 18:35:39  sueh
 * <p> bug# 1235 Added setNames.
 * <p>
 * <p> Revision 1.25  2009/04/27 18:06:42  sueh
 * <p> bug# 1211 Added checkIncorrectPaths, fixIncorrectPaths, and
 * <p> fixIncorrectPath.
 * <p>
 * <p> Revision 1.24  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.23  2008/10/10 20:44:13  sueh
 * <p> bug# 1142 Commented setParameters(ConstPeetMetaData).
 * <p>
 * <p> Revision 1.22  2008/10/01 22:55:07  sueh
 * <p> bug# 1113 Adding index and vewport parameters to display().
 * <p>
 * <p> Revision 1.21  2008/04/02 17:35:29  sueh
 * <p> bug# 1098 Improved user error messages.
 * <p>
 * <p> Revision 1.20  2008/04/02 02:28:20  sueh
 * <p> bug# 1097 FieldCell can now return a matlab syntax instance.
 * <p>
 * <p> Revision 1.19  2007/07/25 22:59:15  sueh
 * <p> bug# 1027 Change start and end tilt range angles to min and max angles.
 * <p>
 * <p> Revision 1.18  2007/07/10 00:44:12  sueh
 * <p> bug# 1022 In validateRun, added the row # to the error message.
 * <p>
 * <p> Revision 1.17  2007/06/06 16:07:46  sueh
 * <p> bug# 1013 Added setFnModParticle() and validateRun().
 * <p>
 * <p> Revision 1.16  2007/05/16 02:35:58  sueh
 * <p> bug# 964 Added getIndex(), remove(), and setIndex(int).
 * <p>
 * <p> Revision 1.15  2007/05/07 17:23:59  sueh
 * <p> bug# 964 Changed MatlabParamFile to MatlabParam.
 * <p>
 * <p> Revision 1.14  2007/04/09 21:24:44  sueh
 * <p> bug# 964 Removed unecessary function MatlabParamFile.isRelativeOrientSet().
 * <p>
 * <p> Revision 1.13  2007/04/02 21:53:47  sueh
 * <p> bug# 964 Added FieldCell.editable to make instances of FieldCell that can't be
 * <p> edited.  This allows FieldCell.setEditable and setEnabled to be called without
 * <p> checking whether a field should be editable.
 * <p>
 * <p> Revision 1.12  2007/04/02 16:04:16  sueh
 * <p> bug# 964 Not weighting the number and highlight buttons, so they will stay small.
 * <p>
 * <p> Revision 1.11  2007/03/30 23:55:18  sueh
 * <p> bug# 964 Changes to accomodate parsing improvements in MatlabParamFile
 * <p>
 * <p> Revision 1.10  2007/03/27 19:32:49  sueh
 * <p> bug# 964 Number the rows.
 * <p>
 * <p> Revision 1.9  2007/03/27 00:07:21  sueh
 * <p> bug# 964 Added imodVolume() to open fnVolume and fnModParticle in 3dmod.
 * <p>
 * <p> Revision 1.8  2007/03/26 18:40:53  sueh
 * <p> bug# 964 Removed functionality that shows/hides columns.  Fixed bug in
 * <p> setting initMOTL and relativeOrient.
 * <p>
 * <p> Revision 1.7  2007/03/21 19:49:12  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.  Removed some gets/sets
 * <p> and replaced them with get/setParameters.
 * <p>
 * <p> Revision 1.6  2007/03/20 23:12:09  sueh
 * <p> bug# 964 Added FieldCell.getExpandableInstance() which is disabled and cannot
 * <p> be enabled.
 * <p>
 * <p> Revision 1.5  2007/03/20 00:46:41  sueh
 * <p> bug# 964 Removed the spinner version of Initial MOTL.  Hiding/showing the
 * <p> Initial MOTL column.
 * <p>
 * <p> Revision 1.4  2007/03/15 21:54:48  sueh
 * <p> bug# 964 Loading data from .prm file.  Changing the field names to match the
 * <p> .prm file format.
 * <p>
 * <p> Revision 1.3  2007/03/01 01:46:32  sueh
 * <p> bug# 964 Added highlighting, model and motl.
 * <p>
 * <p> Revision 1.2  2007/02/22 20:39:13  sueh
 * <p> bug# 964 Displaying the Tomogram column.
 * <p>
 * <p> Revision 1.1  2007/02/20 20:37:13  sueh
 * <p> bug# 964 Represents each row of the volume table.
 * <p> </p>
 */

final class VolumeRow implements Highlightable {
  public static final String rcsid = "$Id$";

  private final HeaderCell number = new HeaderCell();

  private final HighlighterButton btnHighlighter;
  private final FieldCell fnVolume;
  private final FileButtonCell fbFnVolume;
  private final FieldCell fnModParticle;
  private final FileButtonCell fbFnModParticle;
  private final FieldCell initMotlFile;
  private final FileButtonCell fbInitMotlFile;
  private final FieldCell tiltRangeMin;
  private final FieldCell tiltRangeMax;
  private final VolumeTable table;
  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final BaseManager manager;
  private final FieldCell tiltRangeMultiAxes;
  private final FileButtonCell fbTiltRangeMultiAxes;

  private int imodIndex = -1;
  private int index;

  static VolumeRow getInstance(final BaseManager manager, final int index,
      final VolumeTable table, final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints, final VolumeFileFilter volumeFileFilter) {
    VolumeRow instance = new VolumeRow(manager, index, table, panel, layout, constraints,
        volumeFileFilter);
    instance.addActionTargets();
    instance.setTooltips();
    return instance;
  }

  static VolumeRow getInstance(final BaseManager manager, final File fnVolume,
      final File fnModParticle, final File tiltRangeMultiAxesFile, final int index,
      final VolumeTable table, final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints, final VolumeFileFilter volumeFileFilter) {
    VolumeRow instance = new VolumeRow(manager, fnVolume, fnModParticle,
        tiltRangeMultiAxesFile, index, table, panel, layout, constraints,
        volumeFileFilter);
    instance.addActionTargets();
    instance.setTooltips();
    return instance;
  }

  static VolumeRow getInstance(final BaseManager manager, final String fnVolume,
      final String fnModParticle, final String tiltRangeMultiAxesFile, final int index,
      final VolumeTable table, final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints, final VolumeFileFilter volumeFileFilter) {
    VolumeRow instance = new VolumeRow(manager, fnVolume, fnModParticle,
        tiltRangeMultiAxesFile, index, table, panel, layout, constraints,
        volumeFileFilter);
    instance.addActionTargets();
    instance.setTooltips();
    return instance;
  }

  static VolumeRow getInstance(final VolumeRow volumeRow, final int index) {
    VolumeRow instance = new VolumeRow(volumeRow, index);
    instance.addActionTargets();
    instance.setTooltips();
    return instance;
  }

  private VolumeRow(final BaseManager manager, final int index, final VolumeTable table,
      final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints, final VolumeFileFilter volumeFileFilter) {
    this.manager = manager;
    this.index = index;
    this.table = table;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    number.setText(String.valueOf(index + 1));
    btnHighlighter = HighlighterButton.getInstance(this, table);
    String rootDir = manager.getPropertyUserDir();
    fnVolume = FieldCell.getExpandableInstance(rootDir);
    // setValue(fnVolume, fnVolumeFile);
    fbFnVolume = FileButtonCell.getInstance(table);
    fbFnVolume.setFileFilter(volumeFileFilter);
    fnModParticle = FieldCell.getExpandableInstance(rootDir);
    // setValue(fnModParticle, fnModParticleFile);
    fbFnModParticle = FileButtonCell.getInstance(table);
    fbFnModParticle.setFileFilter(new ModelFileFilter());
    initMotlFile = FieldCell.getExpandableInstance(rootDir);
    fbInitMotlFile = FileButtonCell.getInstance(table);
    fbInitMotlFile.setFileFilter(new MotlFileFilter());
    tiltRangeMin = FieldCell.getEditableMatlabInstance();
    tiltRangeMax = FieldCell.getEditableMatlabInstance();
    tiltRangeMultiAxes = FieldCell.getExpandableInstance(rootDir);
    fbTiltRangeMultiAxes = FileButtonCell.getInstance(table);
    fbTiltRangeMultiAxes.setFileFilter(volumeFileFilter);
  }

  private VolumeRow(final VolumeRow volumeRow, final int index) {
    manager = volumeRow.manager;
    this.index = index;
    table = volumeRow.table;
    panel = volumeRow.panel;
    layout = volumeRow.layout;
    constraints = volumeRow.constraints;
    String rootDir = manager.getPropertyUserDir();
    number.setText(String.valueOf(index + 1));
    btnHighlighter = HighlighterButton.getInstance(this, table);
    fnVolume = FieldCell.getInstance(volumeRow.fnVolume);
    fbFnVolume = FileButtonCell.getInstance(volumeRow.fbFnVolume);
    fnModParticle = FieldCell.getInstance(volumeRow.fnModParticle);
    fbFnModParticle = FileButtonCell.getInstance(volumeRow.fbFnModParticle);
    initMotlFile = FieldCell.getInstance(volumeRow.initMotlFile);
    fbInitMotlFile = FileButtonCell.getInstance(volumeRow.fbInitMotlFile);
    tiltRangeMin = FieldCell.getInstance(volumeRow.tiltRangeMin);
    tiltRangeMax = FieldCell.getInstance(volumeRow.tiltRangeMax);
    tiltRangeMultiAxes = FieldCell.getInstance(volumeRow.tiltRangeMultiAxes);
    fbTiltRangeMultiAxes = FileButtonCell.getInstance(volumeRow.fbTiltRangeMultiAxes);
  }

  private VolumeRow(final BaseManager manager, final File fnVolumeFile,
      final File fnModParticleFile, final File tiltRangeMultiAxesFile, final int index,
      final VolumeTable table, final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints, final VolumeFileFilter volumeFileFilter) {
    this.manager = manager;
    this.index = index;
    this.table = table;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    number.setText(String.valueOf(index + 1));
    btnHighlighter = HighlighterButton.getInstance(this, table);
    String rootDir = manager.getPropertyUserDir();
    fnVolume = FieldCell.getExpandableInstance(rootDir);
    setValue(fnVolume, fnVolumeFile);
    fbFnVolume = FileButtonCell.getInstance(table);
    fbFnVolume.setFileFilter(volumeFileFilter);
    fnModParticle = FieldCell.getExpandableInstance(rootDir);
    setValue(fnModParticle, fnModParticleFile);
    fbFnModParticle = FileButtonCell.getInstance(table);
    fbFnModParticle.setFileFilter(new ModelFileFilter());
    initMotlFile = FieldCell.getExpandableInstance(rootDir);
    fbInitMotlFile = FileButtonCell.getInstance(table);
    fbInitMotlFile.setFileFilter(new MotlFileFilter());
    tiltRangeMin = FieldCell.getEditableMatlabInstance();
    tiltRangeMax = FieldCell.getEditableMatlabInstance();
    tiltRangeMultiAxes = FieldCell.getExpandableInstance(rootDir);
    setValue(tiltRangeMultiAxes, tiltRangeMultiAxesFile);
    fbTiltRangeMultiAxes = FileButtonCell.getInstance(table);
    fbTiltRangeMultiAxes.setFileFilter(volumeFileFilter);
  }

  private VolumeRow(final BaseManager manager, final String fnVolumeFile,
      final String fnModParticleFile, final String tiltRangeMultiAxesFile,
      final int index, final VolumeTable table, final JPanel panel,
      final GridBagLayout layout, final GridBagConstraints constraints,
      final VolumeFileFilter volumeFileFilter) {
    this.manager = manager;
    this.index = index;
    this.table = table;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    number.setText(String.valueOf(index + 1));
    btnHighlighter = HighlighterButton.getInstance(this, table);
    String rootDir = manager.getPropertyUserDir();
    fnVolume = FieldCell.getExpandableInstance(rootDir);
    setValue(fnVolume, fnVolumeFile);
    fbFnVolume = FileButtonCell.getInstance(table);
    fbFnVolume.setFileFilter(volumeFileFilter);
    fnModParticle = FieldCell.getExpandableInstance(rootDir);
    setValue(fnModParticle, fnModParticleFile);
    fbFnModParticle = FileButtonCell.getInstance(table);
    fbFnModParticle.setFileFilter(new ModelFileFilter());
    initMotlFile = FieldCell.getExpandableInstance(rootDir);
    fbInitMotlFile = FileButtonCell.getInstance(table);
    fbInitMotlFile.setFileFilter(new MotlFileFilter());
    tiltRangeMin = FieldCell.getEditableMatlabInstance();
    tiltRangeMax = FieldCell.getEditableMatlabInstance();
    tiltRangeMultiAxes = FieldCell.getExpandableInstance(rootDir);
    setValue(tiltRangeMultiAxes, tiltRangeMultiAxesFile);
    fbTiltRangeMultiAxes = FileButtonCell.getInstance(table);
    fbTiltRangeMultiAxes.setFileFilter(volumeFileFilter);
  }

  private void addActionTargets() {
    fbFnVolume.setActionTarget(fnVolume);
    fbFnModParticle.setActionTarget(fnModParticle);
    fbInitMotlFile.setActionTarget(initMotlFile);
    fbTiltRangeMultiAxes.setActionTarget(tiltRangeMultiAxes);
  }

  void setNames() {
    btnHighlighter.setHeaders(VolumeTable.LABEL, number,
        table.getVolumeNumberHeaderCell());
    setHeaders(fnVolume, fbFnVolume, table.getFnVolumeHeaderCell());
    setHeaders(fnModParticle, fbFnModParticle, table.getFnModParticleHeaderCell());
    setHeaders(initMotlFile, fbInitMotlFile, table.getInitMotlFileHeaderCell());
    setHeaders(tiltRangeMultiAxes, fbTiltRangeMultiAxes,
        table.getTiltRangeMultiAxesHeaderCell());
    fbInitMotlFile.setLabel(VolumeTable.INIT_MOTL_FILE_HEADER1 + " "
        + VolumeTable.INIT_MOTL_FILE_HEADER2);
    tiltRangeMin.setHeaders(VolumeTable.LABEL, number, table.getTiltRangeHeaderCell());
    tiltRangeMax.setHeaders(VolumeTable.LABEL, number, table.getTiltRangeHeaderCell());
  }

  /**
   * Return the text size, or an estimate or the minimum field text width of the three
   * changeable field (volume, model, and MOTL).
   */
  int getTextSize(final boolean isTiltRangeMultiAxes) {
    return Math.max(fnVolume.getValue().length(), 6)
        + Math.max(fnModParticle.getValue().length(), 5)
        + Math.max(initMotlFile.getValue().length(), 5)
        + (isTiltRangeMultiAxes ? Math.max(tiltRangeMultiAxes.getValue().length(), 5) : 0);
  }

  void setHeaders(final FieldCell fieldCell, final FileButtonCell fileButtonCell,
      final HeaderCell headerCell) {
    fieldCell.setHeaders(VolumeTable.LABEL, number, headerCell);
    fileButtonCell.setHeaders(VolumeTable.LABEL, number, headerCell);
  }

  void setHighlighterSelected(final boolean select) {
    btnHighlighter.setSelected(select);
  }

  public void highlight(final boolean highlight) {
    fnVolume.setHighlight(highlight);
    fnModParticle.setHighlight(highlight);
    initMotlFile.setHighlight(highlight);
    tiltRangeMultiAxes.setHighlight(highlight);
    tiltRangeMin.setHighlight(highlight);
    tiltRangeMax.setHighlight(highlight);
    tiltRangeMultiAxes.setHighlight(highlight);
  }

  void remove() {
    number.remove();
    btnHighlighter.remove();
    fnVolume.remove();
    fbFnVolume.remove();
    fnModParticle.remove();
    fbFnModParticle.remove();
    initMotlFile.remove();
    fbInitMotlFile.remove();
    tiltRangeMin.remove();
    tiltRangeMax.remove();
    tiltRangeMultiAxes.remove();
    fbTiltRangeMultiAxes.remove();
  }

  void display(int index, Viewport viewport) {
    if (!viewport.inViewport(index)) {
      return;
    }
    constraints.gridwidth = 1;
    number.add(panel, layout, constraints);
    btnHighlighter.add(panel, layout, constraints);
    fnVolume.add(panel, layout, constraints);
    fbFnVolume.add(panel, layout, constraints);
    fnModParticle.add(panel, layout, constraints);
    fbFnModParticle.add(panel, layout, constraints);
    initMotlFile.add(panel, layout, constraints);
    fbInitMotlFile.add(panel, layout, constraints);
    if (!table.isTiltRangeMultiAxes()) {
      tiltRangeMin.add(panel, layout, constraints);
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      tiltRangeMax.add(panel, layout, constraints);
    }
    else {
      tiltRangeMultiAxes.add(panel, layout, constraints);
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      fbTiltRangeMultiAxes.add(panel, layout, constraints);
    }
  }

  void expandFnVolume(final boolean expanded) {
    fnVolume.expand(expanded);
  }

  int getIndex() {
    return index;
  }

  void setIndex(int index) {
    this.index = index;
    number.setText(String.valueOf(index + 1));
  }

  void expandFnModParticle(final boolean expanded) {
    fnModParticle.expand(expanded);
  }

  void expandInitMotlFile(final boolean expanded) {
    initMotlFile.expand(expanded);
  }

  void expandTiltRangeMultiAxes(final boolean expanded) {
    tiltRangeMultiAxes.expand(expanded);
  }

  void getParameters(final PeetMetaData metaData) {
    metaData.setInitMotlFile(initMotlFile.getExpandedValue(), index);
    metaData.setTiltRangeMin(tiltRangeMin.getValue(), index);
    metaData.setTiltRangeMax(tiltRangeMax.getValue(), index);
    metaData.setTiltRangeMultiAxesFile(tiltRangeMultiAxes.getValue(), index);
  }

  /**
   * Make the copied paths relative to this dataset, preserving the location of the files
   * that the old dataset was using.  So if the files where in the original dataset
   * directory, the new path will point (with a relative path if possible) to the file in
   * the original dataset directory.  If a file path is absolute, don't change it.
   * @param rootOfCopiedFilePaths
   */
  void convertCopiedPaths(final String origDatasetDir) {
    String propertyUserDir = manager.getPropertyUserDir();
    if (!fnVolume.isEmpty()) {
      fnVolume.setValue(FilePath.getRerootedRelativePath(origDatasetDir, propertyUserDir,
          fnVolume.getExpandedValue()));
    }
    if (!fnModParticle.isEmpty()) {
      fnModParticle.setValue(FilePath.getRerootedRelativePath(origDatasetDir,
          propertyUserDir, fnModParticle.getExpandedValue()));
    }
    if (!initMotlFile.isEmpty()) {
      initMotlFile.setValue(FilePath.getRerootedRelativePath(origDatasetDir,
          propertyUserDir, initMotlFile.getExpandedValue()));
    }
    if (!tiltRangeMultiAxes.isEmpty()) {
      tiltRangeMultiAxes.setValue(FilePath.getRerootedRelativePath(origDatasetDir,
          propertyUserDir, tiltRangeMultiAxes.getExpandedValue()));
    }
  }

  /**
   * @return true if one or more paths are incorrect.
   */
  boolean isIncorrectPaths() {
    if (!fnVolume.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            fnVolume.getExpandedValue()).exists()) {
      return true;
    }
    if (!fnModParticle.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            fnModParticle.getExpandedValue()).exists()) {
      return true;
    }
    if (!initMotlFile.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            initMotlFile.getExpandedValue()).exists()) {
      return true;
    }
    if (!tiltRangeMultiAxes.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            tiltRangeMultiAxes.getExpandedValue()).exists()) {
      return true;
    }
    return false;
  }

  boolean fixIncorrectPaths(boolean choosePathEveryRow) {
    if (!fnVolume.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            fnVolume.getExpandedValue()).exists()) {
      if (!fixIncorrectPath(fnVolume, choosePathEveryRow, table.isFnVolumeExpanded(),
          fbFnVolume.getFileFilter())) {
        return false;
      }
    }
    if (!fnModParticle.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            fnModParticle.getExpandedValue()).exists()) {
      if (!fixIncorrectPath(fnModParticle, false, table.isFnModParticleExpanded(),
          fbFnModParticle.getFileFilter())) {
        return false;
      }
    }
    if (!initMotlFile.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            initMotlFile.getExpandedValue()).exists()) {
      if (!fixIncorrectPath(initMotlFile, false, table.isInitMotlFileExpanded(),
          fbInitMotlFile.getFileFilter())) {
        return false;
      }
    }
    if (!tiltRangeMultiAxes.isEmpty()
        && !FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
            tiltRangeMultiAxes.getExpandedValue()).exists()) {
      if (!fixIncorrectPath(tiltRangeMultiAxes, false, table.isInitMotlFileExpanded(),
          fbTiltRangeMultiAxes.getFileFilter())) {
        return false;
      }
    }
    return true;
  }

  /**
   * Fix an incorrect path.
   * @param fileTextField
   * @param choosePathEveryRow
   * @return false if the user cancels the file selector
   */
  private boolean fixIncorrectPath(FieldCell fieldCell, boolean choosePath,
      boolean expand, final FileFilter fileFilter) {
    File newFile = null;
    while (newFile == null || !newFile.exists()) {
      // Have the user choose the location of the file if they haven't chosen
      // before or they want to choose most of the files individuallly, otherwise
      // just use the current correctPath.
      if (table.isCorrectPathNull() || choosePath
          || (newFile != null && !newFile.exists())) {
        JFileChooser fileChooser = table.getFileChooserInstance();
        fileChooser.setSelectedFile(FilePath.buildAbsoluteFile(
            manager.getPropertyUserDir(), fieldCell.getExpandedValue()));
        fileChooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(fileFilter);
        int returnVal = fileChooser.showOpenDialog(table.getContainer());
        if (returnVal != JFileChooser.APPROVE_OPTION) {
          return false;
        }
        newFile = fileChooser.getSelectedFile();
        if (newFile != null && newFile.exists()) {
          table.setCorrectPath(newFile.getParent());
          setValue(fieldCell, newFile);
          fieldCell.expand(expand);
        }
      }
      else if (!table.isCorrectPathNull()) {
        newFile = new File(table.getCorrectPath(), fieldCell.getContractedValue());
        if (newFile.exists()) {
          setValue(fieldCell, newFile);
          fieldCell.expand(expand);
        }
      }
    }
    return true;
  }

  /**
   * Always set metaData before the functional data from the prm file, since
   * that may override the metaData.
   * @param metaData
   */
  void setParameters(final ConstPeetMetaData metaData) {
    if (metaData == null) {
      return;
    }
    setValue(initMotlFile, metaData.getInitMotlFile(index));
    setTiltRangeMin(metaData.getTiltRangeMin(index));
    setTiltRangeMax(metaData.getTiltRangeMax(index));
    setValue(tiltRangeMultiAxes, metaData.getTiltRangeMultiAxesFile(index));
  }

  void getParameters(final MatlabParam matlabParamFile, final boolean isTiltRangeMultiAxes) {
    MatlabParam.Volume volume = matlabParamFile.getVolume(index);
    volume.setFnVolume(fnVolume.getExpandedValue());
    volume.setFnModParticle(fnModParticle.getExpandedValue());
    volume.setInitMotl(initMotlFile.getExpandedValue());
    if (!isTiltRangeMultiAxes) {
      volume.setTiltRangeStart(tiltRangeMin.getValue());
      volume.setTiltRangeEnd(tiltRangeMax.getValue());
    }
    else {
      volume.setTiltRangeMultiAxes(tiltRangeMultiAxes.getValue());
    }
  }

  void setParameters(final MatlabParam matlabParam, final boolean useInitMotlFile,
      final boolean useTiltRange, final boolean isTiltRangeMultiAxes) {
    MatlabParam.Volume volume = matlabParam.getVolume(index);
    if (useInitMotlFile) {
      setValue(initMotlFile, volume.getInitMotlString());
    }
    if (useTiltRange) {
      if (!isTiltRangeMultiAxes) {
        setTiltRangeMin(volume.getTiltRangeStart());
        setTiltRangeMax(volume.getTiltRangeEnd());
      }
      else {
        setValue(tiltRangeMultiAxes, volume.getTiltRangeMultiAxesString());
      }
    }
  }

  void clearInitMotlFile() {
    initMotlFile.setValue();
  }

  void registerInitMotlFileColumn(Column column) {
    column.add(initMotlFile);
    column.add(fbInitMotlFile);
  }

  void registerTiltRangeColumn(final Column column) {
    column.add(tiltRangeMin);
    column.add(tiltRangeMax);
    column.add(tiltRangeMultiAxes);
    column.add(fbTiltRangeMultiAxes);
  }

  void imodVolume(Run3dmodMenuOptions menuOptions) {
    imodIndex = manager.imodOpen(ImodManager.TOMOGRAM_KEY, imodIndex,
        fnVolume.getExpandedValue(), fnModParticle.getExpandedValue(), menuOptions);
  }

  /**
   * Validate for running.  Returns error message.
   * @return null if valid
   */
  String validateRun(final boolean tiltRangeRequired, final boolean isTiltRangeMultiAxes) {
    if (fnModParticle.isEmpty()) {
      return VolumeTable.LABEL + ":  In row " + number.getText() + ", "
          + VolumeTable.FN_MOD_PARTICLE_HEADER1 + " must not be empty.";
    }
    if (tiltRangeRequired) {
      if (!isTiltRangeMultiAxes) {
        if (tiltRangeMin.isEmpty() || tiltRangeMax.isEmpty()) {
          return VolumeTable.LABEL + ":  In row " + number.getText() + ", "
              + VolumeTable.TILT_RANGE_HEADER1_LABEL + " is required.";
        }
      }
      else {
        if (tiltRangeMultiAxes.isEmpty()) {
          return VolumeTable.LABEL + ":  In row " + number.getText() + ", "
              + VolumeTable.getTiltRangeMultiAxesLabel() + " is required.";
        }
      }
    }
    return null;
  }

  /**
   * Sets the contracted and expanded values of the fieldCell while preserving the
   * filePath string.
   * @param fieldCell
   * @param filePath
   */
  private void setValue(final FieldCell fieldCell, final String filePath) {
    // Don't override existing values with null value.
    if (filePath == null || filePath.matches("\\s*")) {
      return;
    }
    // Preserve the text of the filePath.
    fieldCell.setValue(filePath);
  }

  /**
   * Sets the contracted and expanded values of the fieldCell with the file name and a
   * relative path from propertyUserDir to the file.
   * @param fieldCell
   * @param file
   */
  private void setValue(final FieldCell fieldCell, final File file) {
    // Don't override existing values with null value.
    if (file == null) {
      return;
    }
    fieldCell.setValue(file);
  }

  void setInitMotlFile(String initMotlFile) {
    setValue(this.initMotlFile, initMotlFile);
  }

  void setInitMotlFile(File initMotlFile) {
    setValue(this.initMotlFile, initMotlFile);
  }

  String getExpandedInitMotlFile() {
    if (initMotlFile.isEmpty()) {
      return null;
    }
    return initMotlFile.getExpandedValue();
  }

  File getFnVolumeFile() {
    if (fnVolume.isEmpty()) {
      return null;
    }
    return FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
        fnVolume.getExpandedValue());
  }

  File getFnModParticleFile() {
    if (fnModParticle.isEmpty()) {
      return null;
    }
    return FilePath.buildAbsoluteFile(manager.getPropertyUserDir(),
        fnModParticle.getExpandedValue());
  }

  void setFnModParticle(File input) {
    setValue(fnModParticle, input);
  }

  void setTiltRangeMin(final String input) {
    if (input == null) {
      return;
    }
    tiltRangeMin.setValue(input);
  }

  String getTiltRangeMin() {
    return tiltRangeMin.getValue();
  }

  String getTiltRangeMax() {
    return tiltRangeMax.getValue();
  }

  void setTiltRangeMax(final String input) {
    if (input == null) {
      return;
    }
    tiltRangeMax.setValue(input);
  }

  boolean isHighlighted() {
    return btnHighlighter.isHighlighted();
  }

  private void setTooltips() {
    fnVolume.setToolTipText("The filename of the tomogram in MRC format.");
    fbFnVolume.setToolTipText("Select a filename of the tomogram in MRC format.");
    fnModParticle.setToolTipText("The filename of the IMOD model specifying particle "
        + "positions in the tomogram.");
    fbFnModParticle
        .setToolTipText("Select a filename of the IMOD model specifying particle "
            + "positions in the tomogram.");
    initMotlFile.setToolTipText("The name of a .csv file containing an initial motive "
        + "list with orientations and shifts.");
    fbInitMotlFile
        .setToolTipText("Select a .csv file with initial orientations and shifts");
    String tooltip = " tilt angle (in degrees) used "
        + "during image acquisition for this tomogram.  Used only if missing "
        + "wedge compensation is enabled.";
    tiltRangeMin.setToolTipText("The minimum" + tooltip);
    tiltRangeMax.setToolTipText("The maximum" + tooltip);
    tiltRangeMultiAxes
        .setToolTipText("A binary mask file in MRC format with 0's and 1's indicating "
            + "missing and valid regions in Fourier space, respectively, for this "
            + "volume.  The mask should be cubical, with an even number of voxels, at "
            + "least as large as the largest dimension of the reference, along each "
            + "edge.");
  }
}
