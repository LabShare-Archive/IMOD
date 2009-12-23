package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.process.ImodManager;
import etomo.storage.MatlabParam;
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
  private final FieldCell fnModParticle = FieldCell.getExpandableInstance();
  private final FieldCell fnVolume = FieldCell.getExpandableInstance();
  private final FieldCell initMotlFile = FieldCell.getExpandableInstance();
  private final FieldCell tiltRangeMin = FieldCell.getEditableMatlabInstance();
  private final FieldCell tiltRangeMax = FieldCell.getEditableMatlabInstance();
  private final FieldCell relativeOrientX = FieldCell
      .getEditableMatlabInstance();
  private final FieldCell relativeOrientY = FieldCell
      .getEditableMatlabInstance();
  private final FieldCell relativeOrientZ = FieldCell
      .getEditableMatlabInstance();
  private final HighlighterButton btnHighlighter;

  private final VolumeTable table;
  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final BaseManager manager;

  private int imodIndex = -1;
  private int index;

  static VolumeRow getInstance(final BaseManager manager, final File fnVolume,
      final File fnModParticle, final int index, final VolumeTable table,
      final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    VolumeRow instance = new VolumeRow(manager, fnVolume, fnModParticle, index,
        table, panel, layout, constraints);
    instance.setTooltips();
    return instance;
  }

  private VolumeRow(final BaseManager manager, final File fnVolumeFile,
      final File fnModParticleFile, final int index, final VolumeTable table,
      final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    this.manager = manager;
    this.index = index;
    this.table = table;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    setExpandableValues(fnVolume, fnVolumeFile);
    setExpandableValues(fnModParticle, fnModParticleFile);
    btnHighlighter = HighlighterButton.getInstance(this, table);
    number.setText(String.valueOf(index + 1));
  }

  void setNames() {
    btnHighlighter.setHeaders(VolumeTable.LABEL, number, table
        .getVolumeNumberHeaderCell());
    fnVolume.setHeaders(VolumeTable.LABEL, number, table
        .getFnVolumeHeaderCell());
    fnModParticle.setHeaders(VolumeTable.LABEL, number, table
        .getFnModParticleHeaderCell());
    tiltRangeMin.setHeaders(VolumeTable.LABEL, number, table
        .getTiltRangeHeaderCell());
    tiltRangeMax.setHeaders(VolumeTable.LABEL, number, table
        .getTiltRangeHeaderCell());
    relativeOrientX.setHeaders(VolumeTable.LABEL, number, table
        .getRelativeOrientHeaderCell());
    relativeOrientY.setHeaders(VolumeTable.LABEL, number, table
        .getRelativeOrientHeaderCell());
    relativeOrientZ.setHeaders(VolumeTable.LABEL, number, table
        .getRelativeOrientHeaderCell());
  }

  public void highlight(final boolean highlight) {
    fnVolume.setHighlight(highlight);
    fnModParticle.setHighlight(highlight);
    initMotlFile.setHighlight(highlight);
    tiltRangeMin.setHighlight(highlight);
    tiltRangeMax.setHighlight(highlight);
    relativeOrientX.setHighlight(highlight);
    relativeOrientY.setHighlight(highlight);
    relativeOrientZ.setHighlight(highlight);
  }

  void remove() {
    number.remove();
    btnHighlighter.remove();
    fnVolume.remove();
    fnModParticle.remove();
    initMotlFile.remove();
    tiltRangeMin.remove();
    tiltRangeMax.remove();
    relativeOrientX.remove();
    relativeOrientY.remove();
    relativeOrientZ.remove();
  }

  void display(int index, Viewport viewport) {
    if (!viewport.inViewport(index)) {
      return;
    }
    constraints.weightx = 0.0;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    number.add(panel, layout, constraints);
    btnHighlighter.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 2;
    fnVolume.add(panel, layout, constraints);
    fnModParticle.add(panel, layout, constraints);
    initMotlFile.add(panel, layout, constraints);
    constraints.gridwidth = 1;
    tiltRangeMin.add(panel, layout, constraints);
    tiltRangeMax.add(panel, layout, constraints);
    relativeOrientX.add(panel, layout, constraints);
    relativeOrientY.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    relativeOrientZ.add(panel, layout, constraints);
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

  void getParameters(final PeetMetaData metaData) {
    metaData.setInitMotlFile(initMotlFile.getExpandedValue(), index);
    metaData.setTiltRangeMin(tiltRangeMin.getValue(), index);
    metaData.setTiltRangeMax(tiltRangeMax.getValue(), index);
  }

  /**
   * @return true if one or more paths are incorrect.
   */
  boolean isIncorrectPaths() {
    if (!fnVolume.isEmpty() && !new File(fnVolume.getExpandedValue()).exists()) {
      return true;
    }
    if (!fnModParticle.isEmpty()
        && !new File(fnModParticle.getExpandedValue()).exists()) {
      return true;
    }
    if (!initMotlFile.isEmpty()
        && !new File(initMotlFile.getExpandedValue()).exists()) {
      return true;
    }
    return false;
  }

  boolean fixIncorrectPaths(boolean choosePathEveryRow) {
    if (!fnVolume.isEmpty() && !new File(fnVolume.getExpandedValue()).exists()) {
      if (!fixIncorrectPath(fnVolume, choosePathEveryRow, table
          .isFnVolumeExpanded())) {
        return false;
      }
    }
    if (!fnModParticle.isEmpty()
        && !new File(fnModParticle.getExpandedValue()).exists()) {
      if (!fixIncorrectPath(fnModParticle, false, table
          .isFnModParticleExpanded())) {
        return false;
      }
    }
    if (!initMotlFile.isEmpty()
        && !new File(initMotlFile.getExpandedValue()).exists()) {
      if (!fixIncorrectPath(initMotlFile, false, table.isInitMotlFileExpanded())) {
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
      boolean expand) {
    File newFile = null;
    while (newFile == null || !newFile.exists()) {
      //Have the user choose the location of the file if they haven't chosen
      //before or they want to choose most of the files individuallly, otherwise
      //just use the current correctPath.
      if (table.isCorrectPathNull() || choosePath
          || (newFile != null && !newFile.exists())) {
        JFileChooser fileChooser = table.getFileChooserInstance();
        fileChooser.setSelectedFile(new File(fieldCell.getExpandedValue()));
        fileChooser.setPreferredSize(UIParameters.INSTANCE
            .getFileChooserDimension());
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        int returnVal = fileChooser.showOpenDialog(table.getContainer());
        if (returnVal != JFileChooser.APPROVE_OPTION) {
          return false;
        }
        newFile = fileChooser.getSelectedFile();
        if (newFile != null && newFile.exists()) {
          table.setCorrectPath(newFile.getParent());
          setExpandableValues(fieldCell, newFile);
          fieldCell.expand(expand);
        }
      }
      else if (!table.isCorrectPathNull()) {
        newFile = new File(table.getCorrectPath(), fieldCell
            .getContractedValue());
        if (newFile.exists()) {
          setExpandableValues(fieldCell, newFile);
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
    setExpandableValues(initMotlFile, metaData.getInitMotlFile(index));
    setTiltRangeMin(metaData.getTiltRangeMin(index));
    setTiltRangeMax(metaData.getTiltRangeMax(index));
  }

  void getParameters(final MatlabParam matlabParamFile) {
    MatlabParam.Volume volume = matlabParamFile.getVolume(index);
    volume.setFnVolume(fnVolume.getExpandedValue());
    volume.setFnModParticle(fnModParticle.getExpandedValue());
    volume.setInitMotl(initMotlFile.getExpandedValue());
    volume.setTiltRangeStart(tiltRangeMin.getValue());
    volume.setTiltRangeEnd(tiltRangeMax.getValue());
    volume.setRelativeOrientX(relativeOrientX.getValue());
    volume.setRelativeOrientY(relativeOrientY.getValue());
    volume.setRelativeOrientZ(relativeOrientZ.getValue());
  }

  void setParameters(final MatlabParam matlabParam, boolean useInitMotlFile,
      boolean useTiltRange) {
    MatlabParam.Volume volume = matlabParam.getVolume(index);
    if (useInitMotlFile) {
      setExpandableValues(initMotlFile, volume.getInitMotlString());
    }
    if (useTiltRange) {
      setTiltRangeMin(volume.getTiltRangeStart());
      setTiltRangeMax(volume.getTiltRangeEnd());
    }
    relativeOrientX.setValue(volume.getRelativeOrientX());
    relativeOrientY.setValue(volume.getRelativeOrientY());
    relativeOrientZ.setValue(volume.getRelativeOrientZ());
  }

  void clearInitMotlFile() {
    initMotlFile.clearExpandableValues();
  }

  void registerInitMotlFileColumn(Column column) {
    column.add(initMotlFile);
  }

  void registerTiltRangeColumn(Column column) {
    column.add(tiltRangeMin);
    column.add(tiltRangeMax);
  }

  void imodVolume(Run3dmodMenuOptions menuOptions) {
    imodIndex = manager.imodOpen(ImodManager.TOMOGRAM_KEY, imodIndex, fnVolume
        .getExpandedValue(), fnModParticle.getExpandedValue(), menuOptions);
  }

  /**
   * Validate for running.  Returns error message.
   * @return null if valid
   */
  String validateRun(boolean tiltRangeRequired) {
    if (fnModParticle.isEmpty()) {
      return VolumeTable.LABEL + ":  In row " + number.getText() + ", "
          + VolumeTable.FN_MOD_PARTICLE_HEADER1 + " must not be empty.";
    }
    if (tiltRangeRequired && (tiltRangeMin.isEmpty() || tiltRangeMax.isEmpty())) {
      return VolumeTable.LABEL + ":  In row " + number.getText() + ", "
          + VolumeTable.TILT_RANGE_HEADER1_LABEL + " is required when either "
          + MissingWedgeCompensationPanel.TILT_RANGE_LABEL + " or "
          + MissingWedgeCompensationPanel.FLG_WEDGE_WEIGHT_LABEL
          + " is checked.";
    }
    return null;
  }

  private void setExpandableValues(final FieldCell fieldCell,
      final String fileName) {
    //Don't override existing values with null value.
    if (fileName == null || fileName.matches("\\s*")) {
      return;
    }
    setExpandableValues(fieldCell, new File(fileName));
  }

  private void setExpandableValues(final FieldCell fieldCell, final File file) {
    //Don't override existing values with null value.
    if (file == null) {
      return;
    }
    fieldCell.setExpandableValues(file.getName(), file.getAbsolutePath());
  }

  void setInitMotlFile(File initMotlFile) {
    setExpandableValues(this.initMotlFile, initMotlFile);
  }

  File getInitMotlFile() {
    if (initMotlFile.isEmpty()) {
      return null;
    }
    return new File(initMotlFile.getExpandedValue());
  }

  File getFnVolumeFile() {
    if (fnVolume.isEmpty()) {
      return null;
    }
    return new File(fnVolume.getExpandedValue());
  }

  File getFnModParticleFile() {
    if (fnModParticle.isEmpty()) {
      return null;
    }
    return new File(fnModParticle.getExpandedValue());
  }

  void setFnModParticle(File input) {
    setExpandableValues(fnModParticle, input);
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

  void setRelativeOrientX(final String input) {
    relativeOrientX.setValue(input);
  }

  void setRelativeOrientY(final String input) {
    relativeOrientY.setValue(input);
  }

  void setRelativeOrientZ(final String input) {
    relativeOrientZ.setValue(input);
  }

  boolean isHighlighted() {
    return btnHighlighter.isHighlighted();
  }

  private void setTooltips() {
    fnVolume.setToolTipText("The filename of the tomogram in MRC format.");
    fnModParticle
        .setToolTipText("The filename of the IMOD model specifying particle "
            + "positions in the tomogram.");
    initMotlFile
        .setToolTipText("The name of a .csv file containing an initial motive "
            + "list with orientations and shifts.");
    String tooltip = "The minimum and maximum tilt angle (in degrees) used "
        + "during image acquisition for this tomogram.  Used only if missing "
        + "wedge compensation is enabled.";
    tiltRangeMin.setToolTipText(tooltip);
    tiltRangeMax.setToolTipText(tooltip);
    relativeOrientX
        .setToolTipText("The Slicer X axis rotation required to rotate this "
            + "tomogram to to a common orientation with the other tomograms.");
    relativeOrientY
    .setToolTipText("The Slicer Y axis rotation required to rotate this "
        + "tomogram to to a common orientation with the other tomograms.");
    relativeOrientZ
    .setToolTipText("The Slicer Z axis rotation required to rotate this "
        + "tomogram to to a common orientation with the other tomograms.");
  }
}
