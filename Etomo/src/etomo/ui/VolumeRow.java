package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;

import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.process.ImodManager;
import etomo.storage.MatlabParamFile;
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

  private final int index;
  private final VolumeTable table;
  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final BaseManager manager;

  private final FieldCell fnModParticle = FieldCell.getExpandableInstance();
  private final FieldCell fnVolume = FieldCell.getExpandableInstance();
  private final FieldCell initMotlFile = FieldCell.getExpandableInstance();
  private final FieldCell tiltRangeStart = new FieldCell();
  private final FieldCell tiltRangeEnd = new FieldCell();
  private final FieldCell relativeOrientX = new FieldCell();
  private final FieldCell relativeOrientY = new FieldCell();
  private final FieldCell relativeOrientZ = new FieldCell();
  private final HighlighterButton btnHighlighter;
  private int imodIndex = -1;

  static VolumeRow getInstance(final BaseManager manager, final File fnVolume,
      final File fnModParticle, final int index, final VolumeTable table,
      final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    return new VolumeRow(manager, fnVolume, fnModParticle, index, table, panel,
        layout, constraints);
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
  }

  public void highlight(final boolean highlight) {
    fnVolume.setHighlight(highlight);
    fnModParticle.setHighlight(highlight);
    initMotlFile.setHighlight(highlight);
    tiltRangeStart.setHighlight(highlight);
    tiltRangeEnd.setHighlight(highlight);
    relativeOrientX.setHighlight(highlight);
    relativeOrientY.setHighlight(highlight);
    relativeOrientZ.setHighlight(highlight);
  }

  void display() {
    constraints.weightx = 0.1;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    btnHighlighter.add(panel, layout, constraints);
    constraints.gridwidth = 2;
    fnVolume.add(panel, layout, constraints);
    fnModParticle.add(panel, layout, constraints);
    initMotlFile.add(panel, layout, constraints);
    constraints.gridwidth = 1;
    tiltRangeStart.add(panel, layout, constraints);
    tiltRangeEnd.add(panel, layout, constraints);
    relativeOrientX.add(panel, layout, constraints);
    relativeOrientY.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    relativeOrientZ.add(panel, layout, constraints);
  }

  void expandFnVolume(final boolean expanded) {
    fnVolume.expand(expanded);
  }

  void expandFnModParticle(final boolean expanded) {
    fnModParticle.expand(expanded);
  }

  void expandInitMotlFile(final boolean expanded) {
    initMotlFile.expand(expanded);
  }

  void getParameters(final PeetMetaData metaData) {
    metaData.setInitMotlFile(initMotlFile.getExpandedValue(), index);
    metaData.setTiltRangeStart(tiltRangeStart.getValue(), index);
    metaData.setTiltRangeEnd(tiltRangeEnd.getValue(), index);
  }

  void setParameters(final ConstPeetMetaData metaData) {
    if (metaData == null) {
      return;
    }
    setExpandableValues(initMotlFile, metaData.getInitMotlFile(index));
    setTiltRangeStart(metaData.getTiltRangeStart(index));
    setTiltRangeEnd(metaData.getTiltRangeEnd(index));
  }

  void getParameters(final MatlabParamFile matlabParamFile) {
    MatlabParamFile.Volume volume = matlabParamFile.getVolume(index);
    volume.setFnVolume(fnVolume.getExpandedValue());
    volume.setFnModParticle(fnModParticle.getExpandedValue());
    volume.setInitMotl(initMotlFile.getExpandedValue());
    volume.setTiltRangeStart(tiltRangeStart.getValue());
    volume.setTiltRangeEnd(tiltRangeEnd.getValue());
    volume.setRelativeOrientX(relativeOrientX.getValue());
    volume.setRelativeOrientY(relativeOrientY.getValue());
    volume.setRelativeOrientZ(relativeOrientZ.getValue());
  }

  void setParameters(final MatlabParamFile matlabParamFile,
      boolean useInitMotlFile, boolean useTiltRange) {
    MatlabParamFile.Volume volume = matlabParamFile.getVolume(index);
    if (useInitMotlFile) {
      setExpandableValues(initMotlFile, volume.getInitMotl());
    }
    if (useTiltRange) {
      setTiltRangeStart(volume.getTiltRangeStart());
      setTiltRangeEnd(volume.getTiltRangeEnd());
    }
    if (volume.isRelativeOrientSet()) {
      relativeOrientX.setValue(volume.getRelativeOrientX());
      relativeOrientY.setValue(volume.getRelativeOrientY());
      relativeOrientZ.setValue(volume.getRelativeOrientZ());
    }
    else {
      relativeOrientX.setValue();
      relativeOrientY.setValue();
      relativeOrientZ.setValue();
    }
  }

  void imodVolume(Run3dmodMenuOptions menuOptions) {
    imodIndex = manager.imodOpen(ImodManager.TOMOGRAM_KEY, imodIndex, fnVolume
        .getExpandedValue(), fnModParticle.getExpandedValue(), menuOptions);
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

  void setTiltRangeStart(final String tiltRangeStart) {
    if (tiltRangeStart == null) {
      return;
    }
    this.tiltRangeStart.setValue(tiltRangeStart);
  }

  void setTiltRangeEnd(String tiltRangeEnd) {
    if (tiltRangeEnd == null) {
      return;
    }
    this.tiltRangeEnd.setValue(tiltRangeEnd);
  }

  boolean isHighlighted() {
    return btnHighlighter.isHighlighted();
  }
}
