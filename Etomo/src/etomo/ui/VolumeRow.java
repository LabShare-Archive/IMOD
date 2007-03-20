package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;

import javax.swing.JPanel;

import etomo.type.ConstEtomoNumber;

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

  private final FieldCell fnModParticle = FieldCell.getExpandableInstance();
  private final FieldCell fnVolume = FieldCell.getExpandableInstance();
  private final FieldCell initMotlFile = FieldCell.getExpandableInstance();
  private final FieldCell tiltRangeStart = new FieldCell();
  private final FieldCell tiltRangeEnd = new FieldCell();
  private final FieldCell relativeOrientX = new FieldCell();
  private final FieldCell relativeOrientY = new FieldCell();
  private final FieldCell relativeOrientZ = new FieldCell();
  private final HighlighterButton btnHighlighter;

  static VolumeRow getInstance(final String contractedFnVolume,
      final String expandedFnVolume, final String contractedFnModParticle,
      final String expandedFnModParticle, final int index,
      final VolumeTable table, final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    return new VolumeRow(contractedFnVolume, expandedFnVolume,
        contractedFnModParticle, expandedFnModParticle, index, table, panel,
        layout, constraints);
  }

  private VolumeRow(final String contractedFnVolume,
      final String expandedFnVolume, final String contractedFnModParticle,
      final String expandedFnModParticle, final int index,
      final VolumeTable table, final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    this.index = index;
    this.table = table;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    this.fnVolume.setExpandableValues(contractedFnVolume, expandedFnVolume);
    this.fnModParticle.setExpandableValues(contractedFnModParticle,
        expandedFnModParticle);
    btnHighlighter = new HighlighterButton(this, table);
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
    if (table.usingInitMotlFile()) {
      initMotlFile.add(panel, layout, constraints);
    }
    if (table.usingTiltRange()) {
      constraints.gridwidth = 1;
      tiltRangeStart.add(panel, layout, constraints);
      tiltRangeEnd.add(panel, layout, constraints);
    }
    constraints.gridwidth = 1;
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

  String getInitMotlFile() {
    return initMotlFile.getExpandedValue();
  }
  
  String getTiltRangeStart() {
    return tiltRangeStart.getValue();
  }
  
  String getTiltRangeEnd() {
    return tiltRangeEnd.getValue();
  }
  
  int setInitMotlFile(File initMotlFile) {
    this.initMotlFile.setExpandableValues(initMotlFile.getName(), initMotlFile
        .getAbsolutePath());
    return index;
  }
  
  int setInitMotlFile(String initMotlFile) {
    return setInitMotlFile(new File(initMotlFile));
  }

  void setTiltRangeStart(ConstEtomoNumber tiltRangeStart) {
    this.tiltRangeStart.setValue(tiltRangeStart);
  }
  
  void setTiltRangeStart(String tiltRangeStart) {
    this.tiltRangeStart.setValue(tiltRangeStart);
  }

  void setTiltRangeEnd(ConstEtomoNumber tiltRangeEnd) {
    this.tiltRangeEnd.setValue(tiltRangeEnd);
  }
  
  void setTiltRangeEnd(String tiltRangeEnd) {
    this.tiltRangeEnd.setValue(tiltRangeEnd);
  }

  void setRelativeOrientX(String relativeOrientX) {
    this.relativeOrientX.setValue(relativeOrientX);
  }

  void setRelativeOrientY(String relativeOrientY) {
    this.relativeOrientY.setValue(relativeOrientY);
  }

  void setRelativeOrientZ(String relativeOrientZ) {
    this.relativeOrientZ.setValue(relativeOrientZ);
  }

  boolean isHighlighted() {
    return btnHighlighter.isHighlighted();
  }

  void remove() {
    btnHighlighter.remove();
    fnVolume.remove();
    fnModParticle.remove();
    initMotlFile.remove();
    tiltRangeStart.remove();
    tiltRangeEnd.remove();
    relativeOrientX.remove();
    relativeOrientY.remove();
    relativeOrientZ.remove();
  }
}
