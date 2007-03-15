package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;

import javax.swing.JPanel;

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

  private final FieldCell fnModParticle = new FieldCell();
  private final FieldCell fnVolume = new FieldCell();
  private final FieldCell initMotl = new FieldCell();
  private final HighlighterButton btnHighlighter;
  private final SpinnerCell initMotlSpinner;

  private boolean useInitMotlSpinner = true;

  static VolumeRow getInstance(final String contractedFnVolume,
      final String expandedFnVolume, final String contractedFnModParticle,
      final String expandedFnModParticle, final int index, final VolumeTable table,
      final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    return new VolumeRow(contractedFnVolume, expandedFnVolume, contractedFnModParticle,
        expandedFnModParticle, index, table, panel, layout, constraints);
  }
  
  private VolumeRow(final String contractedFnVolume, final String expandedFnVolume,
      final String contractedFnModParticle, final String expandedFnModParticle,
      final int index, final VolumeTable table, final JPanel panel,
      final GridBagLayout layout, final GridBagConstraints constraints) {
    this.index = index;
    this.table = table;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    this.fnVolume.setExpandableValues(contractedFnVolume, expandedFnVolume);
    this.fnModParticle.setExpandableValues(contractedFnModParticle, expandedFnModParticle);
    btnHighlighter = new HighlighterButton(this, table);
    initMotlSpinner = SpinnerCell.getIntInstance(0, 2);
    //disable fields
    fnVolume.setEnabled(false);
    fnModParticle.setEnabled(false);
  }
  
  public void highlight(final boolean highlight) {
    fnVolume.setHighlight(highlight);
    fnModParticle.setHighlight(highlight);
  }

  void display() {
    constraints.weightx = 0.1;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    btnHighlighter.add(panel, layout, constraints);
    constraints.gridwidth = 2;
    fnVolume.add(panel, layout, constraints);
    fnModParticle.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    if (useInitMotlSpinner) {
      initMotlSpinner.add(panel, layout, constraints);
    }
    else {
      initMotl.add(panel, layout, constraints);
    }
  }
  
  void displayInitMotl() {
    constraints.weightx = 0.1;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    if (useInitMotlSpinner) {
      initMotlSpinner.add(panel, layout, constraints);
    }
    else {
      initMotl.add(panel, layout, constraints);
    }
  }

  void expandFnVolume(final boolean expanded) {
    fnVolume.expand(expanded);
  }

  void expandFnModParticle(final boolean expanded) {
    fnModParticle.expand(expanded);
  }
  
  void expandInitMotl(final boolean expanded) {
    initMotl.expand(expanded);
  }

  int setInitMotl(File initMotl) {
    this.initMotl.setExpandableValues(initMotl.getName(), initMotl.getAbsolutePath());
    useInitMotlSpinner = false;
    return index;
  }

  boolean isHighlighted() {
    return btnHighlighter.isHighlighted();
  }
  
  boolean usingInitMotlSpinner() {
    return useInitMotlSpinner;
  }
  
  void removeInitMotl() {
    initMotlSpinner.remove();
    initMotl.remove();
  }

  void remove() {
    btnHighlighter.remove();
    fnVolume.remove();
    fnModParticle.remove();
    initMotlSpinner.remove();
    initMotl.remove();
  }
}
