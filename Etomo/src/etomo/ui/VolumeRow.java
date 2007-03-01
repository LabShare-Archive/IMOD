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

  private final FieldCell model = new FieldCell();
  private final FieldCell tomogram = new FieldCell();
  private final FieldCell motl = new FieldCell();
  private final HighlighterButton btnHighlighter;
  private final SpinnerCell motlSpinner;

  private boolean useMotlSpinner = true;

  public void highlight(final boolean highlight) {
    tomogram.setHighlight(highlight);
    model.setHighlight(highlight);
  }

  static VolumeRow getInstance(final String contractedTomogram,
      final String expandedTomogram, final String contractedModel,
      final String expandedModel, final int index, final VolumeTable table,
      final JPanel panel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    return new VolumeRow(contractedTomogram, expandedTomogram, contractedModel,
        expandedModel, index, table, panel, layout, constraints);
  }

  void display() {
    constraints.weightx = 0.1;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    btnHighlighter.add(panel, layout, constraints);
    constraints.gridwidth = 2;
    tomogram.add(panel, layout, constraints);
    model.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    if (useMotlSpinner) {
      motlSpinner.add(panel, layout, constraints);
    }
    else {
      motl.add(panel, layout, constraints);
    }
  }
  
  void displayMotl() {
    constraints.weightx = 0.1;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    if (useMotlSpinner) {
      motlSpinner.add(panel, layout, constraints);
    }
    else {
      motl.add(panel, layout, constraints);
    }
  }

  void expandTomogram(final boolean expanded) {
    tomogram.expand(expanded);
  }

  void expandModel(final boolean expanded) {
    model.expand(expanded);
  }
  
  void expandMotl(final boolean expanded) {
    motl.expand(expanded);
  }

  int setMotl(File motl) {
    this.motl.setExpandableValues(motl.getName(), motl.getAbsolutePath());
    useMotlSpinner = false;
    return index;
  }

  boolean isHighlighted() {
    return btnHighlighter.isHighlighted();
  }
  
  boolean usingMotlSpinner() {
    return useMotlSpinner;
  }
  
  void removeMotl() {
    motlSpinner.remove();
    motl.remove();
  }

  void remove() {
    btnHighlighter.remove();
    tomogram.remove();
    model.remove();
    motlSpinner.remove();
    motl.remove();
  }
  
  private VolumeRow(final String contractedTomogram, final String expandedTomogram,
      final String contractedModel, final String expandedModel,
      final int index, final VolumeTable table, final JPanel panel,
      final GridBagLayout layout, final GridBagConstraints constraints) {
    this.index = index;
    this.table = table;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    this.tomogram.setExpandableValues(contractedTomogram, expandedTomogram);
    this.model.setExpandableValues(contractedModel, expandedModel);
    btnHighlighter = new HighlighterButton(this, table);
    motlSpinner = SpinnerCell.getIntInstance(0, 2);
    //disable fields
    tomogram.setEnabled(false);
    model.setEnabled(false);
  }
}
