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
 * <p> Revision 1.1  2007/02/20 20:37:13  sueh
 * <p> bug# 964 Represents each row of the volume table.
 * <p> </p>
 */

final class VolumeRow {
  public static final String rcsid = "$Id$";

  private final FieldCell tomogram = new FieldCell();
  private final int index;
  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final File tomogramFile;

  VolumeRow(File tomogramFile, int index, JPanel panel, GridBagLayout layout,
      GridBagConstraints constraints, boolean expanded) {
    this.index = index;
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    this.tomogramFile = tomogramFile;
    if (expanded) {
      tomogram.setValue(tomogramFile.getAbsolutePath());
    }
    else {
      tomogram.setValue(tomogramFile.getName());
    }
  }

  void display() {
    constraints.weightx = 0.1;
    constraints.weighty = 0.1;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    tomogram.add(panel, layout, constraints);
  }
}
