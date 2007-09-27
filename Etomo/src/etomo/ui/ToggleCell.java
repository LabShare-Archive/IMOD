package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.event.ChangeListener;

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
 * <p> $Log$ </p>
 */
interface ToggleCell {
  public static final String rcsid = "$Id$";

  public String getLabel();

  public void setLabel( String label);

  public void setSelected( boolean selected);

  public void addActionListener(ActionListener actionListener);

  public void add(JPanel panel, GridBagLayout layout,
      GridBagConstraints constraints);

  public boolean isSelected();

  public int getHeight();

  public int getWidth();

  public void setWarning(boolean warning);
  
  public void addChangeListener(ChangeListener listener);
}
