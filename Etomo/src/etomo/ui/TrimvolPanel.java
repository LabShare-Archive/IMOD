package etomo.ui;

import javax.swing.JPanel;
import javax.swing.JCheckBox;
/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */

public class TrimvolPanel {
  public static final String rcsid = "$Id$";
  
  private JPanel pnlRange = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X min:");
  private LabeledTextField ltfXMax = new LabeledTextField("X max:");
  private LabeledTextField ltfYMin = new LabeledTextField("Y min:");
  private LabeledTextField ltfYMax = new LabeledTextField("Y max:");
  private LabeledTextField ltfZMin = new LabeledTextField("Z min:");
  private LabeledTextField ltfZMax = new LabeledTextField("Z max:");
  
  private JCheckBox cbSwapXYZ = new JCheckBox();
  
  public TrimvolPanel() {
    
  }
}
