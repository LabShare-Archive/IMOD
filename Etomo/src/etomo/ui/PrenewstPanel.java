/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2004/02/05 04:36:44  rickg
 * <p> Bug# 390 Initial revision
 * <p> </p>
 */
package etomo.ui;

import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import etomo.type.AxisID;

public class PrenewstPanel {
  public static final String rcsid = "$Id$";

  private JPanel panelPrenewst = new JPanel();

  private LabeledSpinner spinBinning;

  AxisID axisID;

  public PrenewstPanel(AxisID id) {
    axisID = id;
    //  Construct the binning spinner
    SpinnerModel integerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinBinning = new LabeledSpinner("Pre-aligned image stack binning ",
      integerModel);
    panelPrenewst
    .setBorder(new EtchedBorder("Newstack Parameters").getBorder());
    panelPrenewst.add(spinBinning.getContainer());
    
    spinBinning.setEnabled(false);
  }

  JPanel getPanel() {
    return panelPrenewst;
  }
}
