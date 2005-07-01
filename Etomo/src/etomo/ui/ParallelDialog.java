package etomo.ui;

import javax.swing.JDialog;
import javax.swing.JFrame;

import etomo.type.AxisID;
import etomo.type.DialogType;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
final public class ParallelDialog extends JDialog {
  public static  final String  rcsid =  "$Id$";
  
  private ParallelPanel parallelPanel = null;
  private AxisID axisID = null;
  
  public ParallelDialog(JFrame frame, AxisID axisID) {
    super(frame);
    this.axisID = axisID;
    parallelPanel = new ParallelPanel(axisID);
    getRootPane().getContentPane().add(parallelPanel.getRootPanel());
    setLocation(400, 400);
  }
  
  public void setDialogType(DialogType dialogType) {
    if (dialogType == DialogType.TOMOGRAM_GENERATION) {
      setTitle("Parallel Tilt Processing");
    }
    else if (dialogType == DialogType.TOMOGRAM_COMBINATION) {
      setTitle("Parallel Volcombine Processing");
    }
    parallelPanel.setDialogType(dialogType);
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/07/01 21:20:53  sueh
* <p> bug# 619 JDialog to demo parallel processing
* <p> </p>
*/
