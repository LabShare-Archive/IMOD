package etomo.ui;

import java.io.File;

import javax.swing.JPanel;

import etomo.EtomoDirector;
import etomo.JoinManager;
import etomo.storage.DataFileFilter;
import etomo.storage.JoinFileFilter;
import etomo.type.AxisID;
import etomo.type.JoinMetaData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.2  2004/11/19 23:58:44  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.8  2004/10/29 01:23:01  sueh
* <p> bug# 520 Fixing updateDataParameters.  Only use "No data set loaded"
* <p> is joinMetaData is not valid (has not rootName set).
* <p>
* <p> Revision 1.1.2.7  2004/10/15 00:49:31  sueh
* <p> bug# 520 Added implementation of getDataFileFilter.
* <p>
* <p> Revision 1.1.2.6  2004/10/14 02:29:24  sueh
* <p> bug# 520 Removed Axis Type from the join status bar.
* <p>
* <p> Revision 1.1.2.5  2004/10/11 02:15:37  sueh
* <p> bug# 520 Moved responsibility for axisPanelA and axisPanelB member
* <p> variables to the child classes.  Used abstract functions to use these
* <p> variables in the base class.  This is more reliable and doesn't require
* <p> casting.
* <p>
* <p> Revision 1.1.2.4  2004/10/08 16:33:43  sueh
* <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
* <p> member variables non-static.
* <p>
* <p> Revision 1.1.2.3  2004/09/29 19:35:59  sueh
* <p> bug# 520 Created private variables that are cast from base-class
* <p> member variables during construction.
* <p>
* <p> Revision 1.1.2.2  2004/09/15 22:42:21  sueh
* <p> bug# 520 added castManger(), overrode createAxisPanelA and B
* <p>
* <p> Revision 1.1.2.1  2004/09/08 20:09:02  sueh
* <p> bug# 520 MainPanel for Join
* <p> </p>
*/

public class MainJoinPanel extends MainPanel {
  public static  final String  rcsid =  "$Id$";
  
  private JoinProcessPanel axisPanelA;
  private JoinProcessPanel axisPanelB;
  /**
   * @param joinManager
   */
  public MainJoinPanel(JoinManager joinManager) {
    super(joinManager);
  }
  
  public void saveDisplayState() {
    
  }
  
  protected DataFileFilter getDataFileFilter() {
    return new JoinFileFilter();
  }
  
  protected void createAxisPanelA(AxisID axisID) {
    axisPanelA = new JoinProcessPanel((JoinManager) manager, axisID);
  }

  protected void createAxisPanelB() {
  }
  
  /**
   * Open the setup panel
   */
  public void openPanel(JPanel panel) {
    scrollA.add(panel);
    revalidate();
    EtomoDirector.getInstance().getMainFrame().pack();
  }

  /**
   * Set the status bar with the file name of the data parameter file
   */
  public void updateDataParameters(File paramFile, JoinMetaData joinMetaData) {
    StringBuffer buffer = new StringBuffer();
    if (joinMetaData == null || !joinMetaData.isValid()) {
      buffer.append("No data set loaded");
    }
    else {
      if (paramFile == null) {
        buffer.append("Data file: NOT SAVED");
      }
      else {
        buffer.append("Data file: " + paramFile.getAbsolutePath());
      }
    }
    statusBar.setText(buffer.toString());
  }
  
  
  protected void resetAxisPanels() {
    axisPanelA = null;
    axisPanelB = null;
  }
  
  protected void addAxisPanelA() {
    scrollA.add(axisPanelA.getContainer());
  }
  
  protected void addAxisPanelB() {
    scrollB.add(axisPanelB.getContainer());
  }
  
  protected boolean AxisPanelAIsNull() {
    return axisPanelA == null;
  }
  
  protected boolean AxisPanelBIsNull() {
    return axisPanelB == null;
  }
  
  protected boolean hideAxisPanelA() {
    return axisPanelA.hide();
  }
  
  protected boolean hideAxisPanelB() {
    return axisPanelB.hide();
  }
  
  protected void showAxisPanelA() {
    axisPanelA.show();
  }
  
  protected void showAxisPanelB() {
    axisPanelB.show();
  }
  
  protected boolean isAxisPanelAFitScreenError() {
    return isFitScreenError(axisPanelA);
  }
  
  protected AxisProcessPanel mapBaseAxis(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return axisPanelB;
    }
    return axisPanelA;
  }
}
