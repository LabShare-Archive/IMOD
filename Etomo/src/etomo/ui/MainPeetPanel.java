package etomo.ui;

import java.io.File;

import etomo.BaseManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;

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
public final class MainPeetPanel extends MainPanel {
  public static  final String  rcsid =  "$Id$";
  
  private PeetProcessPanel axisPanelA = null;
  
  public MainPeetPanel(BaseManager manager) {
    super(manager);
  }
  
  public final void setStatusBarText(File paramFile, BaseMetaData metaData) {
    super.setStatusBarText(paramFile, metaData);
  }
  
  protected void addAxisPanelA() { 
    scrollA.add(axisPanelA.getContainer());
  }
  
  protected void addAxisPanelB() {
  }
  
  protected boolean isAxisPanelANull() {
    return axisPanelA == null;
  }
  
  protected boolean isAxisPanelBNull() {
    return true;
  }
  
  protected void createAxisPanelA(AxisID axisID) {
    axisPanelA = new PeetProcessPanel(manager);
  }
  
  protected void createAxisPanelB() {
  }
  
  protected AxisProcessPanel getAxisPanelA() {
    return axisPanelA;
  }
  
  protected AxisProcessPanel getAxisPanelB() {
    return null;
  }
  
  protected DataFileFilter getDataFileFilter() {
    return null;
  }
  
  protected boolean hideAxisPanelA() {
    return axisPanelA.hide();
  }
  
  protected boolean hideAxisPanelB() {
    return true;
  }
  
  protected AxisProcessPanel mapBaseAxis(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return null;
    }
    return axisPanelA;
  }
  
  protected void resetAxisPanels() {
    axisPanelA = null;
  }
  
  public void saveDisplayState() {
  }
  
  public void setState(ProcessState processState, AxisID axisID,
      AbstractParallelDialog parallelDialog) {
  }
  
  protected void showAxisPanelA() {
    axisPanelA.show();
  }
  
  protected void showAxisPanelB() {
  }
}
