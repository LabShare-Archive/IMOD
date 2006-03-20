package etomo.ui;

import java.io.File;

import etomo.ParallelManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
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
public final class MainParallelPanel extends MainPanel {
  public static  final String  rcsid =  "$Id$";
  
  private final ParallelManager manager;
  
  private ParallelProcessPanel axisPanelA = null;
  
  public MainParallelPanel(ParallelManager manager) {
    super(manager);
    this.manager = manager;
  }
  
  protected void addAxisPanelA() { 
    scrollA.add(axisPanelA.getContainer());
  }
  
  protected void addAxisPanelB() {
  }
  
  protected boolean AxisPanelAIsNull() {
    return axisPanelA == null;
  }
  
  protected boolean AxisPanelBIsNull() {
    return true;
  }
  
  protected void createAxisPanelA(AxisID axisID) {
    axisPanelA = new ParallelProcessPanel(manager);
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
  
  public final void setStatusBarText(File paramFile, BaseMetaData metaData) {
    super.setStatusBarText(paramFile, metaData);
  }
}
/**
* <p> $Log$ </p>
*/