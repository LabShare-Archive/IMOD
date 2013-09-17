package etomo.ui.swing;

import java.io.File;

import etomo.FrontPageManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/10/27 20:43:02  sueh
 * <p> bug# 1275 Required class for FrontPageManager.
 * <p> </p>
 */
public final class MainFrontPagePanel extends MainPanel {
  public static final String rcsid = "$Id$";

  private final FrontPageManager manager;

  private FrontPageProcessPanel axisPanelA = null;

  public MainFrontPagePanel(FrontPageManager manager) {
    super(manager);
    this.manager = manager;
  }

  void addAxisPanelA() {
    scrollA.add(axisPanelA.getContainer());
  }

  void addAxisPanelB() {
  }

  boolean isAxisPanelANull() {
    return axisPanelA == null;
  }

  boolean isAxisPanelBNull() {
    return true;
  }

  void createAxisPanelA(AxisID axisID) {
    axisPanelA = new FrontPageProcessPanel(manager);
  }

  void createAxisPanelB() {
  }

  AxisProcessPanel getAxisPanelA() {
    return axisPanelA;
  }

  AxisProcessPanel getAxisPanelB() {
    return null;
  }

  DataFileFilter getDataFileFilter() {
    return null;
  }

  boolean hideAxisPanelA() {
    return axisPanelA.hide();
  }

  boolean hideAxisPanelB() {
    return true;
  }

  AxisProcessPanel mapBaseAxis(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return null;
    }
    return axisPanelA;
  }

  void resetAxisPanels() {
    axisPanelA = null;
  }

  public void saveDisplayState() {
  }

  public void setState(ProcessState processState, AxisID axisID,
      AbstractParallelDialog parallelDialog) {
  }

  void showAxisPanelA() {
    axisPanelA.show();
  }

  void showAxisPanelB() {
  }

  public final void setStatusBarText(File paramFile, BaseMetaData metaData,
      LogWindow logWindow) {
    statusBar.setText(metaData.getName());
  }
}
