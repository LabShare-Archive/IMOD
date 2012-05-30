package etomo.ui.swing;

import etomo.BaseManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.storage.PeetFileFilter;
import etomo.type.AxisID;

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
 * <p> Revision 1.3  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.2  2007/02/21 04:22:33  sueh
 * <p> bug# 964 In getDataFileFilter, returning PeetFileFilter.
 * <p>
 * <p> Revision 1.1  2007/02/19 22:02:49  sueh
 * <p> bug# 964 Main panel for PEET interface.
 * <p> </p>
 */
public final class MainPeetPanel extends MainPanel {
  public static final String rcsid = "$Id$";

  private PeetProcessPanel axisPanelA = null;

  public MainPeetPanel(BaseManager manager) {
    super(manager);
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
    return new PeetFileFilter();
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
