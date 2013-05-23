package etomo.ui.swing;

import etomo.DirectiveEditorManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class MainDirectiveEditorPanel extends MainPanel{
  public static final String rcsid = "$Id:$";

  private final DirectiveEditorManager manager;

  private DirectiveEditorProcessPanel axisPanelA = null;

  public MainDirectiveEditorPanel(final DirectiveEditorManager manager) {
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
    axisPanelA = new DirectiveEditorProcessPanel(manager);
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

  public final void setStatusBarText(final String directory, final int maxTitleLength) {
    super.setStatusBarTextToDirectory(directory, maxTitleLength);
  }
}
