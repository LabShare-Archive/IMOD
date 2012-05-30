package etomo.ui.swing;

import etomo.BaseManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.storage.SerialSectionsFileFilter;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class MainSerialSectionsPanel extends MainPanel {
  public static final String rcsid = "$Id:$";

  private SerialSectionsProcessPanel axisPanelA = null;

  public MainSerialSectionsPanel(final BaseManager manager) {
    super(manager);
  }

  void addAxisPanelA() {
    scrollA.add(axisPanelA.getContainer());
  }

  void addAxisPanelB() {
  }

  void createAxisPanelA(final AxisID axisID) {
    axisPanelA = new SerialSectionsProcessPanel(manager);
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
    return new SerialSectionsFileFilter();
  }

  boolean hideAxisPanelA() {
    return axisPanelA.hide();
  }

  boolean hideAxisPanelB() {
    return true;
  }

  boolean isAxisPanelANull() {
    return axisPanelA == null;
  }

  boolean isAxisPanelBNull() {
    return true;
  }

  AxisProcessPanel mapBaseAxis(final AxisID axisID) {
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

  public void setState(final ProcessState processState, final AxisID axisID,
      final AbstractParallelDialog parallelDialog) {
  }

  void showAxisPanelA() {
    axisPanelA.show();
  }

  void showAxisPanelB() {
  }
}
