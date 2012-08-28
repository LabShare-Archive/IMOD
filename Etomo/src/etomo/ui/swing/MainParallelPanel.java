package etomo.ui.swing;

import etomo.ParallelManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.type.AxisID;

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
 * <p> $Log$
 * <p> Revision 1.4  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.3  2008/07/19 00:56:35  sueh
 * <p> Reduced exposure by removing "protected" directive.
 * <p>
 * <p> Revision 1.2  2007/02/19 22:02:30  sueh
 * <p> bug# 964 Fixed function names:  was AxisPanelIsNull, now its isAxisPanelNull.
 * <p>
 * <p> Revision 1.1  2006/03/20 18:04:25  sueh
 * <p> bug# 835 Added a main panel for ParallelManager.
 * <p> </p>
 */
public final class MainParallelPanel extends MainPanel {
  public static final String rcsid = "$Id$";

  private final ParallelManager manager;

  private ParallelProcessPanel axisPanelA = null;

  public MainParallelPanel(ParallelManager manager) {
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
    axisPanelA = new ParallelProcessPanel(manager);
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
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.3  2008/07/19 00:56:35  sueh
 * <p> Reduced exposure by removing "protected" directive.
 * <p>
 * <p> Revision 1.2  2007/02/19 22:02:30  sueh
 * <p> bug# 964 Fixed function names:  was AxisPanelIsNull, now its isAxisPanelNull.
 * <p>
 * <p> Revision 1.1  2006/03/20 18:04:25  sueh
 * <p> bug# 835 Added a main panel for ParallelManager.
 * <p> </p>
 */
