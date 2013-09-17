package etomo.ui.swing;

import java.io.File;

import javax.swing.JPanel;

import etomo.JoinManager;
import etomo.process.ProcessState;
import etomo.storage.DataFileFilter;
import etomo.storage.JoinFileFilter;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;

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
 * <p> Revision 1.12  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.11  2007/02/19 22:02:10  sueh
 * <p> bug# 964 Fixed function names:  was AxisPanelIsNull, now its isAxisPanelNull.
 * <p>
 * <p> Revision 1.10  2006/03/20 18:03:23  sueh
 * <p> bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
 * <p>
 * <p> Revision 1.9  2005/09/21 16:38:57  sueh
 * <p> bug# 532 Added empty implementation of setState(), which is required by
 * <p> MainPanel.
 * <p>
 * <p> Revision 1.8  2005/08/04 20:11:52  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 1.7  2005/05/17 19:37:56  sueh
 * <p> bug# 663 Renamed updateDataParameters() to setStatusBarText() and
 * <p> moved the common functionality to MainPanel.setStatusBarText().
 * <p>
 * <p> Revision 1.6  2005/04/26 17:40:25  sueh
 * <p> bug# 615 Made MainFrame a package-level class.  All MainFrame
 * <p> functionality is handled through UIHarness to make Etomo more
 * <p> compatible with JUnit.
 * <p>
 * <p> Revision 1.5  2005/04/21 20:39:05  sueh
 * <p> bug# 615 Moved two frame code out of newstuff.  Removed
 * <p> isAxisPanelAFitScreenError(), since it is not necessary.
 * <p>
 * <p> Revision 1.4  2005/04/01 02:53:27  sueh
 * <p> bug# 622 Added getAxisPAnelA and B.
 * <p>
 * <p> Revision 1.3  2005/02/17 02:41:41  sueh
 * <p> bug# 605 Added empty saveDisplayState().
 * <p>
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
  public static final String rcsid = "$Id$";

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

  protected AxisProcessPanel getAxisPanelA() {
    return axisPanelA;
  }

  protected AxisProcessPanel getAxisPanelB() {
    return null;
  }

  /**
   * Open the setup panel
   */
  public void openPanel(JPanel panel) {
    scrollA.add(panel);
    revalidate();
    UIHarness.INSTANCE.pack(manager);
  }

  /**
   * Set the status bar with the file name of the data parameter file
   */
  public final void setStatusBarText(File paramFile, BaseMetaData metaData,
      LogWindow logWindow) {
    StringBuffer buffer = new StringBuffer();
    if (metaData == null || !metaData.isValid()) {
      statusBar.setText(buffer.toString());
    }
    else {
      super.setStatusBarText(paramFile, metaData, logWindow);
    }
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

  protected boolean isAxisPanelANull() {
    return axisPanelA == null;
  }

  protected boolean isAxisPanelBNull() {
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

  protected AxisProcessPanel mapBaseAxis(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return axisPanelB;
    }
    return axisPanelA;
  }

  public final void setState(ProcessState processState, AxisID axisID,
      AbstractParallelDialog parallelDialog) {
  }
}
