package etomo.ui;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import javax.swing.*;
import java.awt.Rectangle;

import etomo.type.AxisID;
import etomo.type.EtomoNumber;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.11  2005/04/12 19:36:14  sueh
 * <p> bug# 615 Made a newstuff version with the split pane and a very simple
 * <p> fitting algorithm.
 * <p>
 * <p> Revision 3.10  2005/04/01 02:52:10  sueh
 * <p> bug# 622 newstuff: try a raised border for each axis.
 * <p>
 * <p> Revision 3.9  2005/02/17 02:41:05  sueh
 * <p> bug# 605 Added saveDisplayState() to save the current width to lastWidth.
 * <p> In getWidth(), if lastWidth is not null return it instead of calling
 * <p> computeVisibleRect().
 * <p>
 * <p> Revision 3.8  2004/11/19 23:47:18  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.7.2.4  2004/10/11 02:09:17  sueh
 * <p> bug# 520 moved responsibility for the manager member variable to child
 * <p> classes.  Used abstract functions to use this variable in the base class.
 * <p> This is more reliable and doesn't require casting.
 * <p>
 * <p> Revision 3.7.2.3  2004/09/15 22:39:00  sueh
 * <p> bug# 520 This class must be abstract because the base constructor
 * <p> cannot call the overridden version of createProcessControlPanel.  This
 * <p> function may refer to member variables initialized in the child class.
 * <p>
 * <p> Revision 3.7.2.2  2004/09/08 20:04:43  sueh
 * <p> butg# 520 class doesn't have to be abstract.  manager should be saved in
 * <p> this base class because it is used here
 * <p>
 * <p> Revision 3.7.2.1  2004/09/08 19:46:06  sueh
 * <p> bug# 520 Converted AxisProcessPanel to a base class.  Move
 * <p> everything specific to creating tomograms to TomogramProcessPanel.
 * <p>
 * <p> Revision 3.7  2004/07/23 22:43:18  sueh
 * <p> bug# 517, bug# 513 adding comments, moving tooSmall() to
 * <p> MainFrame (renamed), hide() - get the width immediately before
 * <p> deciding whether to hide
 * <p>
 * <p> Revision 3.6  2004/07/23 00:03:05  sueh
 * <p> bug# 517 removing prints
 * <p>
 * <p> Revision 3.5  2004/07/16 23:00:21  sueh
 * <p> bug# 501 sending System.out prints only when debug is set
 * <p>
 * <p> Revision 3.4  2004/07/16 22:03:51  sueh
 * <p> bug# 501 adjusting divider to fix problem with
 * <p> JsplitPane.resetToPreferedSizes() that happens when
 * <p> etomo is too wide for the screen
 * <p>
 * <p> Revision 3.3  2004/05/19 23:09:46  sueh
 * <p> bug# 425 separating getting the panel width from deciding to hide
 * <p> because computeVisibleRect() is inaccurate after
 * <p> MainFrame.setDividerLocation() until the user has control of the screen
 * <p>
 * <p> Revision 3.2  2004/04/28 22:37:12  sueh
 * <p> bug# 268 hide() - return true if hid
 * <p>
 * <p> Revision 3.1  2004/02/13 17:36:43  sueh
 * <p> bug# 268 added hide and show function to make the root
 * <p> panel invisible and visible
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.12  2003/10/23 18:13:16  rickg
 * <p> Bug# 309
 * <p>
 * <p> Revision 2.11  2003/08/21 16:10:05  rickg
 * <p> Tooltip now use the TooltipFormatter class
 * <p>
 * <p> Revision 2.10  2003/07/01 22:55:28  rickg
 * <p> Fixed status panel layout
 * <p>
 * <p> Revision 2.9  2003/06/10 05:14:36  rickg
 * <p> Changes model for button selection to be managed by
 * <p> the application manager
 * <p>
 * <p> Revision 2.8  2003/06/09 04:26:16  rickg
 * <p> Manage button selected state
 * <p>
 * <p> Revision 2.7  2003/06/04 23:42:02  rickg
 * <p> Keep kill button grey out until a process is running
 * <p>
 * <p> Revision 2.6  2003/05/27 08:49:12  rickg
 * <p> Determinant progress bar now takes a string
 * <p>
 * <p> Revision 2.5  2003/05/23 14:23:53  rickg
 * <p> Progress bar determinant delegate methods
 * <p>
 * <p> Revision 2.4  2003/05/21 21:32:38  rickg
 * <p> Enabled kill process functionality
 * <p>
 * <p> Revision 2.3  2003/05/08 04:19:24  rickg
 * <p> Updated tooltips
 * <p>
 * <p> Revision 2.2  2003/02/24 23:24:15  rickg
 * <p> Added process killing button that is not yet implemented
 * <p>
 * <p> Revision 2.1  2003/01/27 23:54:07  rickg
 * <p> Align process panel along top
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public abstract class AxisProcessPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  protected AxisID axisID;

  protected JPanel panelRoot = new JPanel();
  private JPanel panelProcessInfo = new JPanel();
  protected JPanel panelStatus = new JPanel();
  protected JPanel panelDialog = new JPanel();
  private EtomoNumber lastWidth = new EtomoNumber(EtomoNumber.INTEGER_TYPE);

  //  Progress panel
  ProgressPanel progressPanel = new ProgressPanel("No process");
  JButton buttonKillProcess = new JButton("Kill Process");

  //  Process select panel
  protected JPanel panelProcessSelect = new JPanel();
  
  protected abstract void buttonKillAction(ActionEvent event);
  abstract void showBothAxis();

  /**
   * Constructor
   * @param appManager
   * @param axis
   */
  public AxisProcessPanel(AxisID axis) {
    axisID = axis;

    //  Create the status panel
    buttonKillProcess.addActionListener(new KillButtonActionListener(this));
    buttonKillProcess.setEnabled(false);
    buttonKillProcess.setAlignmentY(Component.BOTTOM_ALIGNMENT);
    panelStatus.add(Box.createRigidArea(FixedDim.x5_y0));
    panelStatus.add(progressPanel.getContainer());
    panelStatus.add(Box.createRigidArea(FixedDim.x5_y0));
    panelStatus.add(buttonKillProcess);
    panelStatus.add(Box.createRigidArea(FixedDim.x0_y5));
    panelStatus.setLayout(new BoxLayout(panelStatus, BoxLayout.X_AXIS));
  }
  
  protected void initializePanels() {
    panelProcessSelect.setAlignmentY(Component.TOP_ALIGNMENT);
    panelProcessInfo.setAlignmentY(Component.TOP_ALIGNMENT);

    panelProcessInfo.setLayout(new BorderLayout());
    panelProcessInfo.add(panelStatus, BorderLayout.NORTH);
    panelProcessInfo.add(panelDialog, BorderLayout.CENTER);

    panelRoot.setLayout(new BoxLayout(panelRoot, BoxLayout.X_AXIS));
    panelRoot.add(Box.createRigidArea(FixedDim.x5_y0));
    panelRoot.add(panelProcessSelect);
    panelRoot.add(panelProcessInfo);
    panelRoot.add(Box.createRigidArea(FixedDim.x5_y0));
  }
  
  /**
   * hide the panel if its width it zero because of the divider
   * @return
   */
  public boolean hide() {
    boolean hide = false;
    if (getWidth() != 0) {
      return hide;
    }
    hide = true;
    panelRoot.setVisible(false);
    return hide;
  }
  
  /**
   * make panel visible
   *
   */
  public void show() {
    panelRoot.setVisible(true);
  }
  
  void saveDisplayState() {
    lastWidth.set(getWidth());
  }
  
  /**
   * get panel width
   * @return
   */
  public int getWidth() {
    if (!lastWidth.isNull()) {
      int width = lastWidth.getInteger();
      lastWidth.reset();
      return width;
    }
    else {
      Rectangle size = new Rectangle();
      panelRoot.computeVisibleRect(size);
      return size.width;
    }
  }
  
  /**
   * 
   * @return
   */
  public Container getContainer() {
    return panelRoot;
  }

  /**
   * 
   * @param newDialog
   */
  public void replaceDialogPanel(Container newDialog) {
    panelDialog.removeAll();
    panelDialog.add(newDialog);
    panelDialog.revalidate();
    panelDialog.repaint();
  }

  /**
   * Remove all process information from the dialog panel
   */
  public void eraseDialogPanel() {
    //  Get the current panel size and a new blank panel of the same size
    panelDialog.removeAll();
    panelDialog.revalidate();
    panelDialog.repaint();
  }

  /**
   * Setup the progress bar for a determinate
   * @param label
   * @param nSteps
   */
  public void setProgressBar(String label, int nSteps) {
    progressPanel.setLabel(label);
    progressPanel.setMinimum(0);
    progressPanel.setMaximum(nSteps);
    buttonKillProcess.setEnabled(true);
  }

  /**
   * 
   * @param n
   */
  public void setProgressBarValue(int n) {
    progressPanel.setValue(n);
  }

  /**
   * 
   * @param n
   */
  public void setProgressBarValue(int n, String string) {
    progressPanel.setValue(n, string);
  }

  /**
   * 
   * @param label
   */
  public void startProgressBar(String label) {
    progressPanel.setLabel(label);
    progressPanel.start();
    buttonKillProcess.setEnabled(true);
  }

  /**
   * 
   *
   */
  public void stopProgressBar() {
    progressPanel.stop();
    buttonKillProcess.setEnabled(false);
  }

  /**
     * Right mouse button context menu
     */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(panelRoot, mouseEvent, "");
  }

  /**
  * Action listener to handle process kill
  */
  class KillButtonActionListener implements ActionListener {
    AxisProcessPanel adaptee;
    KillButtonActionListener(AxisProcessPanel adaptee) {
      this.adaptee = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      adaptee.buttonKillAction(event);
    }
  }
  
  protected void createProcessControlPanel() {
    panelProcessSelect.setLayout(new BoxLayout(panelProcessSelect,
        BoxLayout.Y_AXIS));

    if (axisID == AxisID.FIRST) {
      JLabel axisLabel = new JLabel("Axis A:");
      axisLabel.setAlignmentX(Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(axisLabel);
    }
    if (axisID == AxisID.SECOND) {
      JLabel axisLabel = new JLabel("Axis B:");
      axisLabel.setAlignmentX(Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(axisLabel);
    }
  }

}
