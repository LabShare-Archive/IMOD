package etomo.ui.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import javax.swing.*;

import java.awt.Rectangle;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;

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
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.42  2010/07/02 03:18:03  sueh
 * <p> bug# 1388 Added popupChunkWarnings.
 * <p>
 * <p> Revision 3.41  2010/03/03 05:00:48  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 3.40  2009/10/27 20:41:01  sueh
 * <p> bug# 1275 In initializePanels only add the outerStatusPanel if the manager
 * <p> contains a process manager.  The outerStatusPanel contains the progress
 * <p> panel.
 * <p>
 * <p> Revision 3.39  2009/01/20 19:44:29  sueh
 * <p> bug# 1102 Changed button to type SimpleButton so that they can name themselves.
 * <p>
 * <p> Revision 3.38  2008/10/06 22:37:41  sueh
 * <p> bug# 1113 Removed pack, which is unecessary since scrolling was
 * <p> removed.
 * <p>
 * <p> Revision 3.37  2008/09/30 20:55:45  sueh
 * <p> bug# 1113 Using a private constructor in ProgressPanel.
 * <p>
 * <p> Revision 3.36  2008/05/30 22:31:27  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 3.35  2008/05/30 21:27:44  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 3.34  2007/12/26 22:22:22  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 3.33  2007/09/27 20:31:22  sueh
 * <p> bug# 1044 Made ProcessorTable the ParallelProgress display instead of
 * <p> ParallelPanel.
 * <p>
 * <p> Revision 3.32  2007/09/07 00:25:48  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.31  2007/05/26 00:31:45  sueh
 * <p> bug# 994 Using getInstance in ParallelPanel.
 * <p>
 * <p> Revision 3.30  2007/02/05 23:32:33  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 3.29  2006/11/29 00:18:36  sueh
 * <p> bug# 934 Added endThreads() to notify the load average threads when the
 * <p> manager exits.
 * <p>
 * <p> Revision 3.28  2006/11/07 22:32:04  sueh
 * <p> bug# 954 Adding tooltip for buttonKillProcess.
 * <p>
 * <p> Revision 3.27  2006/10/24 22:33:41  sueh
 * <p> bug# 947
 * <p>
 * <p> Revision 3.26  2006/08/10 17:45:43  sueh
 * <p> bug# 686 Passing manager and axis to ProgressPanel.
 * <p>
 * <p> Revision 3.25  2006/06/21 15:49:49  sueh
 * <p> bug# 581 Passing manager and axis to ContextPopup, so that imodqtassist can
 * <p> be run.
 * <p>
 * <p> Revision 3.24  2006/04/25 19:00:18  sueh
 * <p> bug# 787 Giving the "Kill Process" button a name and making the name
 * <p> public.
 * <p>
 * <p> Revision 3.23  2006/03/20 18:01:42  sueh
 * <p> Improved the name of the parameter of setParallelDialog
 * <p>
 * <p> Revision 3.22  2006/01/12 17:06:02  sueh
 * <p> bug# 798 Reducing the visibility and inheritability of ui classes.
 * <p>
 * <p> Revision 3.21  2005/10/15 00:32:29  sueh
 * <p> bug# 532 Moved functionality to keep track of when the parallel panel
 * <p> should be started and stopped from BaseManager to this class.  Changed
 * <p> showParallelPanel() to setParallelDialog().  Added setParallelInUse().
 * <p> Added booleans: parallelDialog, parallelInUse, parallelShowing.
 * <p> SetParallelDialog() tells this class when the current dialog has a parallel
 * <p> processing checkbox checked.  setParallelInUse() tells this class when
 * <p> the parallel panel is being used.  The parallel panel is shown only when it
 * <p> is in use or when the dialog has a selected parallel processing checkbox.
 * <p>
 * <p> Revision 3.20  2005/09/27 23:24:44  sueh
 * <p> bug# 532 Saving the state of the parallel header.  Since
 * <p> AxisProcessPanel is in the ui package, no longer refering to ParallelPanel
 * <p> through the interface.  Adding done() to save the parallel header state.
 * <p>
 * <p> Revision 3.19  2005/09/22 20:55:14  sueh
 * <p> bug# 532 Managing the parallel process panel (ParallelProgressDisplay).
 * <p> Added parallelStatusPanel to hold the parallel process panel.  Added
 * <p> showParallelStatus() to signal when to show the panel.  Removed
 * <p> setParallelProgressDisplay() so that the parallel process panel could be
 * <p> set by showParallelStatus().
 * <p>
 * <p> Revision 3.18  2005/08/22 17:53:59  sueh
 * <p> bug# 532  Add status string, remove pause button and add parallel
 * <p> progress display instead so that the button will be managed indirectly.
 * <p> The pause button should only be turned on for processes that can be
 * <p> paused.
 * <p>
 * <p> Revision 3.17  2005/08/09 20:12:51  sueh
 * <p> bug# 711  No longer inheriting JButton in MultiLineButton.  This allows
 * <p> MultiLineButton to treate toggling as an attribute.  Then we can get rid of
 * <p> MultiLineToggleButton.  Then we can have one Run3dmodButton which
 * <p> can be toggle or non-toggle.
 * <p>
 * <p> Revision 3.16  2005/07/29 19:47:36  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 3.15  2005/07/26 23:07:11  sueh
 * <p> bug# 701 When stopping the progress bar, pass the process end state.
 * <p>
 * <p> Revision 3.14  2005/07/21 22:18:35  sueh
 * <p> bug# 532 removed "kill / pause" label from kill process button.  Pause
 * <p> button with be managed by separately by AxisProcessPanel, which
 * <p> receives a pointer to the button through setPauseButton().
 * <p>
 * <p> Revision 3.13  2005/07/11 22:53:42  sueh
 * <p> bug# 619 Allow the kill process button label to be changed to
 * <p> "Kill / Pause".  Added setProgressBar(String, int, boolean) and
 * <p> startProgressBar(String, boolean).
 * <p>
 * <p> Revision 3.12  2005/04/16 01:53:28  sueh
 * <p> bug# 615 Made some panels protected so there color could be changed.
 * <p> Added a rigid area to the bottom of panelRoot so that the color would wrap
 * <p> around the process buttons.
 * <p>
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
  public static final String rcsid = "$Id$";

  public static final String KILL_BUTTON_LABEL = "Kill Process";

  protected final BaseManager manager;
  protected final AxisID axisID;

  protected JPanel panelRoot = new JPanel();
  private JPanel panelProcessInfo = new JPanel();
  protected final JPanel outerStatusPanel = new JPanel();
  protected final JPanel innerStatusPanel = new JPanel();
  protected JPanel panelDialog = new JPanel();
  protected final JPanel parallelStatusPanel = new JPanel();
  private EtomoNumber lastWidth = new EtomoNumber(EtomoNumber.Type.INTEGER);
  private KillButtonActionListener actionListener;
  private boolean parallelShowing = false;
  /**
   * processingMethodLocked: when on, prevents any changes to visibility of
   * parallel panel.
   */
  private boolean processingMethodLocked = false;

  private final boolean popupChunkWarnings;

  // Progress panel
  final ProgressPanel progressPanel;
  private final SimpleButton buttonKillProcess = new SimpleButton(KILL_BUTTON_LABEL);
  private ParallelPanel parallelPanel = null;

  // Process select panel
  protected JPanel panelProcessSelect = new JPanel();

  void showBothAxis() {
  }

  /**
   * Sets panel color (does not include the axis button panel)
   * @param color
   */
  void setBackground(Color color) {
    panelRoot.setBackground(color);
    outerStatusPanel.setBackground(color);
    innerStatusPanel.setBackground(color);
    parallelStatusPanel.setBackground(color);
    panelDialog.setBackground(color);
    panelProcessSelect.setBackground(color);
    progressPanel.setBackground(color);
  }

  /**
   * Constructor
   * @param appManager
   * @param axis
   */
  AxisProcessPanel(AxisID axis, BaseManager manager, boolean popupChunkWarnings) {
    progressPanel = ProgressPanel.getInstance("No process", manager, axis);
    axisID = axis;
    this.manager = manager;
    this.popupChunkWarnings = popupChunkWarnings;
    // Create the status panel
    actionListener = new KillButtonActionListener(this);
    buttonKillProcess.addActionListener(actionListener);
    buttonKillProcess.setEnabled(false);
    buttonKillProcess.setAlignmentY(Component.BOTTOM_ALIGNMENT);
    outerStatusPanel.setLayout(new BoxLayout(outerStatusPanel, BoxLayout.Y_AXIS));
    outerStatusPanel.add(innerStatusPanel);
    parallelStatusPanel.setVisible(false);
    outerStatusPanel.add(parallelStatusPanel);
    innerStatusPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    innerStatusPanel.add(progressPanel.getContainer());
    innerStatusPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    innerStatusPanel.add(buttonKillProcess);
    innerStatusPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    innerStatusPanel.setLayout(new BoxLayout(innerStatusPanel, BoxLayout.X_AXIS));
    buttonKillProcess.setToolTipText("Press to end the current process.");
    manager.getProcessingMethodMediator(axis).register(this);
  }

  void buttonKillAction(final ActionEvent event) {
    manager.kill(axisID);
  }

  protected void initializePanels() {
    panelProcessSelect.setAlignmentY(Component.TOP_ALIGNMENT);
    panelProcessInfo.setAlignmentY(Component.TOP_ALIGNMENT);

    panelProcessInfo.setLayout(new BorderLayout());
    if (manager.getProcessManager() != null) {
      panelProcessInfo.add(outerStatusPanel, BorderLayout.NORTH);
    }
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
  final boolean hide() {
    boolean hide = false;
    if (getWidth() != 0) {
      return hide;
    }
    hide = true;
    panelRoot.setVisible(false);
    return hide;
  }

  public void lockProcessingMethod(final boolean lock) {
    processingMethodLocked = lock;
  }

  /**
   * Create and show, or hide parallel panel if necessary.
   * @param method
   */
  public void showParallelPanel(final boolean show) {
    showParallelPanel(show, false);
  }

  /**
   * Call showParallelPanel, ignoring processingMethodLocked.
   * @param show
   */
  public void forceShowParallelPanel(final boolean show) {
    showParallelPanel(show, true);
  }

  /**
   * Create and show, or hide parallel panel if necessary.
   * @param method
   */
  private void showParallelPanel(final boolean show, final boolean force) {
    if (processingMethodLocked && !force) {
      return;
    }
    if (!show) {
      // Parallel panel is not in use, hide it if necessary
      if (parallelPanel != null && parallelShowing) {
        parallelShowing = false;
        parallelStatusPanel.setVisible(false);
        UIHarness.INSTANCE.pack(axisID, manager);
      }
    }
    else {
      if (parallelPanel == null) {
        parallelPanel = ParallelPanel.getInstance(manager, axisID, manager
            .getBaseScreenState(axisID).getParallelHeaderState(), this,
            popupChunkWarnings);
        parallelStatusPanel.add(Box.createRigidArea(FixedDim.x5_y0));
        parallelStatusPanel.add(parallelPanel.getContainer());
      }
      if (!parallelShowing) {
        parallelShowing = true;
        parallelStatusPanel.setVisible(true);
        UIHarness.INSTANCE.pack(axisID, manager);
      }
    }
  }

  private final void startParallelPanel() {
    parallelShowing = true;
    parallelPanel.getLoadDisplay().startLoad();
    parallelStatusPanel.setVisible(true);
    UIHarness.INSTANCE.pack(axisID, manager);
  }
  
  private final void stopParallelPanel() {
    parallelShowing = false;
    parallelPanel.getLoadDisplay().stopLoad();
    parallelStatusPanel.setVisible(false);
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  final ParallelPanel getParallelPanel() {
    return parallelPanel;
  }

  final void done() {
    if (parallelPanel != null) {
      parallelPanel.getHeaderState(manager.getBaseScreenState(axisID)
          .getParallelHeaderState());
    }
  }

  /**
   * make panel visible
   *
   */
  final void show() {
    panelRoot.setVisible(true);
  }

  final void saveDisplayState() {
    lastWidth.set(getWidth());
  }

  /**
   * get panel width
   * @return
   */
  final int getWidth() {
    if (!lastWidth.isNull()) {
      int width = lastWidth.getInt();
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
  final Container getContainer() {
    return panelRoot;
  }

  /**
   * 
   * @param newDialog
   */
  final void replaceDialogPanel(Container newDialog) {
    panelDialog.removeAll();
    panelDialog.add(newDialog);
    panelDialog.revalidate();
    panelDialog.repaint();
  }

  /**
   * Remove all process information from the dialog panel
   */
  final void eraseDialogPanel() {
    // Get the current panel size and a new blank panel of the same size
    panelDialog.removeAll();
    panelDialog.revalidate();
    panelDialog.repaint();
  }

  /**
   * Setup the progress bar for a determinate
   * @param label
   * @param nSteps
   */
  final void setProgressBar(String label, int nSteps, boolean enablePause) {
    progressPanel.setLabel(label);
    progressPanel.setMinimum(0);
    progressPanel.setMaximum(nSteps);
    buttonKillProcess.setEnabled(true);
    if (parallelPanel != null) {
      parallelPanel.setPauseEnabled(enablePause);
    }
  }

  /**
   * Setup the progress bar for a state, not a process (with no moving bar, or percentage,
   * and without the kill button being enabled.
   * @param label
   * @param nSteps
   */
  final void setStaticProgressBar(String label) {
    progressPanel.setLabel(label);
  }

  /**
   * 
   * @param n
   */
  final void setProgressBarValue(int n) {
    progressPanel.setValue(n);
  }

  /**
   * 
   * @param n
   */
  final void setProgressBarValue(int n, String string) {
    progressPanel.setValue(n, string);
  }

  /**
   * 
   * @param label
   */
  final void startProgressBar(String label, ProcessName processName) {
    progressPanel.setLabel(label);
    progressPanel.start();
    buttonKillProcess.setEnabled(true);
  }

  /**
   * 
   *
   */
  final void stopProgressBar(ProcessEndState processEndState, String statusString) {
    progressPanel.stop(processEndState, statusString);
    buttonKillProcess.setEnabled(false);
    if (parallelPanel != null) {
      parallelPanel.setPauseEnabled(false);
    }
  }

  /**
   * Right mouse button context menu
   */
  public final void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(panelRoot, mouseEvent, "", manager,
        axisID);
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

  void createProcessControlPanel() {
    panelProcessSelect.setLayout(new BoxLayout(panelProcessSelect, BoxLayout.Y_AXIS));

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
