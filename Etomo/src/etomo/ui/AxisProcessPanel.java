package etomo.ui;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.process.ProcessState;
import etomo.type.AxisID;

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

public class AxisProcessPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;
  private AxisID axisID;

  private JPanel panelRoot = new JPanel();
  private JPanel panelProcessInfo = new JPanel();
  private JPanel panelStatus = new JPanel();
  private JPanel panelDialog = new JPanel();

  //  Progress panel
  ProgressPanel progressPanel = new ProgressPanel("No process");
  JButton buttonKillProcess = new JButton("Kill Process");

  //  Process select panel
  private JPanel panelProcessSelect = new JPanel();
  private ProcessControlPanel procCtlPreProc =
    new ProcessControlPanel("Pre-processing");
  private ProcessControlPanel procCtlCoarseAlign =
    new ProcessControlPanel("Coarse Alignment");
  private ProcessControlPanel procCtlFiducialModel =
    new ProcessControlPanel("Fiducial Model Gen.");
  private ProcessControlPanel procCtlFineAlignment =
    new ProcessControlPanel("Fine Alignment");
  private ProcessControlPanel procCtlTomogramPositioning =
    new ProcessControlPanel("Tomogram Positioning");
  private ProcessControlPanel procCtlTomogramGeneration =
    new ProcessControlPanel("Tomogram Generation");
  private ProcessControlPanel procCtlTomogramCombination =
    new ProcessControlPanel("Tomogram Combination");
  private ProcessControlPanel procCtlPostProcessing =
    new ProcessControlPanel("Post-processing");

  /**
   * Constructor
   * @param appManager
   * @param axis
   */
  public AxisProcessPanel(ApplicationManager appManager, AxisID axis) {
    applicationManager = appManager;
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

    //  Create the process control panel    
    createProcessControlPanel();

    panelProcessSelect.setAlignmentY(Component.TOP_ALIGNMENT);
    panelProcessInfo.setAlignmentY(Component.TOP_ALIGNMENT);

    panelProcessInfo.setLayout(new BorderLayout());
    panelProcessInfo.add(panelStatus, BorderLayout.NORTH);
    panelProcessInfo.add(panelDialog, BorderLayout.CENTER);

    panelRoot.setLayout(new BoxLayout(panelRoot, BoxLayout.X_AXIS));
    panelRoot.add(panelProcessSelect);
    panelRoot.add(panelProcessInfo);
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
   * 
   * @param event
   */
  private void buttonKillAction(ActionEvent event) {
    applicationManager.kill(axisID);
  }

  /**
   * Invoke the appropriate ApplicationManager method for the button press
   */
  private void buttonProcessAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(procCtlPreProc.getName())) {
      applicationManager.openPreProcDialog(axisID);
      return;
    }

    if (command.equals(procCtlCoarseAlign.getName())) {
      applicationManager.openCoarseAlignDialog(axisID);
      return;
    }

    if (command.equals(procCtlFiducialModel.getName())) {
      applicationManager.openFiducialModelDialog(axisID);
      return;
    }

    if (command.equals(procCtlFineAlignment.getName())) {
      applicationManager.openFineAlignmentDialog(axisID);
      return;
    }
    if (command.equals(procCtlTomogramPositioning.getName())) {
      applicationManager.openTomogramPositioningDialog(axisID);
      return;
    }

    if (command.equals(procCtlTomogramGeneration.getName())) {
      applicationManager.openTomogramGenerationDialog(axisID);
      return;
    }

    if (command.equals(procCtlTomogramCombination.getName())) {
      applicationManager.openTomogramCombinationDialog();
      return;
    }

    if (command.equals(procCtlPostProcessing.getName())) {
      applicationManager.openPostProcessingDialog();
      return;
    }
  }

  /**
    * Pre-processing panel state control
    */
  public void setPreProcState(ProcessState state) {
    procCtlPreProc.setState(state);
  }

  /**
   * Coarse alignment panel state control
   */
  public void setCoarseAlignState(ProcessState state) {
    procCtlCoarseAlign.setState(state);
  }

  /**
   * Fiducial model panel state control
   */
  public void setFiducialModelState(ProcessState state) {
    procCtlFiducialModel.setState(state);
  }

  /**
   * 
   * @param state
   */
  public void setFineAlignmentState(ProcessState state) {
    procCtlFineAlignment.setState(state);
  }

  /**
   * 
   * @param state
   */
  public void setTomogramPositioningState(ProcessState state) {
    procCtlTomogramPositioning.setState(state);
  }

  /**
   * 
   * @param state
   */
  public void setTomogramGenerationState(ProcessState state) {
    procCtlTomogramGeneration.setState(state);
  }

  /**
   * 
   * @param state
   */
  public void setTomogramCombinationState(ProcessState state) {
    procCtlTomogramCombination.setState(state);
  }

  /**
   * 
   * @param state
   */
  public void setPostProcessingState(ProcessState state) {
    procCtlPostProcessing.setState(state);
  }

  /**
   *  Build the process control panels
   */
  private void createProcessControlPanel() {

    //  Bind each button to action listener and the generic mouse listener
    ProcessButtonActionListener buttonListener =
      new ProcessButtonActionListener(this);
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);

    setToolTipText();

    panelProcessSelect.setLayout(
      new BoxLayout(panelProcessSelect, BoxLayout.Y_AXIS));

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
    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
    procCtlPreProc.setButtonActionListener(buttonListener);
    procCtlPreProc.addMouseListener(mouseAdapter);
    procCtlPreProc.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlPreProc.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
    procCtlCoarseAlign.addMouseListener(mouseAdapter);
    procCtlCoarseAlign.setButtonActionListener(buttonListener);
    procCtlCoarseAlign.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlCoarseAlign.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
    procCtlFiducialModel.addMouseListener(mouseAdapter);
    procCtlFiducialModel.setButtonActionListener(buttonListener);
    procCtlFiducialModel.getContainer().setAlignmentX(
      Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlFiducialModel.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
    procCtlFineAlignment.addMouseListener(mouseAdapter);
    procCtlFineAlignment.setButtonActionListener(buttonListener);
    procCtlFineAlignment.getContainer().setAlignmentX(
      Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlFineAlignment.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
    procCtlTomogramPositioning.addMouseListener(mouseAdapter);
    procCtlTomogramPositioning.setButtonActionListener(buttonListener);
    procCtlTomogramPositioning.getContainer().setAlignmentX(
      Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlTomogramPositioning.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
    procCtlTomogramGeneration.addMouseListener(mouseAdapter);
    procCtlTomogramGeneration.setButtonActionListener(buttonListener);
    procCtlTomogramGeneration.getContainer().setAlignmentX(
      Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlTomogramGeneration.getContainer());

    if (axisID == AxisID.FIRST) {
      panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
      procCtlTomogramCombination.addMouseListener(mouseAdapter);
      procCtlTomogramCombination.setButtonActionListener(buttonListener);
      procCtlTomogramCombination.getContainer().setAlignmentX(
        Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(procCtlTomogramCombination.getContainer());
    }
    if (axisID != AxisID.SECOND) {
      panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y20));
      procCtlPostProcessing.addMouseListener(mouseAdapter);
      procCtlPostProcessing.setButtonActionListener(buttonListener);
      procCtlPostProcessing.getContainer().setAlignmentX(
        Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(procCtlPostProcessing.getContainer());
    }
  }

  /**
   * Select the requested button
   * @param name
   */
  public void selectButton(String name) {
    unSelectAll();
    if (name.equals(procCtlPreProc.getName())) {
      procCtlPreProc.setSelected(true);
      return;
    }
    if (name.equals(procCtlCoarseAlign.getName())) {
      procCtlCoarseAlign.setSelected(true);
      return;
    }
    if (name.equals(procCtlFiducialModel.getName())) {
      procCtlFiducialModel.setSelected(true);
      return;
    }
    if (name.equals(procCtlFineAlignment.getName())) {
      procCtlFineAlignment.setSelected(true);
      return;
    }
    if (name.equals(procCtlTomogramPositioning.getName())) {
      procCtlTomogramPositioning.setSelected(true);
      return;
    }
    if (name.equals(procCtlTomogramGeneration.getName())) {
      procCtlTomogramGeneration.setSelected(true);
      return;
    }
    if (name.equals(procCtlTomogramCombination.getName())) {
      procCtlTomogramCombination.setSelected(true);
      return;
    }
    if (name.equals(procCtlPostProcessing.getName())) {
      procCtlPostProcessing.setSelected(true);
      return;
    }
  }

  private void unSelectAll() {
    procCtlPreProc.setSelected(false);
    procCtlCoarseAlign.setSelected(false);
    procCtlFiducialModel.setSelected(false);
    procCtlFineAlignment.setSelected(false);
    procCtlTomogramPositioning.setSelected(false);
    procCtlTomogramGeneration.setSelected(false);
    procCtlTomogramCombination.setSelected(false);
    procCtlPostProcessing.setSelected(false);
  }

  /**
     * Right mouse button context menu
     */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(panelRoot, mouseEvent, "");
  }

  /**
     * Initialize the tooltip text for the axis panel objects
     */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    text =
      "Open the Pre-processing panel to erase x-rays, bad pixels and/or bad"
        + " CCD rows from the raw projection stack.";
    procCtlPreProc.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Open the Coarse Alignment panel to generate a coarsely aligned "
        + "stack using cross correlation and to fix coarse alignment problems "
        + "with Midas.";
    procCtlCoarseAlign.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Open the Fiducial Model Generation panel to create a fiducial "
        + "model to be used in the fine alignment step.";
    procCtlFiducialModel.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Open the Fine Alignment panel to use the generated fiducial model to "
        + "sub-pixel align the project sequence.";
    procCtlFineAlignment.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Open the Tomogram Position panel to optimally adjust the 3D location "
        + "and size of the reconstruction volume.";
    procCtlTomogramPositioning.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Open the Tomogram Generation panel to generate the final aligned "
        + "stack and calcuate the tomographic reconstruction.";
    procCtlTomogramGeneration.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Open the Tomogram Combination panel to combine the tomograms generated "
        + "from the A and B axes into a single dual axis reconstruction.";
    procCtlTomogramCombination.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Open the Post Processing panel to trim the final reconstruction to size"
        + " and delete the intermediate files.";
    procCtlPostProcessing.setToolTipText(
      tooltipFormatter.setText(text).format());
  }

  /**
  *   Action listeners to handle process panel events
  */
  class ProcessButtonActionListener implements ActionListener {
    AxisProcessPanel adaptee;
    ProcessButtonActionListener(AxisProcessPanel adaptee) {
      this.adaptee = adaptee;
    }
    public void actionPerformed(ActionEvent event) {
      adaptee.buttonProcessAction(event);
    }
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
}
