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
  ProgressPanel progressPanel = new ProgressPanel("");
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
    new ProcessControlPanel("Post Processing");

  public AxisProcessPanel(ApplicationManager appManager, AxisID axis) {
    applicationManager = appManager;
    axisID = axis;

    //  Create the status panel
    buttonKillProcess.addActionListener(new KillButtonActionListener(this));
    buttonKillProcess.setEnabled(false);
    panelStatus.add(progressPanel.getContainer());
    panelStatus.add(buttonKillProcess);

    //  Create the process control panel    
    createProcessControlPanel();

    panelProcessSelect.setAlignmentY(Component.TOP_ALIGNMENT);
    panelProcessInfo.setAlignmentY(Component.TOP_ALIGNMENT);

    // panel layout structure
    //    panelProcessInfo.setLayout(
    //      new BoxLayout(panelProcessInfo, BoxLayout.Y_AXIS));
    panelProcessInfo.setLayout(new BorderLayout());
    panelProcessInfo.add(panelStatus, BorderLayout.NORTH);
    panelProcessInfo.add(panelDialog, BorderLayout.CENTER);

    panelRoot.setLayout(new BoxLayout(panelRoot, BoxLayout.X_AXIS));
    panelRoot.add(panelProcessSelect);
    panelRoot.add(panelProcessInfo);
  }

  public Container getContainer() {
    return panelRoot;
  }

  public void replaceDialogPanel(Container newDialog) {
    panelDialog.removeAll();
    panelDialog.add(newDialog);
    panelDialog.revalidate();
    panelDialog.repaint();
  }

  public void eraseDialogPanel() {
    //  Get the current panel size and a new blank panel of the same size
    panelDialog.removeAll();
    panelDialog.revalidate();
    panelDialog.repaint();
  }

  public void startProgressBar(String label) {
    progressPanel.setLabel(label);
    progressPanel.start();
  }

  public void stopProgressBar() {
    progressPanel.stop();
  }

  void buttonKillAction(ActionEvent event) {
    applicationManager.interrupt(axisID);
  }

  /**
   * Invoke the appropriate ApplicationManager method for the button press
   */
  public void buttonProcessAction(ActionEvent event) {
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

  public void setFineAlignmentState(ProcessState state) {
    procCtlFineAlignment.setState(state);
  }

  public void setTomogramPositioningState(ProcessState state) {
    procCtlTomogramPositioning.setState(state);
  }

  public void setTomogramGenerationState(ProcessState state) {
    procCtlTomogramGeneration.setState(state);
  }

  public void setTomogramCombinationState(ProcessState state) {
    procCtlTomogramCombination.setState(state);
  }

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

  //  Right mouse button context menu
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(panelRoot, mouseEvent, "");
  }

  /**
   * Initialize the tooltip text for the main window objects
   */
  private void setToolTipText() {
    procCtlPreProc.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the conversion of Digital Micrograph files, specifying<br>the CCD eraser parameters and performing the corr-<br>correlation required for coarse alignment.");
    procCtlCoarseAlign.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>the generation and examination of a coarse aligned<br>stack and the ability to fix alignment problems using Midas.");
    procCtlFiducialModel.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the construction of the fiducial model used to<br>develop the fine alignment of the projection images");
    procCtlFineAlignment.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the generation and examination of a finely aligned stack.");
    procCtlTomogramPositioning.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the bounding and positioning of the tomogram<br>volume and creating the final alignment parameters.");
    procCtlTomogramGeneration.setToolTipText(
      "<html>This process control panel opens a dialog box allowing<br>for the generation of the final aligned stack and generation<br>and examination of the tomogram.");
    procCtlTomogramCombination.setToolTipText(
      "<html>This process control panel is not yet complete<br>");
    procCtlPostProcessing.setToolTipText(
      "<html>This process control panel is not yet complete<br>");
  }

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