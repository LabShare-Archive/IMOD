package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.process.ProcessState;
import etomo.type.AxisID;
import etomo.type.DialogType;

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
 * <p> Revision 1.4  2005/03/01 21:00:56  sueh
 * <p> Bug# 610 Keeping track of current dialog type in ApplicationManager by
 * <p> setting it in each open function.  Changing saveDialog to
 * <p> saveCurrentDialog and use currentDialogType to pick the dialog to save.
 * <p>
 * <p> Revision 1.3  2005/01/14 03:14:14  sueh
 * <p> bug# 511 Creating ProcessControlPanels with DialogType instead of
 * <p> strings.  Added currentProcess to process the most recent panel before
 * <p> switching dialogs.  Calling applicationManager.saveDialog(DialogType, AxisID)
 * <p> before switching dialogs.
 * <p>
 * <p> Revision 1.2  2004/11/20 00:06:57  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/11 02:25:04  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 1.1.2.2  2004/09/29 19:45:25  sueh
 * <p> bug# 520 Created private variables that are cast from base-class
 * <p> member variables during construction.  Moved status bar initiailization from
 * <p> base class.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/08 20:14:27  sueh
 * <p> bug# 520 class contains tomogram specific functionality from
 * <p> AxisProcessPanel,
 * <p> which is its base class.  Casts member variables which are used as super
 * <p> classes in MainPanel.
 * <p> </p>
 */
public class TomogramProcessPanel extends AxisProcessPanel {
  public static final String rcsid = "$Id$";
  
  private ProcessControlPanel procCtlPreProc = new ProcessControlPanel(
      DialogType.PRE_PROCESSING);
  private ProcessControlPanel procCtlCoarseAlign = new ProcessControlPanel(
      DialogType.COARSE_ALIGNMENT);
  private ProcessControlPanel procCtlFiducialModel = new ProcessControlPanel(
      DialogType.FIDUCIAL_MODEL);
  private ProcessControlPanel procCtlFineAlignment = new ProcessControlPanel(
      DialogType.FINE_ALIGNMENT);
  private ProcessControlPanel procCtlTomogramPositioning = new ProcessControlPanel(
      DialogType.TOMOGRAM_POSITIONING);
  private ProcessControlPanel procCtlTomogramGeneration = new ProcessControlPanel(
      DialogType.TOMOGRAM_GENERATION);
  private ProcessControlPanel procCtlTomogramCombination = new ProcessControlPanel(
      DialogType.TOMOGRAM_COMBINATION);
  private ProcessControlPanel procCtlPostProcessing = new ProcessControlPanel(
      DialogType.POST_PROCESSING);
  private JButton btnBothAxis = new JButton("Both");
  private JButton btnOtherAxis = null;
  
  private ApplicationManager applicationManager;

  /**
   * @param appManager
   * @param axis
   */
  public TomogramProcessPanel(ApplicationManager appManager, AxisID axis) {
    super(axis);
    applicationManager = appManager;
    //  Create the process control panel    
    createProcessControlPanel();
    initializePanels();
  }
  
  /**
   * 
   * @param event
   */
  protected void buttonKillAction(ActionEvent event) {
    applicationManager.kill(axisID);
  }
    
  /**
   * Invoke the appropriate ApplicationManager method for the button press
   */
  private void buttonProcessAction(ActionEvent event) {
    String command = event.getActionCommand();
    applicationManager.saveCurrentDialog(axisID);
    ProcessControlPanel currentProcess = null;
    
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

  protected void createProcessControlPanel() {
    super.createProcessControlPanel();
    //  Bind each button to action listener and the generic mouse listener
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    ProcessButtonActionListener buttonListener = new ProcessButtonActionListener(
        this);
    setToolTipText();
    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y5));
    JPanel axisButtonPanel = new JPanel();
    axisButtonPanel.setLayout(new BoxLayout(axisButtonPanel,
        BoxLayout.X_AXIS));
    if (axisID == AxisID.FIRST) {
      btnOtherAxis = new JButton("Axis B");
    }
    else if (axisID == AxisID.SECOND) {
      btnOtherAxis = new JButton("Axis A");
    }
    if (btnOtherAxis != null ) {
      axisButtonPanel.add(btnBothAxis);
      axisButtonPanel.add(Box.createRigidArea(FixedDim.x40_y0));
      axisButtonPanel.add(btnOtherAxis);
      panelProcessSelect.add(axisButtonPanel);
    }
    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y5));
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
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    text = "Open the Pre-processing panel to erase x-rays, bad pixels and/or bad"
        + " CCD rows from the raw projection stack.";
    procCtlPreProc.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Open the Coarse Alignment panel to generate a coarsely aligned "
        + "stack using cross correlation and to fix coarse alignment problems "
        + "with Midas.";
    procCtlCoarseAlign.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Open the Fiducial Model Generation panel to create a fiducial "
        + "model to be used in the fine alignment step.";
    procCtlFiducialModel
        .setToolTipText(tooltipFormatter.setText(text).format());

    text = "Open the Fine Alignment panel to use the generated fiducial model to "
        + "sub-pixel align the project sequence.";
    procCtlFineAlignment
        .setToolTipText(tooltipFormatter.setText(text).format());

    text = "Open the Tomogram Position panel to optimally adjust the 3D location "
        + "and size of the reconstruction volume.";
    procCtlTomogramPositioning.setToolTipText(tooltipFormatter.setText(text)
        .format());

    text = "Open the Tomogram Generation panel to generate the final aligned "
        + "stack and calcuate the tomographic reconstruction.";
    procCtlTomogramGeneration.setToolTipText(tooltipFormatter.setText(text)
        .format());

    text = "Open the Tomogram Combination panel to combine the tomograms generated "
        + "from the A and B axes into a single dual axis reconstruction.";
    procCtlTomogramCombination.setToolTipText(tooltipFormatter.setText(text)
        .format());

    text = "Open the Post Processing panel to trim the final reconstruction to size"
        + " and delete the intermediate files.";
    procCtlPostProcessing.setToolTipText(tooltipFormatter.setText(text)
        .format());
  }

  /**
   *   Action listeners to handle process panel events
   */
  class ProcessButtonActionListener implements ActionListener {
    TomogramProcessPanel adaptee;

    ProcessButtonActionListener(TomogramProcessPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonProcessAction(event);
    }
  }

}