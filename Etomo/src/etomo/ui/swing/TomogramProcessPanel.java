package etomo.ui.swing;

import java.awt.Color;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.process.ProcessState;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.util.Utilities;

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
 * <p> Revision 1.3  2011/09/10 03:42:49  sueh
 * <p> Bug# 1441 In buttonProcessAction printing an action.
 * <p>
 * <p> Revision 1.2  2011/02/22 21:40:59  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.31  2010/07/02 03:20:01  sueh
 * <p> bug# 1388 Constructing super class with popupChunkWarnings equal to true.
 * <p>
 * <p> Revision 1.30  2009/01/20 20:32:42  sueh
 * <p> bug# 1102 Changed buttons to type SimpleButton and naming them.
 * <p>
 * <p> Revision 1.29  2008/10/16 22:33:46  sueh
 * <p> bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 * <p>
 * <p> Revision 1.28  2008/01/14 22:06:48  sueh
 * <p> bug# 1050 Moved string "Axis B" to TomogramProcessPanel.
 * <p>
 * <p> Revision 1.27  2007/09/07 00:29:35  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.26  2007/02/09 00:54:33  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.25  2006/08/08 21:16:47  sueh
 * <p> bug# 438 Correct compact display.
 * <p>
 * <p> Revision 1.24  2006/07/31 21:46:50  sueh
 * <p> bug# 438 Fixing the size of the axis buttons.  Stacking the axis buttons for a
 * <p> compact display
 * <p>
 * <p> Revision 1.23  2006/07/26 16:43:50  sueh
 * <p> bug# 868 Moved functions associated with TomogramGenerationDialog from
 * <p> ApplicationManager to TomogramGenerationExpert.
 * <p>
 * <p> Revision 1.22  2006/05/19 19:52:42  sueh
 * <p> bug# 866 Opening tomo pos using a UIExpert class.
 * <p>
 * <p> Revision 1.21  2006/04/25 19:27:04  sueh
 * <p>  bug# 787 Made the both axis label public.
 * <p>
 * <p> Revision 1.20  2006/04/07 23:32:47  sueh
 * <p> bug# 846 Changing the background colors for java 1.5.
 * <p>
 * <p> Revision 1.19  2006/04/06 23:35:20  sueh
 * <p> bug# 844 Added a color for the single axis reconstruction window.
 * <p>
 * <p> Revision 1.18  2005/11/14 22:29:45  sueh
 * <p> bug# 762 Made buttonAxisAction() and stateChanged() protected.
 * <p>
 * <p> Revision 1.17  2005/09/22 21:33:33  sueh
 * <p> bug# 532 Moved the parallel process panel to AxisProcessPanel.
 * <p>
 * <p> Revision 1.16  2005/06/17 19:18:26  sueh
 * <p> bug# 685 Put all timestamp functionality into one function.  Added
 * <p> buttonTimestamp to provide an interface to the main timestamp function.
 * <p>
 * <p> Revision 1.15  2005/06/17 00:34:58  sueh
 * <p> bug# 685 Timestamped process panel button presses.
 * <p>
 * <p> Revision 1.14  2005/05/17 19:41:07  sueh
 * <p> bug# 615 Removed unnecessary imports.
 * <p>
 * <p> Revision 1.13  2005/05/13 17:46:51  sueh
 * <p> bug# 658 Setting the size of axis buttons based on text extent.  Hopefully
 * <p> this will fix it on mac.
 * <p>
 * <p> Revision 1.12  2005/04/26 17:42:29  sueh
 * <p> bug# 615 Made MainFrame a package-level class.  All MainFrame
 * <p> functionality is handled through UIHarness to make Etomo more
 * <p> compatible with JUnit.
 * <p>
 * <p> Revision 1.11  2005/04/21 20:55:35  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 1.10  2005/04/16 02:08:02  sueh
 * <p> bug# 615 Setting panel background colors.  Setting axis buttons to be
 * <p> A and B when both axis are displayed.  Setting axis buttons to be the
 * <p> other axis and Both when only one axis is displayed.
 * <p>
 * <p> Revision 1.9  2005/04/01 00:17:06  sueh
 * <p> bug# 622 Adding showAxisA, B, and Both.  Need to change the axis
 * <p> button names when switching axis.
 * <p>
 * <p> Revision 1.8  2005/03/30 23:46:01  sueh
 * <p> bug# 622 Adding actions for the axis buttons.
 * <p>
 * <p> Revision 1.7  2005/03/30 21:43:30  sueh
 * <p> bug# 622 Temporarily hiding working with newstuff flag.
 * <p>
 * <p> Revision 1.6  2005/03/24 17:55:06  sueh
 * <p> bug# 621 Added a button for the clean up dialog and reduced the space
 * <p> between buttons.
 * <p>
 * <p> Revision 1.5  2005/03/21 18:34:52  sueh
 * <p> bug# 622 Adding buttons to change axis.
 * <p>
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

  public static final String BOTH_AXIS_LABEL = "Both";
  static final String AXIS_B_LABEL = "Axis B";
  private static final String AXIS_A_LABEL = "Axis A";

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
  private ProcessControlPanel procCtlFinalAlignedStack = new ProcessControlPanel(
      DialogType.FINAL_ALIGNED_STACK);
  private ProcessControlPanel procCtlTomogramGeneration = new ProcessControlPanel(
      DialogType.TOMOGRAM_GENERATION);
  private ProcessControlPanel procCtlTomogramCombination = new ProcessControlPanel(
      DialogType.TOMOGRAM_COMBINATION);
  private ProcessControlPanel procCtlPostProcessing = new ProcessControlPanel(
      DialogType.POST_PROCESSING);
  private ProcessControlPanel procCtlCleanUp = new ProcessControlPanel(
      DialogType.CLEAN_UP);
  private SimpleButton axisButton1 = new SimpleButton();
  private SimpleButton axisButton2 = new SimpleButton();
  JPanel axisButtonPanel = new JPanel();
  private String bothAxisTooltip = null;
  private String axisATooltip = null;
  private String axisBTooltip = null;
  private UIHarness uiHarness = UIHarness.INSTANCE;
  private final ApplicationManager applicationManager;

  // private int buttonWidth = 0;
  // private int buttonHeight = 0;

  /**
   * @param appManager
   * @param axis
   */
  public TomogramProcessPanel(ApplicationManager appManager, AxisID axis) {
    super(axis, appManager, true);
    applicationManager = (ApplicationManager) manager;
    // Create the process control panel
    createProcessControlPanel();
    initializePanels();
  }

  protected void buttonAxisAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(BOTH_AXIS_LABEL)) {
      uiHarness.showBothAxis();
    }
    else if (command.equals(AXIS_A_LABEL)) {
      uiHarness.showAxisA();
    }
    else if (command.equals(AXIS_B_LABEL)) {
      uiHarness.showAxisB();
    }
  }

  /**
   * Invoke the appropriate ApplicationManager method for the button press
   */
  protected void buttonProcessAction(ActionEvent event) {
    String command = event.getActionCommand();
    Utilities.buttonTimestamp(command);
    applicationManager.saveCurrentDialog(axisID);
    if (command.equals(procCtlPreProc.getCommand())) {
      applicationManager.openPreProcDialog(axisID);
    }
    else if (command.equals(procCtlCoarseAlign.getCommand())) {
      applicationManager.openCoarseAlignDialog(axisID);
    }
    else if (command.equals(procCtlFiducialModel.getCommand())) {
      applicationManager.openFiducialModelDialog(axisID);
    }
    else if (command.equals(procCtlFineAlignment.getCommand())) {
      applicationManager.openFineAlignmentDialog(axisID);
    }
    else if (command.equals(procCtlTomogramPositioning.getCommand())) {
      ((TomogramPositioningExpert) applicationManager.getUIExpert(
          DialogType.TOMOGRAM_POSITIONING, axisID)).openDialog();
    }
    else if (command.equals(procCtlFinalAlignedStack.getCommand())) {
      ((FinalAlignedStackExpert) applicationManager.getUIExpert(
          DialogType.FINAL_ALIGNED_STACK, axisID)).openDialog();
    }
    else if (command.equals(procCtlTomogramGeneration.getCommand())) {
      ((TomogramGenerationExpert) applicationManager.getUIExpert(
          DialogType.TOMOGRAM_GENERATION, axisID)).openDialog();
    }
    else if (command.equals(procCtlTomogramCombination.getCommand())) {
      applicationManager.openTomogramCombinationDialog();
    }
    else if (command.equals(procCtlPostProcessing.getCommand())) {
      applicationManager.openPostProcessingDialog();
    }
    else if (command.equals(procCtlCleanUp.getCommand())) {
      applicationManager.openCleanUpDialog();
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
  public void setFinalAlignedStackState(ProcessState state) {
    procCtlFinalAlignedStack.setState(state);
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
   * 
   * @param state
   */
  public void setCleanUpState(ProcessState state) {
    procCtlCleanUp.setState(state);
  }

  void showBothAxis() {
    if (axisID == AxisID.FIRST) {
      showAxisA(true);
    }
    else if (axisID == AxisID.SECOND) {
      showAxisB(true);
    }
  }

  private void showAxisOnly() {
    setBackground(Colors.getBackgroundA());
  }

  private void showAxisA(boolean showingBothAxis) {
    if (axisID != AxisID.FIRST) {
      throw new IllegalStateException("Function should only be called for A axis panel.");
    }
    setBackground(Colors.getBackgroundA());
    if (showingBothAxis) {
      setButton(axisButton1, AXIS_A_LABEL, axisATooltip);
      setButton(axisButton2, AXIS_B_LABEL, axisBTooltip);
    }
    else {
      setButton(axisButton1, AXIS_B_LABEL, axisBTooltip);
      setButton(axisButton2, BOTH_AXIS_LABEL, bothAxisTooltip);
    }
    axisButtonPanel.setVisible(true);
  }

  void showAxisA() {
    showAxisA(false);
  }

  private void showAxisB(boolean showingBothAxis) {
    if (axisID != AxisID.SECOND) {
      throw new IllegalStateException("Function should only be called for B axis panel.");
    }
    setBackground(Colors.getBackgroundB());
    if (showingBothAxis) {
      axisButtonPanel.setVisible(false);
    }
    else {
      setButton(axisButton1, AXIS_A_LABEL, axisATooltip);
      setButton(axisButton2, BOTH_AXIS_LABEL, bothAxisTooltip);
      axisButtonPanel.setVisible(true);
    }

  }

  void showAxisB() {
    showAxisB(false);
  }

  /**
   * Call parent function and sets axis button panel.
   */
  void setBackground(Color color) {
    super.setBackground(color);
    axisButtonPanel.setBackground(color);
  }

  private void setButton(SimpleButton button, String label, String tooltip) {
    button.setText(label);
    /* Rectangle2D buttonSize = button.getFontMetrics(button.getFont())
     * .getStringBounds(AXIS_A_LABEL.toCharArray(), 0, label.length(),
     * button.getGraphics()); System.out.println("buttonSize="+buttonSize); if
     * (buttonWidth < buttonSize.getWidth()) { buttonWidth = (int) buttonSize.getWidth();
     * } if (buttonHeight <buttonSize.getHeight()) { buttonHeight = (int)
     * buttonSize.getHeight(); }
     * System.out.println("buttonWidth="+buttonWidth+",buttonHeight="+buttonHeight);
     * button.setSize(buttonWidth, buttonHeight); */
    button.setPreferredSize(UIParameters.INSTANCE.getAxisButtonDimension());
    button.setMaximumSize(UIParameters.INSTANCE.getAxisButtonDimension());
    button.setToolTipText(tooltip);
  }

  protected void createProcessControlPanel() {
    super.createProcessControlPanel();
    // Bind each button to action listener and the generic mouse listener
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    ProcessButtonActionListener buttonListener = new ProcessButtonActionListener(this);
    AxisButtonActionListener axisButtonListener = new AxisButtonActionListener(this);
    setToolTipText();
    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y5));
    int layoutAxis = BoxLayout.X_AXIS;
    boolean compactDisplay = EtomoDirector.INSTANCE.getUserConfiguration()
        .getCompactDisplay();
    if (compactDisplay) {
      layoutAxis = BoxLayout.Y_AXIS;
    }
    axisButtonPanel.setLayout(new BoxLayout(axisButtonPanel, layoutAxis));
    if (axisID == AxisID.ONLY) {
      showAxisOnly();
    }
    else {
      axisButton1.addActionListener(axisButtonListener);
      axisButton2.addActionListener(axisButtonListener);
      axisButtonPanel.add(axisButton1);
      if (compactDisplay) {
        axisButtonPanel.add(Box.createRigidArea(FixedDim.x0_y5));
      }
      else {
        axisButtonPanel.add(Box.createRigidArea(FixedDim.x40_y0));
      }
      axisButtonPanel.add(axisButton2);
      axisButtonPanel.setAlignmentX(Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(axisButtonPanel);
      if (axisID == AxisID.FIRST) {
        showAxisA();
      }
    }
    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
    procCtlPreProc.setButtonActionListener(buttonListener);
    procCtlPreProc.addMouseListener(mouseAdapter);
    procCtlPreProc.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlPreProc.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
    procCtlCoarseAlign.addMouseListener(mouseAdapter);
    procCtlCoarseAlign.setButtonActionListener(buttonListener);
    procCtlCoarseAlign.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlCoarseAlign.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
    procCtlFiducialModel.addMouseListener(mouseAdapter);
    procCtlFiducialModel.setButtonActionListener(buttonListener);
    procCtlFiducialModel.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlFiducialModel.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
    procCtlFineAlignment.addMouseListener(mouseAdapter);
    procCtlFineAlignment.setButtonActionListener(buttonListener);
    procCtlFineAlignment.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlFineAlignment.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
    procCtlTomogramPositioning.addMouseListener(mouseAdapter);
    procCtlTomogramPositioning.setButtonActionListener(buttonListener);
    procCtlTomogramPositioning.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlTomogramPositioning.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
    procCtlFinalAlignedStack.addMouseListener(mouseAdapter);
    procCtlFinalAlignedStack.setButtonActionListener(buttonListener);
    procCtlFinalAlignedStack.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlFinalAlignedStack.getContainer());

    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
    procCtlTomogramGeneration.addMouseListener(mouseAdapter);
    procCtlTomogramGeneration.setButtonActionListener(buttonListener);
    procCtlTomogramGeneration.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
    panelProcessSelect.add(procCtlTomogramGeneration.getContainer());

    if (axisID == AxisID.FIRST) {
      panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
      procCtlTomogramCombination.addMouseListener(mouseAdapter);
      procCtlTomogramCombination.setButtonActionListener(buttonListener);
      procCtlTomogramCombination.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(procCtlTomogramCombination.getContainer());
    }
    if (axisID != AxisID.SECOND) {
      panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
      procCtlPostProcessing.addMouseListener(mouseAdapter);
      procCtlPostProcessing.setButtonActionListener(buttonListener);
      procCtlPostProcessing.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(procCtlPostProcessing.getContainer());

      panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
      procCtlCleanUp.addMouseListener(mouseAdapter);
      procCtlCleanUp.setButtonActionListener(buttonListener);
      procCtlCleanUp.getContainer().setAlignmentX(Container.CENTER_ALIGNMENT);
      panelProcessSelect.add(procCtlCleanUp.getContainer());
    }
    panelProcessSelect.add(Box.createRigidArea(FixedDim.x0_y10));
  }

  /**
   * Select the requested button
   * @param name
   */
  public void selectButton(String name) {
    unSelectAll();
    if (name.equals(procCtlPreProc.getCommand())) {
      procCtlPreProc.setSelected(true);
      return;
    }
    if (name.equals(procCtlCoarseAlign.getCommand())) {
      procCtlCoarseAlign.setSelected(true);
      return;
    }
    if (name.equals(procCtlFiducialModel.getCommand())) {
      procCtlFiducialModel.setSelected(true);
      return;
    }
    if (name.equals(procCtlFineAlignment.getCommand())) {
      procCtlFineAlignment.setSelected(true);
      return;
    }
    if (name.equals(procCtlTomogramPositioning.getCommand())) {
      procCtlTomogramPositioning.setSelected(true);
      return;
    }
    if (name.equals(procCtlFinalAlignedStack.getCommand())) {
      procCtlFinalAlignedStack.setSelected(true);
      return;
    }
    if (name.equals(procCtlTomogramGeneration.getCommand())) {
      procCtlTomogramGeneration.setSelected(true);
      return;
    }
    if (name.equals(procCtlTomogramCombination.getCommand())) {
      procCtlTomogramCombination.setSelected(true);
      return;
    }
    if (name.equals(procCtlPostProcessing.getCommand())) {
      procCtlPostProcessing.setSelected(true);
      return;
    }
    if (name.equals(procCtlCleanUp.getCommand())) {
      procCtlCleanUp.setSelected(true);
      return;
    }
  }

  private void unSelectAll() {
    procCtlPreProc.setSelected(false);
    procCtlCoarseAlign.setSelected(false);
    procCtlFiducialModel.setSelected(false);
    procCtlFineAlignment.setSelected(false);
    procCtlTomogramPositioning.setSelected(false);
    procCtlFinalAlignedStack.setSelected(false);
    procCtlTomogramGeneration.setSelected(false);
    procCtlTomogramCombination.setSelected(false);
    procCtlPostProcessing.setSelected(false);
    procCtlCleanUp.setSelected(false);
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    bothAxisTooltip = TooltipFormatter.INSTANCE.format("See Axis A and B.");
    axisATooltip = TooltipFormatter.INSTANCE.format("See Axis A only.");
    axisBTooltip = TooltipFormatter.INSTANCE.format("See Axis B only.");
    procCtlPreProc
        .setToolTipText("Open the Pre-processing panel to erase x-rays, bad pixels and/or bad"
            + " CCD rows from the raw projection stack.");
    procCtlCoarseAlign
        .setToolTipText("Open the Coarse Alignment panel to generate a coarsely aligned "
            + "stack using cross correlation and to fix coarse alignment problems "
            + "with Midas.");
    procCtlFiducialModel
        .setToolTipText("Open the Fiducial Model Generation panel to create a fiducial "
            + "model to be used in the fine alignment step.");
    procCtlFineAlignment
        .setToolTipText("Open the Fine Alignment panel to use the generated fiducial model to "
            + "sub-pixel align the project sequence.");
    procCtlTomogramPositioning
        .setToolTipText("Open the Tomogram Position panel to optimally adjust the 3D location "
            + "and size of the reconstruction volume.");
    procCtlFinalAlignedStack
        .setToolTipText("Open the Final Aligned Stack panel to generate the final aligned stack.");
    procCtlTomogramGeneration
        .setToolTipText("Open the Tomogram Generation panel to calcuate the tomographic reconstruction.");
    procCtlTomogramCombination
        .setToolTipText("Open the Tomogram Combination panel to combine the tomograms generated "
            + "from the A and B axes into a single dual axis reconstruction.");
    procCtlPostProcessing
        .setToolTipText("Open the Post Processing panel to trim the final reconstruction to size"
            + " and squeeze the final reconstruction volume.");
    procCtlCleanUp
        .setToolTipText("Open the Clean Up panel to delete the intermediate files.");
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

  class AxisButtonActionListener implements ActionListener {
    TomogramProcessPanel adaptee;

    AxisButtonActionListener(TomogramProcessPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAxisAction(event);
    }
  }

}