/**
 * <p>Description: Panel to modify the CCD eraser parameters</p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 */

package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ConstCCDEraserParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;

public class CCDEraserPanel implements ContextMenu, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager;
  private AxisID axisID;

  private JPanel pnlCCDEraser = new JPanel();
  private JPanel pnlXRayReplacement = new JPanel();
  private JPanel pnlXRayButtons = new JPanel();
  private JPanel pnlManualReplacement = new JPanel();
  private JPanel pnlManualButtons = new JPanel();
  private JPanel pnlEraseButtons = new JPanel();

  private CheckBox cbXrayReplacement = new CheckBox(
      "Automatic x-ray replacement");
  private LabeledTextField ltfPeakCriterion = new LabeledTextField(
      "Peak criterion:");
  private LabeledTextField ltfDiffCriterion = new LabeledTextField(
      "Difference criterion:");
  private LabeledTextField ltfGrowCriterion = new LabeledTextField(
      "Grow criterion:");
  private LabeledTextField ltfEdgeExclusion = new LabeledTextField(
      "Edge exclusion:");
  private LabeledTextField ltfMaximumRadius = new LabeledTextField(
      "Maximum radius:");
  private LabeledTextField ltfAnnulusWidth = new LabeledTextField(
      "Annulus width:");
  private LabeledTextField ltfScanRegionSize = new LabeledTextField(
      "XY scan size:");
  private LabeledTextField ltfScanCriterion = new LabeledTextField(
      "Scan criterion:");
  private final MultiLineButton btnFindXRays;
  private Run3dmodButton btnViewXRayModel = new Run3dmodButton(
      "<html><b>View X-ray Model</b>", this);

  private CheckBox cbManualReplacement = new CheckBox("Manual replacement");
  private LabeledTextField ltfGlobalReplacementList = new LabeledTextField(
      "All section replacement list: ");
  private LabeledTextField ltfLocalReplacementList = new LabeledTextField(
      "Line replacement list: ");
  private LabeledTextField ltfBoundaryReplacementList = new LabeledTextField(
  "Boundary replacement list: ");
  private Run3dmodButton btnCreateModel = new Run3dmodButton(
      "<html><b>Create Manual Replacement Model</b>", this);

  private LabeledTextField ltfInputImage = new LabeledTextField("Input file: ");
  private LabeledTextField ltfOutputImage = new LabeledTextField(
      "Output file: ");
  private LabeledTextField ltfBorderPixels = new LabeledTextField(
      "Border pixels: ");
  private LabeledTextField ltfPolynomialOrder = new LabeledTextField(
      "Polynomial order: ");
  private CheckBox cbIncludeAdjacentPoints = new CheckBox(
      "Include adjacent points");

  private final MultiLineButton btnErase;
  private Run3dmodButton btnViewErased = new Run3dmodButton(
      "<html><b>View Fixed Stack</b>", this);
  private final MultiLineButton btnReplaceRawStack;
  private final DialogType dialogType;
  private final CCDEraserActionListener ccdEraserActionListener;

  /**
   * Default constructor
   */
  public CCDEraserPanel(ApplicationManager appMgr, AxisID id,
      DialogType dialogType) {
    applicationManager = appMgr;
    axisID = id;
    this.dialogType = dialogType;
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnFindXRays = (MultiLineButton) displayFactory.getFindXRays();
    btnErase = (MultiLineButton) displayFactory.getCreateFixedStack();
    btnReplaceRawStack = (MultiLineButton) displayFactory.getUseFixedStack();
    setToolTipText();

    pnlXRayReplacement.setLayout(new BoxLayout(pnlXRayReplacement,
        BoxLayout.Y_AXIS));
    pnlXRayReplacement
        .setBorder(new EtchedBorder("Automatic X-ray Replacement").getBorder());

    UIUtilities.addWithYSpace(pnlXRayReplacement, cbXrayReplacement);
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfPeakCriterion
        .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfDiffCriterion
        .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfGrowCriterion
        .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfEdgeExclusion
        .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfMaximumRadius
        .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfAnnulusWidth
        .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfScanRegionSize
        .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfScanCriterion
        .getContainer());

    pnlXRayButtons.setLayout(new BoxLayout(pnlXRayButtons, BoxLayout.X_AXIS));
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnFindXRays.getComponent());
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnViewXRayModel.getComponent());
    pnlXRayButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlXRayButtons, UIParameters.INSTANCE
        .getButtonDimension());

    UIUtilities.addWithYSpace(pnlXRayReplacement, pnlXRayButtons);

    pnlManualReplacement.setLayout(new BoxLayout(pnlManualReplacement,
        BoxLayout.Y_AXIS));
    pnlManualReplacement.setBorder(new EtchedBorder(
        "Manual Pixel Region Replacement").getBorder());
    UIUtilities.addWithYSpace(pnlManualReplacement, cbManualReplacement);
    UIUtilities.addWithYSpace(pnlManualReplacement, ltfGlobalReplacementList
        .getContainer());
    UIUtilities.addWithYSpace(pnlManualReplacement, ltfLocalReplacementList
        .getContainer());
    UIUtilities.addWithYSpace(pnlManualReplacement, ltfBoundaryReplacementList
        .getContainer());

    pnlManualButtons
        .setLayout(new BoxLayout(pnlManualButtons, BoxLayout.X_AXIS));
    pnlManualButtons.add(Box.createHorizontalGlue());
    pnlManualButtons.add(btnCreateModel.getComponent());
    pnlManualButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlManualButtons, UIParameters.INSTANCE
        .getButtonDimension());

    UIUtilities.addWithYSpace(pnlManualReplacement, pnlManualButtons);

    pnlCCDEraser.setLayout(new BoxLayout(pnlCCDEraser, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlCCDEraser, pnlXRayReplacement);
    UIUtilities.addWithYSpace(pnlCCDEraser, pnlManualReplacement);
    UIUtilities.addWithYSpace(pnlCCDEraser, ltfInputImage.getContainer());
    UIUtilities.addWithYSpace(pnlCCDEraser, ltfOutputImage.getContainer());
    UIUtilities.addWithYSpace(pnlCCDEraser, ltfBorderPixels.getContainer());
    UIUtilities.addWithYSpace(pnlCCDEraser, ltfPolynomialOrder.getContainer());
    UIUtilities.addWithYSpace(pnlCCDEraser, cbIncludeAdjacentPoints);

    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlEraseButtons.setLayout(new BoxLayout(pnlEraseButtons, BoxLayout.X_AXIS));
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnErase.getComponent());
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnViewErased.getComponent());
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnReplaceRawStack.getComponent());
    pnlEraseButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlEraseButtons, UIParameters.INSTANCE
        .getButtonDimension());

    UIUtilities.addWithYSpace(pnlCCDEraser, pnlEraseButtons);

    // Bind the buttons to the action listener
    ccdEraserActionListener = new CCDEraserActionListener(this);
    btnFindXRays.addActionListener(ccdEraserActionListener);
    btnViewXRayModel.addActionListener(ccdEraserActionListener);
    btnCreateModel.addActionListener(ccdEraserActionListener);
    btnErase.addActionListener(ccdEraserActionListener);
    btnViewErased.addActionListener(ccdEraserActionListener);
    btnReplaceRawStack.addActionListener(ccdEraserActionListener);
    cbXrayReplacement.addActionListener(ccdEraserActionListener);
    cbManualReplacement.addActionListener(ccdEraserActionListener);

    // Left align  all of the compenents in each panel and center align the
    // panel
    UIUtilities.alignComponentsX(pnlXRayReplacement, Component.LEFT_ALIGNMENT);
    UIUtilities
        .alignComponentsX(pnlManualReplacement, Component.LEFT_ALIGNMENT);
    UIUtilities.alignComponentsX(pnlCCDEraser, Component.LEFT_ALIGNMENT);
    pnlCCDEraser.setAlignmentX(Component.CENTER_ALIGNMENT);

    // Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlCCDEraser.addMouseListener(mouseAdapter);

    enableXRayReplacement();
    enableManualReplacement();
  }

  public static ProcessResultDisplay getFindXRaysDisplay(DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance("Find X-rays (Trial Mode)",
        dialogType);
  }

  public static ProcessResultDisplay getCreateFixedStackDisplay(DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance(
        "Create Fixed Stack", dialogType);
  }

  public static ProcessResultDisplay getUseFixedStackDisplay(
      DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance("Use Fixed Stack",
        dialogType);
  }

  /**
   * Set the fields
   * @param ccdEraserParams
   */
  public void setParameters(ConstCCDEraserParam ccdEraserParams) {
    ltfInputImage.setText(ccdEraserParams.getInputFile());
    ltfOutputImage.setText(ccdEraserParams.getOutputFile());

    cbXrayReplacement.setSelected(ccdEraserParams.isFindPeaks());
    ltfPeakCriterion.setText(ccdEraserParams.getPeakCriterion());
    ltfDiffCriterion.setText(ccdEraserParams.getDiffCriterion());
    ltfGrowCriterion.setText(ccdEraserParams.getGrowCriterion());
    ltfScanCriterion.setText(ccdEraserParams.getScanCriterion());
    ltfMaximumRadius.setText(ccdEraserParams.getMaximumRadius());
    ltfAnnulusWidth.setText(ccdEraserParams.getAnnulusWidth());
    ltfScanRegionSize.setText(ccdEraserParams.getXyScanSize());
    ltfEdgeExclusion.setText(ccdEraserParams.getEdgeExclusion());

    cbManualReplacement.setSelected(!ccdEraserParams.getModelFile().equals(""));
    ltfGlobalReplacementList
        .setText(ccdEraserParams.getGlobalReplacementList());
    ltfLocalReplacementList.setText(ccdEraserParams.getlocalReplacementList());
    ltfBoundaryReplacementList.setText(ccdEraserParams.getBoundaryReplacementList());
    ltfBorderPixels.setText(ccdEraserParams.getBorderPixels());
    ltfPolynomialOrder.setText(ccdEraserParams.getPolynomialOrder());
    cbIncludeAdjacentPoints.setSelected(ccdEraserParams
        .getIncludeAdjacentPoints());

    enableXRayReplacement();
    enableManualReplacement();
  }

  public void done() {
    btnFindXRays.removeActionListener(ccdEraserActionListener);
    btnErase.removeActionListener(ccdEraserActionListener);
    btnReplaceRawStack.removeActionListener(ccdEraserActionListener);
  }

  public final void setParameters(ReconScreenState screenState) {
    //btnReplaceRawStack.setButtonState(screenState
     //   .getButtonState(btnReplaceRawStack.getButtonStateKey()));
    //btnErase.setButtonState(screenState.getButtonState(btnErase
    //    .getButtonStateKey()));
    //btnFindXRays.setButtonState(screenState.getButtonState(btnFindXRays
    //    .getButtonStateKey()));
  }

  public void getParameters(CCDEraserParam ccdEraserParams) {
    ccdEraserParams.setFindPeaks(cbXrayReplacement.isSelected());
    ccdEraserParams.setPeakCriterion(ltfPeakCriterion.getText());
    ccdEraserParams.setDiffCriterion(ltfDiffCriterion.getText());
    ccdEraserParams.setGrowCriterion(ltfGrowCriterion.getText());
    ccdEraserParams.setScanCriterion(ltfScanCriterion.getText());
    ccdEraserParams.setMaximumRadius(ltfMaximumRadius.getText());
    ccdEraserParams.setAnnulusWidth(ltfAnnulusWidth.getText());
    ccdEraserParams.setXyScanSize(ltfScanRegionSize.getText());
    ccdEraserParams.setEdgeExclusion(ltfEdgeExclusion.getText());
    ccdEraserParams.setInputFile(ltfInputImage.getText());
    ccdEraserParams.setOutputFile(ltfOutputImage.getText());
    ccdEraserParams
        .setGlobalReplacementList(ltfGlobalReplacementList.getText());
    ccdEraserParams.setLocalReplacementList(ltfLocalReplacementList.getText());
    ccdEraserParams.setBoundaryReplacementList(ltfBoundaryReplacementList.getText());
    ccdEraserParams.setBorderPixels(ltfBorderPixels.getText());
    ccdEraserParams.setPolynomialOrder(ltfPolynomialOrder.getText());
    ccdEraserParams.setIncludeAdjacentPoints(cbIncludeAdjacentPoints
        .isSelected());
    if (cbManualReplacement.isSelected()) {
      ccdEraserParams.setModelFile(applicationManager.getMetaData()
          .getDatasetName()
          + axisID.getExtension() + ".erase");
    }
    else {
      ccdEraserParams.setModelFile("");
    }
  }

  /**
   * Return the container of panel
   * @return
   */
  public JPanel getContainer() {
    return pnlCCDEraser;
  }

  /**
   * Set the visibility state of the panel
   * @param state
   */
  public void setVisible(boolean state) {
    pnlCCDEraser.setVisible(state);
  }

  /**
   * Makes the advanced components visible or invisible
   * @param state
   */
  void setAdvanced(boolean state) {
    cbXrayReplacement.setVisible(state);
    ltfGrowCriterion.setVisible(state);
    ltfEdgeExclusion.setVisible(state);
    ltfAnnulusWidth.setVisible(state);
    ltfScanRegionSize.setVisible(state);
    ltfScanCriterion.setVisible(state);
    pnlManualReplacement.setVisible(state);
    ltfBorderPixels.setVisible(state);
    ltfPolynomialOrder.setVisible(state);
    cbIncludeAdjacentPoints.setVisible(state);
    ltfInputImage.setVisible(state);
    ltfOutputImage.setVisible(state);
  }

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }

  private void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnViewXRayModel.getActionCommand())) {
      applicationManager.imodXrayModel(axisID, menuOptions);
    }
    else if (command.equals(btnCreateModel.getActionCommand())) {
      applicationManager.imodManualErase(axisID, menuOptions);
    }
    else if (command.equals(btnViewErased.getActionCommand())) {
      applicationManager.imodErasedStack(axisID, menuOptions);
    }
  }

  //  Button action method
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(btnFindXRays.getActionCommand())) {
      applicationManager.findXrays(axisID, btnFindXRays);
    }
    else if (command.equals(btnErase.getActionCommand())) {
      applicationManager.preEraser(axisID, btnErase);
    }
    else if (command.equals(btnReplaceRawStack.getActionCommand())) {
      applicationManager.replaceRawStack(axisID, btnReplaceRawStack);
    }
    else if (command.equals(cbXrayReplacement.getActionCommand())) {
      enableXRayReplacement();
    }
    else if (command.equals(cbManualReplacement.getActionCommand())) {
      enableManualReplacement();
    }
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] label = { "CCDEraser" };
    String[] manPage = { "ccderaser.html" };

    String[] logFileLabel = { "Eraser" };
    String[] logFile = new String[1];
    logFile[0] = "eraser" + axisID.getExtension() + ".log";

    ContextPopup contextPopup = new ContextPopup(pnlCCDEraser, mouseEvent,
        "PRE-PROCESSING", ContextPopup.TOMO_GUIDE, label, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  private void enableXRayReplacement() {
    boolean state = cbXrayReplacement.isSelected();
    ltfPeakCriterion.setEnabled(state);
    ltfDiffCriterion.setEnabled(state);
    ltfGrowCriterion.setEnabled(state);
    ltfEdgeExclusion.setEnabled(state);
    ltfMaximumRadius.setEnabled(state);
    ltfAnnulusWidth.setEnabled(state);
    ltfScanRegionSize.setEnabled(state);
    ltfScanCriterion.setEnabled(state);
    btnFindXRays.setEnabled(state);
    btnViewXRayModel.setEnabled(state);
  }

  private void enableManualReplacement() {
    boolean state = cbManualReplacement.isSelected();
    ltfGlobalReplacementList.setEnabled(state);
    ltfLocalReplacementList.setEnabled(state);
    ltfBoundaryReplacementList.setEnabled(state);
    btnCreateModel.setEnabled(state);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.CCDERASER, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.ReadException except) {
      except.printStackTrace();
    }
    ltfInputImage.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.INPUT_FILE_KEY));
    ltfOutputImage.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.OUTPUT_FILE_KEY));
    cbXrayReplacement.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.FIND_PEAKS_KEY));
    ltfPeakCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.PEAK_CRITERION_KEY));
    ltfDiffCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.DIFF_CRITERION_KEY));
    ltfGrowCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.GROW_CRITERION_KEY));
    ltfScanCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.SCAN_CRITERION_KEY));
    ltfMaximumRadius.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.MAXIMUM_RADIUS_KEY));
    ltfAnnulusWidth.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.ANNULUS_WIDTH_KEY));
    ltfScanRegionSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.X_Y_SCAN_SIZE_KEY));
    ltfEdgeExclusion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.EDGE_EXCLUSION_WIDTH_KEY));
    ltfLocalReplacementList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.LINE_OBJECTS_KEY));
    ltfBoundaryReplacementList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.BOUNDARY_OBJECTS_KEY));
    cbManualReplacement
        .setToolTipText("Use a manually created model to specify regions and lines to replace.");
    ltfGlobalReplacementList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.ALL_SECTION_OBJECTS_KEY));
    ltfBorderPixels.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.BORDER_SIZE_KEY));
    ltfPolynomialOrder.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.POLYNOMIAL_ORDER_KEY));
    cbIncludeAdjacentPoints
        .setToolTipText("Include pixels adjacent to the patch being replaced in the pixels "
            + "being fit.");
    btnFindXRays.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.TRIAL_MODE_KEY));
    btnViewXRayModel
        .setToolTipText("View the x-ray model on the raw stack in 3dmod.");
    btnCreateModel
        .setToolTipText("Create a manual replacement model using 3dmod.");
    btnErase
        .setToolTipText("Run ccderaser, erasing the raw stack and writing the modified stack "
            + "to the output file specified (the default is *_fixed.st).  "
            + "NOTE: subsequent processing uses the "
            + "raw stack filename, therefore for ccderaser to have an effect on "
            + "your data you must commit the raw stack when you are satisfied with"
            + " your ccderaser output stack.");
    btnViewErased.setToolTipText("View the erased stack in 3dmod.");
    btnReplaceRawStack
        .setToolTipText("Use the raw stack with the output from ccderaser.  "
            + "NOTE: subsequent processing uses the "
            + "raw stack filename, therefore for ccderaser to have an effect on "
            + "your data you must commit the raw stack when you are satisfied with"
            + " your ccderaser output stack.");
  }

  //  Action listener
  class CCDEraserActionListener implements ActionListener {

    CCDEraserPanel adaptee;

    CCDEraserActionListener(CCDEraserPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }
}

/**
 * <p> $Log$
 * <p> Revision 3.24  2007/09/10 20:42:06  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.23  2007/03/21 19:45:01  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.22  2007/03/01 01:27:27  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.21  2007/02/09 00:47:31  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.20  2006/07/20 17:19:39  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 3.19  2006/06/21 15:50:08  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.18  2006/02/06 21:20:38  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.17  2006/01/26 22:03:43  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.16  2006/01/12 17:07:44  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.15  2006/01/03 23:30:01  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox
 * <p>
 * <p> Revision 3.14  2005/10/27 00:33:54  sueh
 * <p> bug# 725 Calling preEraser instead eraser so that the B stack can be
 * <p> processed.
 * <p>
 * <p> Revision 3.13  2005/08/27 22:35:26  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.12  2005/08/11 23:45:28  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 3.11  2005/08/10 20:40:19  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.10  2005/08/09 20:13:08  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.
 * <p>
 * <p> Revision 3.9  2005/04/25 20:53:48  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.8  2005/02/22 20:57:49  sueh
 * <p> bug# 600 Converting tooltips to autodoc.
 * <p>
 * <p> Revision 3.7  2004/12/02 20:37:08  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.6  2004/11/19 23:49:22  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.5.4.2  2004/10/11 02:10:20  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.5.4.1  2004/09/07 17:58:36  sueh
 * <p> bug# 520 getting dataset name from metadata
 * <p>
 * <p> Revision 3.5  2004/06/25 00:34:01  sueh
 * <p> bug# 467 Removing outerRadius, adding annulusWidth.
 * <p> Making maximumRadius a basic field.
 * <p>
 * <p> Revision 3.4  2004/04/21 17:06:17  rickg
 * <p> Bug #424 simplified panel layout using UIUtilities
 * <p>
 * <p> Revision 3.3  2004/01/30 22:44:47  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.2  2004/01/29 22:33:01  rickg
 * <p> Tooltip text change
 * <p>
 * <p> Revision 3.1  2003/11/10 07:36:24  rickg
 * <p> Task tags moved to bugzilla, reformat
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.14  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.13  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.12  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.11  2003/10/13 20:26:52  sueh
 * <p> bug270
 * <p> added and changed tooltips
 * <p>
 * <p> Revision 2.10  2003/10/13 17:00:19  sueh
 * <p> bug269
 * <p> UI Changes
 * <p> CCDEraserPanel
 * <p> changed button names
 * <p>
 * <p> Revision 2.9  2003/09/23 23:58:42  sueh
 * <p> bug#237 moved XrayReplacement to Advanced
 * <p>
 * <p> Revision 2.8  2003/09/09 17:14:09  rickg
 * <p> Changed replace text to commit
 * <p>
 * <p> Revision 2.7  2003/08/06 21:56:44  rickg
 * <p> Switched stateful buttons to JToggleButton
 * <p>
 * <p> Revision 2.6  2003/07/30 21:53:44  rickg
 * <p> Use new tooltip formatting class
 * <p>
 * <p> Revision 2.5  2003/07/25 23:02:47  rickg
 * <p> Moved polynomial order, border pixels and inclide adjacent to
 * <p> the global section
 * <p> Corrected spelling mistakes
 * <p>
 * <p> Revision 2.4  2003/07/22 22:17:54  rickg
 * <p> Erase button name change
 * <p> Correct setup of manual replacement parameters
 * <p>
 * <p> Revision 2.3  2003/07/11 23:14:08  rickg
 * <p> Add parameter set and get for new eraser mode
 * <p>
 * <p> Revision 2.2  2003/07/08 20:49:43  rickg
 * <p> Restructure panel for new ccderaser
 * <p>
 * <p> Revision 2.1  2003/05/08 04:26:51  rickg
 * <p> Centered checkbox
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
