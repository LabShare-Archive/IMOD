/**
 * <p>Description: Panel to modify the CCD eraser parameters</p>
 *
 * <p>Copyright: Copyright (c) 2002-2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
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
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ConstCCDEraserParam;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;

public class CCDEraserPanel implements ContextMenu {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager;
  private AxisID axisID;

  private JPanel pnlCCDEraser = new JPanel();
  private JPanel pnlXRayReplacement = new JPanel();
  private JPanel pnlXRayButtons = new JPanel();
  private JPanel pnlManualReplacement = new JPanel();
  private JPanel pnlManualButtons = new JPanel();
  private JPanel pnlEraseButtons = new JPanel();

  private JCheckBox cbXrayReplacement = new JCheckBox(
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
  private MultiLineToggleButton btnFindXRays = new MultiLineToggleButton(
    "<html><b>Find X-rays (Trial Mode)</b>");
  private MultiLineButton btnViewXRayModel = new MultiLineButton(
    "<html><b>View X-ray Model</b>");

  private JCheckBox cbManualReplacement = new JCheckBox("Manual replacement");
  private LabeledTextField ltfGlobalReplacementList = new LabeledTextField(
    "All section replacement list: ");
  private LabeledTextField ltfLocalReplacementList = new LabeledTextField(
    "Line replacement list: ");
  private MultiLineButton btnCreateModel = new MultiLineButton(
    "<html><b>Create Manual Replacement Model</b>");

  private LabeledTextField ltfInputImage = new LabeledTextField("Input file: ");
  private LabeledTextField ltfOutputImage = new LabeledTextField(
    "Output file: ");
  private LabeledTextField ltfBorderPixels = new LabeledTextField(
    "Border pixels: ");
  private LabeledTextField ltfPolynomialOrder = new LabeledTextField(
    "Polynomial order: ");
  private JCheckBox cbIncludeAdjacentPoints = new JCheckBox(
    "Include adjacent points");

  private MultiLineToggleButton btnErase = new MultiLineToggleButton(
    "<html><b>Create Fixed Stack</b>");
  private MultiLineButton btnViewErased = new MultiLineButton(
    "<html><b>View Fixed Stack</b>");
  private MultiLineToggleButton btnReplaceRawStack = new MultiLineToggleButton(
    "<html><b>Use Fixed Stack</b>");

  /**
   * Default constructor
   */
  public CCDEraserPanel(ApplicationManager appMgr, AxisID id) {
    applicationManager = appMgr;
    axisID = id;

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
    UIUtilities
      .addWithYSpace(pnlXRayReplacement, ltfAnnulusWidth.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfScanRegionSize
      .getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfScanCriterion
      .getContainer());

    pnlXRayButtons.setLayout(new BoxLayout(pnlXRayButtons, BoxLayout.X_AXIS));
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnFindXRays);
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnViewXRayModel);
    pnlXRayButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlXRayButtons, UIParameters.dimButton);
    
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

    pnlManualButtons
      .setLayout(new BoxLayout(pnlManualButtons, BoxLayout.X_AXIS));
    pnlManualButtons.add(Box.createHorizontalGlue());
    pnlManualButtons.add(btnCreateModel);
    pnlManualButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlManualButtons, UIParameters.dimButton);
    
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
    pnlEraseButtons.add(btnErase);
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnViewErased);
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnReplaceRawStack);
    pnlEraseButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlEraseButtons, UIParameters.dimButton);
    
    UIUtilities.addWithYSpace(pnlCCDEraser, pnlEraseButtons);

    // Bind the buttons to the action listener
    CCDEraserActionListener ccdEraserActionListener = new CCDEraserActionListener(
      this);
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
    ltfBorderPixels.setText(ccdEraserParams.getBorderPixels());
    ltfPolynomialOrder.setText(ccdEraserParams.getPolynomialOrder());
    cbIncludeAdjacentPoints.setSelected(ccdEraserParams
      .getIncludeAdjacentPoints());

    enableXRayReplacement();
    enableManualReplacement();
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
    ccdEraserParams.setBorderPixels(ltfBorderPixels.getText());
    ccdEraserParams.setPolynomialOrder(ltfPolynomialOrder.getText());
    ccdEraserParams.setIncludeAdjacentPoints(cbIncludeAdjacentPoints
      .isSelected());
    if (cbManualReplacement.isSelected()) {
      ccdEraserParams.setModelFile(applicationManager.getMetaData().getDatasetName()
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

  //  Button action method
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(btnFindXRays.getActionCommand())) {
      applicationManager.findXrays(axisID);
    }
    else if (command.equals(btnViewXRayModel.getActionCommand())) {
      applicationManager.imodXrayModel(axisID);
    }
    else if (command.equals(btnCreateModel.getActionCommand())) {
      applicationManager.imodManualErase(axisID);
    }
    else if (command.equals(btnErase.getActionCommand())) {
      applicationManager.eraser(axisID);
    }
    else if (command.equals(btnViewErased.getActionCommand())) {
      applicationManager.imodErasedStack(axisID);
    }
    else if (command.equals(btnReplaceRawStack.getActionCommand())) {
      applicationManager.replaceRawStack(axisID);
    }
    else if (command.equals(cbXrayReplacement.getActionCommand())) {
      enableXRayReplacement();
    }
    else if (command.equals(cbManualReplacement.getActionCommand())) {
      enableManualReplacement();
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] label = {"CCDEraser"};
    String[] manPage = {"ccderaser.html"};

    String[] logFileLabel = {"Eraser"};
    String[] logFile = new String[1];
    logFile[0] = "eraser" + axisID.getExtension() + ".log";

    ContextPopup contextPopup = new ContextPopup(pnlCCDEraser, mouseEvent,
      "PRE-PROCESSING", ContextPopup.TOMO_GUIDE, label, manPage, logFileLabel, logFile, applicationManager);
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
    btnCreateModel.setEnabled(state);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.get(Autodoc.CCDERASER);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    
    ltfInputImage.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.INPUT_FILE_KEY)).format());
    ltfOutputImage.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.OUTPUT_FILE_KEY)).format());
    cbXrayReplacement.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.FIND_PEAKS_KEY)).format());
    ltfPeakCriterion.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.PEAK_CRITERION_KEY)).format());
    ltfDiffCriterion.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.DIFF_CRITERION_KEY)).format());
    ltfGrowCriterion.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.GROW_CRITERION_KEY)).format());
    ltfScanCriterion.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.SCAN_CRITERION_KEY)).format());
    ltfMaximumRadius.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.MAXIMUM_RADIUS_KEY)).format());
    ltfAnnulusWidth.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.ANNULUS_WIDTH_KEY)).format());
    ltfScanRegionSize.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.X_Y_SCAN_SIZE_KEY)).format());
    ltfEdgeExclusion.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.EDGE_EXCLUSION_WIDTH_KEY)).format());
    ltfLocalReplacementList.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.LINE_OBJECTS_KEY)).format());

    text = "Use a manually created model to specify regions and lines to replace.";
    cbManualReplacement.setToolTipText(tooltipFormatter.setText(text).format());

    ltfGlobalReplacementList.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.ALL_SECTION_OBJECTS_KEY)).format());
    ltfBorderPixels.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.BORDER_SIZE_KEY)).format());
    ltfPolynomialOrder.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.POLYNOMIAL_ORDER_KEY)).format());

    text = "Include pixels adjacent to the patch being replaced in the pixels "
        + "being fit.";
    cbIncludeAdjacentPoints.setToolTipText(tooltipFormatter.setText(text)
      .format());

    btnFindXRays.setToolTipText(tooltipFormatter.setText(EtomoAutodoc.getTooltip(autodoc, CCDEraserParam.TRIAL_MODE_KEY)).format());

    text = "View the x-ray model on the raw stack in 3dmod.";
    btnViewXRayModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Create a manual replacement model using 3dmod.";
    btnCreateModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Run ccderaser, erasing the raw stack and writing the modified stack "
        + "to the output file specified (the default is *_fixed.st).  "
        + "NOTE: subsequent processing uses the "
        + "raw stack filename, therefore for ccderaser to have an effect on "
        + "your data you must commit the raw stack when you are satisfied with"
        + " your ccderaser output stack.";
    btnErase.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the erased stack in 3dmod.";
    btnViewErased.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Use the raw stack with the output from ccderaser.  "
        + "NOTE: subsequent processing uses the "
        + "raw stack filename, therefore for ccderaser to have an effect on "
        + "your data you must commit the raw stack when you are satisfied with"
        + " your ccderaser output stack.";
    btnReplaceRawStack.setToolTipText(tooltipFormatter.setText(text).format());
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