package etomo.ui;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JToggleButton;

import etomo.ApplicationManager;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ConstCCDEraserParam;
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
public class CCDEraserPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;
  private AxisID axisID;

  private JPanel pnlCCDEraser = new JPanel();
  private JPanel pnlXRayReplacement = new JPanel();
  private JPanel pnlXRayButtons = new JPanel();
  private JPanel pnlManualReplacement = new JPanel();
  private JPanel pnlEraseButtons = new JPanel();

  private JCheckBox cbXrayReplacement =
    new JCheckBox("Automatic X-ray replacement");
  private LabeledTextField ltfPeakCriterion =
    new LabeledTextField("Peak criterion:");
  private LabeledTextField ltfDiffCriterion =
    new LabeledTextField("Difference criterion:");
  private LabeledTextField ltfGrowCriterion =
    new LabeledTextField("Grow criterion:");
  private LabeledTextField ltfEdgeExclusion =
    new LabeledTextField("Edge exclusion:");
  private LabeledTextField ltfMaximumRadius =
    new LabeledTextField("Maximum radius:");
  private LabeledTextField ltfOuterRadius =
    new LabeledTextField("Outer radius:");
  private LabeledTextField ltfScanRegionSize =
    new LabeledTextField("XY scan size:");
  private LabeledTextField ltfScanCriterion =
    new LabeledTextField("Scan criterion:");
  private JToggleButton btnFindXRays =
    new JToggleButton("<html><b>Find X-rays (trial mode)</b>");
  private JButton btnViewXRayModel =
    new JButton("<html><b>View X-ray model</b>");

  private JCheckBox cbManualReplacement = new JCheckBox("Manual replacement");
  private LabeledTextField ltfGlobalReplacementList =
    new LabeledTextField("All section replacement list: ");
  private LabeledTextField ltfLocalReplacementList =
    new LabeledTextField("Line replacement list: ");
  private JButton btnCreateModel =
    new JButton("<html><b>Create manual replacement model</b>");

  private LabeledTextField ltfInputImage = new LabeledTextField("Input file: ");
  private LabeledTextField ltfOutputImage =
    new LabeledTextField("Output file: ");
  private LabeledTextField ltfBorderPixels =
    new LabeledTextField("Border pixels: ");
  private LabeledTextField ltfPolynomialOrder =
    new LabeledTextField("Polynomial order: ");
  private JCheckBox cbIncludeAdjacentPoints =
    new JCheckBox("Include adjacent points");

  private JToggleButton btnErase = new JToggleButton("<html><b>Create fixed stack</b>");
  private JButton btnViewErased = new JButton("<html><b>View fixed stack</b>");
  private JToggleButton btnReplaceRawStack =
    new JToggleButton("<html><b>Use fixed stack</b>");

  /**
   * Default constructor
   */
  public CCDEraserPanel(ApplicationManager appMgr, AxisID id) {
    applicationManager = appMgr;
    axisID = id;

    setToolTipText();

    pnlXRayReplacement.setLayout(
      new BoxLayout(pnlXRayReplacement, BoxLayout.Y_AXIS));
    pnlXRayReplacement.setBorder(
      new EtchedBorder("Automatic X-ray Replacement").getBorder());

    pnlXRayReplacement.add(cbXrayReplacement);
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfPeakCriterion.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfDiffCriterion.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfGrowCriterion.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfEdgeExclusion.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfMaximumRadius.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfOuterRadius.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfScanRegionSize.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayReplacement.add(ltfScanCriterion.getContainer());
    pnlXRayReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlXRayButtons.setLayout(new BoxLayout(pnlXRayButtons, BoxLayout.X_AXIS));
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnFindXRays);
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnViewXRayModel);
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayReplacement.add(pnlXRayButtons);

    pnlManualReplacement.setLayout(
      new BoxLayout(pnlManualReplacement, BoxLayout.Y_AXIS));
    pnlManualReplacement.setBorder(
      new EtchedBorder("Manual Pixel Region Replacement").getBorder());
    pnlManualReplacement.add(cbManualReplacement);
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(ltfGlobalReplacementList.getContainer());
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(ltfLocalReplacementList.getContainer());
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(btnCreateModel);

    pnlCCDEraser.setLayout(new BoxLayout(pnlCCDEraser, BoxLayout.Y_AXIS));
    pnlCCDEraser.add(pnlXRayReplacement);
    pnlCCDEraser.add(pnlManualReplacement);
    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlCCDEraser.add(ltfInputImage.getContainer());
    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlCCDEraser.add(ltfOutputImage.getContainer());
    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlCCDEraser.add(ltfBorderPixels.getContainer());
    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlCCDEraser.add(ltfPolynomialOrder.getContainer());
    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlCCDEraser.add(cbIncludeAdjacentPoints);

    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlEraseButtons.setLayout(new BoxLayout(pnlEraseButtons, BoxLayout.X_AXIS));
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnErase);
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnViewErased);
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(btnReplaceRawStack);
    pnlEraseButtons.add(Box.createHorizontalGlue());
    pnlCCDEraser.add(pnlEraseButtons);

    // Set the button sizes relative to the font size
    // FIXME: button sizes
    double height = cbXrayReplacement.getPreferredSize().getHeight();
    Dimension dimButton = new Dimension();
    dimButton.setSize(6 * height, 2 * height);
    btnFindXRays.setPreferredSize(dimButton);
    btnFindXRays.setMaximumSize(dimButton);
    btnViewXRayModel.setPreferredSize(dimButton);
    btnViewXRayModel.setMaximumSize(dimButton);
    btnCreateModel.setPreferredSize(dimButton);
    btnCreateModel.setMaximumSize(dimButton);
    btnErase.setPreferredSize(dimButton);
    btnErase.setMaximumSize(dimButton);
    btnViewErased.setPreferredSize(dimButton);
    btnViewErased.setMaximumSize(dimButton);
    btnReplaceRawStack.setPreferredSize(dimButton);
    btnReplaceRawStack.setMaximumSize(dimButton);

    // Bind the buttons to the action listener
    CCDEraserActionListener ccdEraserActionListener =
      new CCDEraserActionListener(this);
    btnFindXRays.addActionListener(ccdEraserActionListener);
    btnViewXRayModel.addActionListener(ccdEraserActionListener);
    btnCreateModel.addActionListener(ccdEraserActionListener);
    btnErase.addActionListener(ccdEraserActionListener);
    btnViewErased.addActionListener(ccdEraserActionListener);
    btnReplaceRawStack.addActionListener(ccdEraserActionListener);
    cbXrayReplacement.addActionListener(ccdEraserActionListener);
    cbManualReplacement.addActionListener(ccdEraserActionListener);

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
    ltfOuterRadius.setText(ccdEraserParams.getOuterRadius());
    ltfScanRegionSize.setText(ccdEraserParams.getXyScanSize());
    ltfEdgeExclusion.setText(ccdEraserParams.getEdgeExclusion());

    cbManualReplacement.setSelected(!ccdEraserParams.getModelFile().equals(""));
    ltfGlobalReplacementList.setText(
      ccdEraserParams.getGlobalReplacementList());
    ltfLocalReplacementList.setText(ccdEraserParams.getlocalReplacementList());
    ltfBorderPixels.setText(ccdEraserParams.getBorderPixels());
    ltfPolynomialOrder.setText(ccdEraserParams.getPolynomialOrder());
    cbIncludeAdjacentPoints.setSelected(
      ccdEraserParams.getIncludeAdjacentPoints());

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
    ccdEraserParams.setOuterRadius(ltfOuterRadius.getText());
    ccdEraserParams.setXyScanSize(ltfScanRegionSize.getText());
    ccdEraserParams.setEdgeExclusion(ltfEdgeExclusion.getText());
    ccdEraserParams.setInputFile(ltfInputImage.getText());
    ccdEraserParams.setOutputFile(ltfOutputImage.getText());
    ccdEraserParams.setGlobalReplacementList(
      ltfGlobalReplacementList.getText());
    ccdEraserParams.setLocalReplacementList(ltfLocalReplacementList.getText());
    ccdEraserParams.setBorderPixels(ltfBorderPixels.getText());
    ccdEraserParams.setPolynomialOrder(ltfPolynomialOrder.getText());
    ccdEraserParams.setIncludeAdjacentPoints(
      cbIncludeAdjacentPoints.isSelected());
    if (cbManualReplacement.isSelected()) {
      ccdEraserParams.setModelFile(
        applicationManager.getDatasetName() + axisID.getExtension() + ".erase");
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
    ltfMaximumRadius.setVisible(state);
    ltfOuterRadius.setVisible(state);
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
    String[] label = { "ccdEraser" };
    String[] manPage = { "ccderaser.html" };
    String[] logFileLabel = { "eraser" };
    String[] logFile = new String[1];
    logFile[0] = "eraser" + axisID.getExtension() + ".log";
    ContextPopup contextPopup =
      new ContextPopup(
        pnlCCDEraser,
        mouseEvent,
        "Preliminary Steps",
        label,
        manPage,
        logFileLabel,
        logFile);
  }

  private void enableXRayReplacement() {
    boolean state = cbXrayReplacement.isSelected();
    ltfPeakCriterion.setEnabled(state);
    ltfDiffCriterion.setEnabled(state);
    ltfGrowCriterion.setEnabled(state);
    ltfEdgeExclusion.setEnabled(state);
    ltfMaximumRadius.setEnabled(state);
    ltfOuterRadius.setEnabled(state);
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
    text = "Input image file.";
    ltfInputImage.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Output image file.  If no output file is specified and the program "
        + "is not run in trial mode, pixels will be replaced in the input file."
        + "  USE REPLACEMENT OPTION WITH CAUTION.";
    ltfOutputImage.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Find and erase peaks a criterion # of SDs above or below background."
        + "  This option must be included for automatic removal of X-rays.";
    cbXrayReplacement.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Criterion # of SDs above local mean for erasing peak based on "
        + "intensity (the default is 10 SDs).";
    ltfPeakCriterion.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Criterion # of SDs above mean pixel-to-pixel difference for "
        + "erasing a peak based on differences (the default is 10 SDs)";
    ltfDiffCriterion.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Criterion # of SDs above mean for adding points to peak (the "
        + "default is 4 SDs).";
    ltfGrowCriterion.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Criterion # of SDs of mean of scan area for picking peaks in "
        + "initial scan (the default is 3 SDs).";
    ltfScanCriterion.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum radius of peak area to erase (the default is 2.1 pixels).";
    ltfMaximumRadius.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Outer radius of annulus around a peak in which to calculate local"
        + " mean and SD (the default is 4.0 pixels).";
    ltfOuterRadius.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Size of regions to compute mean and SD in for initial scans (the "
        + "default is 100 pixels).";
    ltfScanRegionSize.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Width of area to exclude on all edges of image in pixels "
        + "(default 0).";
    ltfEdgeExclusion.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "List of objects that define lines to be replaced.  Ranges can be "
        + "entered, and / to specify all objects.";
    ltfLocalReplacementList.setToolTipText(
      tooltipFormatter.setText(text).format());

	text = "Use a manually created model to specify regions and lines to replace.";
	cbManualReplacement.setToolTipText(tooltipFormatter.setText(text).format());
	
    text =
      "List of objects with points to be replaced on all sections.  "
        + "Ranges can be entered, and / to specify all objects.";
    ltfGlobalReplacementList.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Size of border around points in patch, which contains the "
        + "points which will be fit to (the default is 2 pixels).";
    ltfBorderPixels.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Order of polynomial to fit to border points (the default is 2, "
        + "which includes terms in x, y, x**2, y**2 and x*y).";
    ltfPolynomialOrder.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Include pixels adjacent to the patch being replaced in the pixels "
      + "being fit.";
    cbIncludeAdjacentPoints.setToolTipText(tooltipFormatter.setText(text).format());
    
    text =
      "Run ccderaser in trial mode creating a x-ray model which can be viewed "
        + "in 3dmod.  This will not create an output stack.";
    btnFindXRays.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the x-ray model on the raw stack in 3dmod.";
    btnViewXRayModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Create a manual replacement model using 3dmod.";
    btnCreateModel.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Run ccderaser, erasing the raw stack and writing the modified stack "
        + "to the output file specified (the default is *_fixed.st).  " 
        + "NOTE: subsequent processing uses the "
        + "raw stack filename, therefore for ccderaser to have an effect on "
        + "your data you must commit the raw stack when you are satisfied with"
        + " your ccderaser output stack.";
    btnErase.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the erased stack in 3dmod.";
    btnViewErased.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Commit the raw stack with the output from ccderaser.  "
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
