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
  private JButton btnFindXRays = new JButton("<html><b>Find X-rays</b>");
  private JButton btnViewXRayModel =
    new JButton("<html><b>View X-ray model</b>");

  private JCheckBox cbManualReplacement = new JCheckBox("Manual replacement");
  private LabeledTextField ltfGlobalReplacementList =
    new LabeledTextField("All section replacement list: ");
  private LabeledTextField ltfLocalReplacementList =
    new LabeledTextField("Line replacement list: ");
  private LabeledTextField ltfBorderPixels =
    new LabeledTextField("Border pixels: ");
  private LabeledTextField ltfPolynomialOrder =
    new LabeledTextField("Polynomial order: ");
  private JCheckBox chkboxIncludeAdjacentPoints =
    new JCheckBox("Include adjacent points");
  private JButton btnCreateModel =
    new JButton("<html><b>Create manual replacement model</b>");

  private LabeledTextField ltfInputImage = new LabeledTextField("Input file: ");
  private LabeledTextField ltfOutputImage =
    new LabeledTextField("Output file: ");
  private JButton btnErase = new JButton("<html><b>Erase stack</b>");
  private JButton btnViewErased = new JButton("<html><b>View erased stack</b>");
  private JButton btnReplaceRawStack =
    new JButton("<html><b>Replace raw stack</b>");

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
      new EtchedBorder("Automatic X-ray Replacment").getBorder());

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
      new EtchedBorder("Manual Pixel Region Replacment").getBorder());
    pnlManualReplacement.add(cbManualReplacement);
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(ltfGlobalReplacementList.getContainer());
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(ltfLocalReplacementList.getContainer());
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(ltfBorderPixels.getContainer());
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(ltfPolynomialOrder.getContainer());
    pnlManualReplacement.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlManualReplacement.add(chkboxIncludeAdjacentPoints);
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
    chkboxIncludeAdjacentPoints.setSelected(
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
      chkboxIncludeAdjacentPoints.isSelected());
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
    ltfGrowCriterion.setVisible(state);
    ltfEdgeExclusion.setVisible(state);
    ltfMaximumRadius.setVisible(state);
    ltfOuterRadius.setVisible(state);
    ltfScanRegionSize.setVisible(state);
    ltfScanCriterion.setVisible(state);
    pnlManualReplacement.setVisible(state);
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
    ContextPopup contextPopup =
      new ContextPopup(
        pnlCCDEraser,
        mouseEvent,
        "Preliminary Steps",
        label,
        manPage);
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
    ltfBorderPixels.setEnabled(state);
    ltfPolynomialOrder.setEnabled(state);
    chkboxIncludeAdjacentPoints.setEnabled(state);
    btnCreateModel.setEnabled(state);
  }
  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String line1, line2, line3, line4, line5, line6, line7;

    line1 = "<html>The input image stack filename.<br>";
    line2 = "This specifies the input image stack.  The default value of <br>";
    line3 = "the original source data will work unless you have modified<br>";
    line4 = "or renamed the orignal data.";
    ltfInputImage.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>The ouput image stack filename.<br>";
    line2 =
      "Leave blank to replace existing image stack file.  The default<br>";
    line3 = "processing expects the image stack file to be replaced so a <br>";
    line4 = "blank entry should work for most situations.";
    ltfOutputImage.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>The global replacemnt list.<br>";
    line2 = "A list of objects in the model file which specify the points<br>";
    line3 = "lines to be replaced in <b>ALL</b> projections of the stack.<br>";
    line4 = "You can use / to indicate that all objects in the model file<br>";
    line5 = "specify the pixels to be replaced.  A blank entry indicates<br>";
    line6 = "that no objects will be used for global replacement.<br>";
    line7 = "Ranges of objects are allowed";
    ltfGlobalReplacementList.setToolTipText(
      line1 + line2 + line3 + line4 + line5 + line6 + line7);

    line1 = "<html>The line replacemnt list.<br>";
    line2 = "A list of objects in the model file which specify lines<br>";
    line3 = "to be replaced in <b>SPECIFIC</b> projections of the stack.<br>";
    line4 = "You can use / to indicate that all objects in the model file<br>";
    line5 = "specify the lines to be replaced.  A blank entry indicates<br>";
    line6 = "that no objects will be used for line replacement.<br>";
    line7 = "Ranges of objects are allowed";
    ltfLocalReplacementList.setToolTipText(
      line1 + line2 + line3 + line4 + line5 + line6 + line7);

    line1 = "<html>The size of the border around the replaced pixels.<br>";
    line2 = "This specifes the number pixles are the replaced pixels to be<br>";
    line3 = "used in the interpolation.  Enter / for the default of 3 pixels";
    ltfBorderPixels.setToolTipText(line1 + line2 + line3);

    line1 = "<html>The order polynomial of the interpolation.<br>";
    line2 = "This specifes order of the two dimensional polynomial used to<br>";
    line3 = "interpolate the pixel being replaced.  Enter / for the<br>";
    line4 = "default of a second order polynomial";
    ltfPolynomialOrder.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>Include point adjacent to the specified points in the<br>";
    line2 =
      "polynomial fit.  <b>???</b>Selecting this check box will replace the erase<br>";
    line3 =
      "model points <b>as well as the adjacent points</b> with interpolated values.<br>";
    line4 =
      "Does this effectively increas the replacement patch by 1 pixel in all directions<br>";
    line5 = "<b>prior</b> to performing the interpolation<b>???</b>";
    chkboxIncludeAdjacentPoints.setToolTipText(
      line1 + line2 + line3 + line4 + line5);

    line1 = "<html>This button will open 3dmod with the current CCD erase<br>";
    line2 = "model.  This will allow you to specify additional pixels and<br>";
    line3 = "regions to be replaced with interpolated values.";
    btnCreateModel.setToolTipText(line1 + line2 + line3);

    line1 = "<html>This button will execute the specified erase command<br>";
    line2 = "applying the erase model created in the previous step.";
    btnErase.setToolTipText(line1 + line2);

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
