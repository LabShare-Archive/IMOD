package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
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
import etomo.EtomoDirector;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.ConstSetParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.CombineParams;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.SetParam;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.EtomoAutodoc;
import etomo.type.MetaData;
import etomo.type.ProcessName;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>
 * Description:
 * 
 * Note: The Y and Z parameters are presented to the user in swapped format,
 * all other representations of those parameters are as they appear in the
 * commands. Specifically Y contains the depth dimension.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 3.28  2005/09/16 20:56:15  sueh
 * bug# 532 Moved call to resetParallelPanel() to
 * ApplicationManager.processchunks().  Added resetParallelPanel() to
 * ParallelDialog.
 *
 * Revision 3.27  2005/09/16 18:09:54  sueh
 * bug# 532 Added cbParallelProcess and resume.  Added functions:
 * resume, getParameters(MetaData),
 * getParameters(ProcesschunksParam), isParallelProcessSelected,
 * runVolcombine, and setParameters(ConstMetaData).
 *
 * Revision 3.26  2005/08/27 22:36:53  sueh
 * bug# 532 Changed Autodoc.get() to getInstance().
 *
 * Revision 3.25  2005/08/11 23:52:08  sueh
 * bug# 711  Change enum Run3dmodMenuOption to
 * Run3dmodMenuOptions, which can turn on multiple options at once.
 * This allows ImodState to combine input from the context menu and the
 * pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * the action function.
 *
 * Revision 3.24  2005/08/09 20:22:37  sueh
 * bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * MultiLineButton from JButton.
 *
 * Revision 3.23  2005/04/25 21:06:01  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.
 *
 * Revision 3.22  2005/02/11 16:45:39  sueh
 * bug# 600 Getting tooltips using EtomoAutodoc instead of TooltipFormatter.
 *
 * Revision 3.21  2005/01/26 00:02:33  sueh
 * Removing ConstEtomoNumber.displayDefault.  To get the default to
 * display, set displayValue and default the same.
 *
 * Revision 3.20  2004/12/03 20:23:18  sueh
 * bug# 556 Support older versions of volcombine.com.  Since the set param
 * may be missing or setting the wrong name, check it before loading or
 * unloading ReductionFactor text.  Added enableReductionFactor() to
 * disable ReductionFactor when the set param is missing or invalid.
 *
 * Revision 3.19  2004/12/02 20:39:41  sueh
 * bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * the join guide.  Need to specify the guide to anchor.
 *
 * Revision 3.18  2004/11/30 18:29:36  sueh
 * bug# 556 Made Reduction Factor advanced and gave it a tooltip.
 *
 * Revision 3.17  2004/11/30 00:35:49  sueh
 * bug# 556 Adding reduction factor and get and set params for volcombine.
 *
 * Revision 3.16  2004/11/19 23:54:04  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.15.2.1  2004/10/11 02:13:15  sueh
 * bug# 520 Passed the manager to the ContextPopup object in order to get
 * the propertyUserDir.
 *
 * Revision 3.15  2004/08/28 01:08:28  sueh
 * bug# 507 changed the wording of the don't run volcombine
 * checkbox
 *
 * Revision 3.14  2004/08/19 02:48:33  sueh
 * bug# 508 Added a way to set the don't run volcombine checkbox
 * Added:
 * setRunVolcombine(boolean runVolcombine)
 *
 * Revision 3.13  2004/07/21 00:19:15  sueh
 * bug# 507 added Don't run volcombine checkbox, non-persistant
 *
 * Revision 3.12  2004/06/25 23:24:25  sueh
 * bug# 485 also set patch region model checkbox based on
 * matchorwarp
 *
 * Revision 3.11  2004/06/17 21:47:17  sueh
 * bug# 474 UIUtilities.setButtonSizeAll() causes this, called
 * UIUtilities.setButtonSize(AbstractButton), since there is only
 * one button on the panel in this case.
 *
 * Revision 3.10  2004/06/17 00:31:44  sueh
 * adding call to setToolTipText()
 *
 * Revision 3.9  2004/06/14 23:39:53  rickg
 * Bug #383 Transitioned to using solvematch
 *
 * Revision 3.8  2004/06/13 17:03:23  rickg
 * Solvematch mid change
 *
 * Revision 3.7  2004/05/11 20:50:01  sueh
 * bug# 302 adding FinalCombineValues interface
 * standardizing synchronization
 *
 * Revision 3.6  2004/03/22 23:20:56  sueh
 * bug# 250 Use Patch Region Model checkbox should be set from Patchcorr, not
 * Matchorwarp
 *
 * Revision 3.5  2004/03/09 01:57:23  sueh
 * bug# 381 added Restart at Volcombine button
 *
 * Revision 3.4  2004/03/06 03:48:05  sueh
 * bug# 380 added Use linear interpolation checkbox (advanced)
 *
 * Revision 3.3  2004/03/05 18:19:29  sueh
 * bug# 250 added getCombineParameters() - retrieve parameters found in
 * Combine from final tab
 *
 * Revision 3.2  2004/03/02 21:54:24  sueh
 * bug# 250 setting useBoudaryModel() with Use Patch Region Model
 *
 * Revision 3.1  2004/01/30 22:45:07  sueh
 * bug# 356 Changing buttons with html labels to
 * MultiLineButton and MultiLineToggleButton
 *
 * Revision 3.0  2003/11/07 23:19:01  rickg
 * Version 1.0.0
 *
 * Revision 1.20  2003/10/29 18:05:21  rickg
 * Bug# 304 Tooltips
 *
 * Revision 1.19  2003/10/23 17:09:41  rickg
 * Bug# 323 Label change
 *
 * Revision 1.18  2003/10/21 23:44:30  rickg
 * Changed patch region model button ot a multiline button
 *
 * Revision 1.17  2003/10/17 15:44:24  rickg
 * Bug# 303 Re-layout of UI items
 * Get button sizes from UIParameters static object
 * Revision 1.16 2003/10/16 21:03:35 rickg
 * Bug# 303 Label changes and re-layout
 * 
 * <p>
 * Revision 1.15 2003/10/15 22:45:40 rickg
 * <p>
 * Button size change
 * <p>
 * <p>
 * Revision 1.14 2003/06/05 04:41:31 rickg
 * <p>
 * Label change
 * <p>
 * <p>
 * Revision 1.13 2003/05/15 04:28:48 rickg
 * <p>
 * Removed test button for volcombine
 * <p>
 * <p>
 * Revision 1.12 2003/05/14 14:38:08 rickg
 * <p>
 * Temporary button
 * <p>
 * <p>
 * Revision 1.11 2003/04/09 23:37:46 rickg
 * <p>
 * Moved CheckBoxTextPanel out
 * <p>
 * <p>
 * Revision 1.10 2003/03/26 00:52:56 rickg
 * <p>
 * Added button to convert patch_vector.mod to patch.out
 * <p>
 * <p>
 * Revision 1.9 2003/03/20 17:57:10 rickg
 * <p>
 * Fixed combined volume button size
 * <p>
 * <p>
 * </p>
 */
public class FinalCombinePanel implements ContextMenu, FinalCombineFields,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private TomogramCombinationDialog tomogramCombinationDialog;
  private ApplicationManager applicationManager;

  private JPanel pnlRoot = new JPanel();

  private JPanel pnlPatchcorr = new JPanel();

  private JPanel pnlPatchsize = new JPanel();
  private JPanel pnlPatchsizeEdit = new JPanel();
  private LabeledTextField ltfXPatchSize = new LabeledTextField(
    "X patch size :");
  private LabeledTextField ltfYPatchSize = new LabeledTextField(
    "Z patch size :");
  private LabeledTextField ltfZPatchSize = new LabeledTextField(
    "Y patch size :");
  private JPanel pnlPatchsizeButtons = new JPanel();
  private MultiLineButton btnPatchsizeIncrease = new MultiLineButton(
    "<html><b>Patch Size +20%</b>");
  private MultiLineButton btnPatchsizeDecrease = new MultiLineButton(
    "<html><b>Patch Size -20%</b>");

  private LabeledTextField ltfXNPatches = new LabeledTextField(
    "Number of X patches :");
  private LabeledTextField ltfYNPatches = new LabeledTextField(
    "Number of Z patches :");
  private LabeledTextField ltfZNPatches = new LabeledTextField(
    "Number of Y patches :");

  private JPanel pnlBoundary = new JPanel();
  private LabeledTextField ltfXLow = new LabeledTextField("X Low :");
  private LabeledTextField ltfXHigh = new LabeledTextField("X high :");
  private LabeledTextField ltfYLow = new LabeledTextField("Z Low :");
  private LabeledTextField ltfYHigh = new LabeledTextField("Z high :");
  private LabeledTextField ltfZLow = new LabeledTextField("Y Low :");
  private LabeledTextField ltfZHigh = new LabeledTextField("Y high :");

  private MultiLineButton btnPatchcorrRestart = new MultiLineButton(
    "<html><b>Restart at Patchcorr</b>");

  private JPanel pnlMatchorwarp = new JPanel();
  private JPanel pnlPatchRegionModel = new JPanel();
  private JCheckBox cbUsePatchRegionModel = new JCheckBox(
    "Use patch region model");
  private Run3dmodButton btnPatchRegionModel = new Run3dmodButton(
    "<html><b>Create/Edit Patch Region Model</b>", this);
  private LabeledTextField ltfWarpLimit = new LabeledTextField(
    "Warping residual limits: ");
  private LabeledTextField ltfRefineLimit = new LabeledTextField(
    "Residual limit for single transform: ");

  private LabeledTextField ltfXLowerExclude = new LabeledTextField(
    "Number of columns to exclude on left (in X): ");
  private LabeledTextField ltfXUpperExclude = new LabeledTextField(
    "Number of columns to exclude on right (in X): ");
  private LabeledTextField ltfZLowerExclude = new LabeledTextField(
    "Number of rows to exclude on bottom (in Y): ");
  private LabeledTextField ltfZUpperExclude = new LabeledTextField(
    "Number of rows to exclude on top (in Y): ");
  private JCheckBox cbUseLinearInterpolation = new JCheckBox(
    "Use linear interpolation");
  private JPanel pnlMatchorwarpButtons = new JPanel();
  private MultiLineButton btnMatchorwarpRestart = new MultiLineButton(
    "<html><b>Restart at Matchorwarp</b>");
  private MultiLineButton btnMatchorwarpTrial = new MultiLineButton(
    "<html><b>Matchorwarp Trial Run</b>");
  private JPanel pnlVolcombine = new JPanel();
  private MultiLineButton btnVolcombineRestart = new MultiLineButton(
    "<html><b>Restart at Volcombine</b>");
  private JPanel pnlButton = new JPanel();
  private MultiLineButton btnPatchVectorModel = new MultiLineButton(
    "<html><b>Examine Patch Vector Model</b>");
  private MultiLineButton btnReplacePatchOut = new MultiLineButton(
    "<html><b>Replace Patch Vectors</b>");
  private Run3dmodButton btnImodMatchedTo = new Run3dmodButton(
    "<html><b>Open Volume Being Matched To</b>", this);
  private Run3dmodButton btnImodCombined = new Run3dmodButton(
    "<html><b>Open Combined Volume</b>", this);
  private JCheckBox cbNoVolcombine = new JCheckBox("Stop before running volcombine");
  private LabeledTextField ltfReductionFactor = new LabeledTextField(
      "Reduction factor for matching amplitudes in combined FFT: ");
  private JCheckBox cbParallelProcess = new JCheckBox("Parallel Processing");
  /**
   * Default constructor
   * 
   * @param appMgr
   */
  public FinalCombinePanel(TomogramCombinationDialog parent,
    ApplicationManager appMgr) {

    tomogramCombinationDialog = parent;

    applicationManager = appMgr;
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));

    // Layout Patch region model panel
    pnlPatchRegionModel.setLayout(new BoxLayout(pnlPatchRegionModel,
      BoxLayout.X_AXIS));
    pnlPatchRegionModel.setBorder(new EtchedBorder("Patch Region Model")
      .getBorder());
    pnlPatchRegionModel.add(cbUsePatchRegionModel);
    pnlPatchRegionModel.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPatchRegionModel.add(btnPatchRegionModel.getComponent());
    pnlPatchRegionModel.add(Box.createHorizontalGlue());
    btnPatchRegionModel.setSize();

    // Layout the Patchcorr panel
    pnlPatchcorr.setLayout(new BoxLayout(pnlPatchcorr, BoxLayout.Y_AXIS));
    pnlPatchcorr
      .setBorder(new EtchedBorder("Patchcorr Parameters").getBorder());

    pnlPatchsizeButtons.setLayout(new BoxLayout(pnlPatchsizeButtons,
      BoxLayout.Y_AXIS));
    pnlPatchsizeButtons.add(btnPatchsizeIncrease.getComponent());
    pnlPatchsizeButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeButtons.add(btnPatchsizeDecrease.getComponent());
    UIUtilities.setButtonSizeAll(pnlPatchsizeButtons, UIParameters
      .getButtonDimension());

    pnlPatchsizeEdit
      .setLayout(new BoxLayout(pnlPatchsizeEdit, BoxLayout.Y_AXIS));

    pnlPatchsizeEdit.add(ltfXPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfZPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfYPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlPatchsize.setLayout(new BoxLayout(pnlPatchsize, BoxLayout.X_AXIS));
    pnlPatchsize.add(pnlPatchsizeEdit);
    pnlPatchsize.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPatchsize.add(pnlPatchsizeButtons);
    pnlPatchcorr.add(pnlPatchsize);

    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlBoundary.setLayout(new GridLayout(3, 3, 5, 5));
    pnlBoundary.add(ltfXNPatches.getContainer());
    pnlBoundary.add(ltfXLow.getContainer());
    pnlBoundary.add(ltfXHigh.getContainer());
    pnlBoundary.add(ltfZNPatches.getContainer());
    pnlBoundary.add(ltfZLow.getContainer());
    pnlBoundary.add(ltfZHigh.getContainer());
    pnlBoundary.add(ltfYNPatches.getContainer());
    pnlBoundary.add(ltfYLow.getContainer());
    pnlBoundary.add(ltfYHigh.getContainer());
    pnlPatchcorr.add(pnlBoundary);
    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    btnPatchcorrRestart.setAlignmentX(Component.CENTER_ALIGNMENT);

    pnlPatchcorr.add(btnPatchcorrRestart.getComponent());
    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    UIUtilities.setButtonSizeAll(pnlPatchcorr, UIParameters
      .getButtonDimension());

    //  Layout the Matchorwarp panel
    pnlMatchorwarp.setLayout(new BoxLayout(pnlMatchorwarp, BoxLayout.Y_AXIS));
    pnlMatchorwarp.setBorder(new EtchedBorder("Matchorwarp Parameters")
      .getBorder());

    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlMatchorwarp.add(ltfRefineLimit.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlMatchorwarp.add(ltfWarpLimit.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlMatchorwarp.add(ltfXLowerExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(ltfXUpperExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(ltfZLowerExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(ltfZUpperExclude.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(cbUseLinearInterpolation);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlMatchorwarpButtons.setLayout(new BoxLayout(pnlMatchorwarpButtons,
      BoxLayout.X_AXIS));
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpRestart.getComponent());
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpTrial.getComponent());
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlMatchorwarpButtons, UIParameters
      .getButtonDimension());

    pnlMatchorwarp.add(pnlMatchorwarpButtons);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlVolcombine.setLayout(new BoxLayout(pnlVolcombine, BoxLayout.Y_AXIS));
    pnlVolcombine.add(cbParallelProcess);
    //TEMP
    cbParallelProcess.setVisible(EtomoDirector.getInstance().isNewstuff());
    pnlVolcombine.add(cbNoVolcombine);
    pnlVolcombine.add(ltfReductionFactor.getContainer());
    pnlVolcombine.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlVolcombine.add(btnVolcombineRestart.getComponent());
    cbNoVolcombine.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnVolcombineRestart.setAlignmentX(Component.CENTER_ALIGNMENT);
    UIUtilities.setButtonSizeAll(pnlVolcombine, UIParameters
      .getButtonDimension());

    //  Create the button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnPatchVectorModel.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnReplacePatchOut.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodMatchedTo.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodCombined.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlButton, UIParameters.getButtonDimension());

    //  Root panel layout
    pnlRoot.add(pnlPatchRegionModel);
    pnlRoot.add(pnlPatchcorr);
    pnlRoot.add(pnlMatchorwarp);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(pnlVolcombine);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(Box.createVerticalGlue());
    pnlRoot.add(pnlButton);

    // Bind the buttons to action listener
    ButtonActionListener actionListener = new ButtonActionListener(this);
    btnPatchcorrRestart.addActionListener(actionListener);
    btnPatchsizeIncrease.addActionListener(actionListener);
    btnPatchsizeDecrease.addActionListener(actionListener);
    btnPatchRegionModel.addActionListener(actionListener);
    btnMatchorwarpRestart.addActionListener(actionListener);
    btnMatchorwarpTrial.addActionListener(actionListener);
    btnVolcombineRestart.addActionListener(actionListener);
    btnPatchVectorModel.addActionListener(actionListener);
    btnReplacePatchOut.addActionListener(actionListener);
    btnImodMatchedTo.addActionListener(actionListener);
    btnImodCombined.addActionListener(actionListener);
    cbParallelProcess.addActionListener(actionListener);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
    setToolTipText();
  }

  void setAdvanced(boolean state) {
    pnlBoundary.setVisible(state);
    ltfRefineLimit.setVisible(state);
    cbUseLinearInterpolation.setVisible(state);
    this.ltfReductionFactor.setVisible(state);
  }

  /**
   * Return the pnlRoot reference
   * 
   * @return Container
   */
  public Container getContainer() {
    return pnlRoot;
  }

  public void setUsePatchRegionModel(boolean usePatchRegionModel) {
    cbUsePatchRegionModel.setSelected(usePatchRegionModel);
  }

  public boolean isUsePatchRegionModel() {
    return cbUsePatchRegionModel.isSelected();
  }

  public void setXMin(String xMin) {
    ltfXLow.setText(xMin);
  }

  public String getXMin() {
    return ltfXLow.getText();
  }

  public void setXMax(String xMax) {
    ltfXHigh.setText(xMax);
  }

  public String getXMax() {
    return ltfXHigh.getText();
  }

  public void setYMin(String yMin) {
    ltfZLow.setText(yMin);
  }

  public String getYMin() {
    return ltfZLow.getText();
  }

  public void setYMax(String yMax) {
    ltfZHigh.setText(yMax);
  }

  public String getYMax() {
    return ltfZHigh.getText();
  }

  public void setZMin(String zMin) {
    ltfYLow.setText(zMin);
  }

  public String getZMin() {
    return ltfYLow.getText();
  }

  public void setZMax(String zMax) {
    ltfYHigh.setText(zMax);
  }

  public String getZMax() {
    return ltfYHigh.getText();
  }
  
  boolean isRunVolcombine() {
    return !cbNoVolcombine.isSelected();
  }
  
  void setRunVolcombine(boolean runVolcombine) {
    cbNoVolcombine.setSelected(!runVolcombine);
  }
  
  final void getParameters(MetaData metaData) {
    //TEMP
    if (EtomoDirector.getInstance().isNewstuff()) {
      metaData.setCombineParallelProcess(cbParallelProcess.isSelected());
    }
  }
  
  final void setParameters(ConstMetaData metaData) {
    cbParallelProcess.setSelected(metaData.getCombineParallelProcess());
  }
  
  final boolean isParallelProcessSelected() {
    return cbParallelProcess.isSelected();
  }

  /**
   * Set the values of the patchcrawl3D UI objects from the
   * ConstPatchcrawl3DParam object.
   * 
   * @param patchrawlParam
   */
  public void setPatchcrawl3DParams(ConstPatchcrawl3DParam patchrawlParam) {
    cbUsePatchRegionModel.setSelected(patchrawlParam.isUseBoundaryModel());
    ltfXPatchSize.setText(patchrawlParam.getXPatchSize());
    ltfYPatchSize.setText(patchrawlParam.getYPatchSize());
    ltfZPatchSize.setText(patchrawlParam.getZPatchSize());
    ltfXNPatches.setText(patchrawlParam.getNX());
    ltfYNPatches.setText(patchrawlParam.getNY());
    ltfZNPatches.setText(patchrawlParam.getNZ());
    ltfXLow.setText(patchrawlParam.getXLow());
    ltfXHigh.setText(patchrawlParam.getXHigh());
    ltfYLow.setText(patchrawlParam.getYLow());
    ltfYHigh.setText(patchrawlParam.getYHigh());
    ltfZLow.setText(patchrawlParam.getZLow());
    ltfZHigh.setText(patchrawlParam.getZHigh());
  }
  
  void setVolcombineParams(ConstSetParam setParam) {
    if (setParam == null || !setParam.isValid()) {
      return;
    }
    ltfReductionFactor.setText(setParam.getValue());
  }
  
  void getVolcombineParams(SetParam setParam) {
    if (setParam == null) {
      return;
    }
    setParam.setValue(ltfReductionFactor.getText());
  }
  
  void enableReductionFactor(boolean enable) {
    ltfReductionFactor.setEnabled(enable);
  }

  /**
   * Set the Patchcrawl3DParam object values from the UI values.
   * 
   * @param patchrawlParam
   * @throws NumberFormatException
   */
  public void getPatchcrawl3DParams(Patchcrawl3DParam patchcrawl3DParam)
    throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = cbUsePatchRegionModel.getText();
      patchcrawl3DParam.setUseBoundaryModel(cbUsePatchRegionModel.isSelected());
      badParameter = ltfXPatchSize.getLabel();
      patchcrawl3DParam
        .setXPatchSize(Integer.parseInt(ltfXPatchSize.getText()));
      badParameter = ltfYPatchSize.getLabel();
      patchcrawl3DParam
        .setYPatchSize(Integer.parseInt(ltfYPatchSize.getText()));
      badParameter = ltfZPatchSize.getLabel();
      patchcrawl3DParam
        .setZPatchSize(Integer.parseInt(ltfZPatchSize.getText()));
      badParameter = ltfXNPatches.getLabel();
      patchcrawl3DParam.setNX(Integer.parseInt(ltfXNPatches.getText()));
      badParameter = ltfYNPatches.getLabel();
      patchcrawl3DParam.setNY(Integer.parseInt(ltfYNPatches.getText()));
      badParameter = ltfZNPatches.getLabel();
      patchcrawl3DParam.setNZ(Integer.parseInt(ltfZNPatches.getText()));
      badParameter = ltfXLow.getLabel();
      patchcrawl3DParam.setXLow(Integer.parseInt(ltfXLow.getText()));
      badParameter = ltfXHigh.getLabel();
      patchcrawl3DParam.setXHigh(Integer.parseInt(ltfXHigh.getText()));
      badParameter = ltfYLow.getLabel();
      patchcrawl3DParam.setYLow(Integer.parseInt(ltfYLow.getText()));
      badParameter = ltfYHigh.getLabel();
      patchcrawl3DParam.setYHigh(Integer.parseInt(ltfYHigh.getText()));
      badParameter = ltfZLow.getLabel();
      patchcrawl3DParam.setZLow(Integer.parseInt(ltfZLow.getText()));
      badParameter = ltfZHigh.getLabel();
      patchcrawl3DParam.setZHigh(Integer.parseInt(ltfZHigh.getText()));

    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  /**
   * Set the values of the matchorwarp UI objects from the
   * ConstMatchorwarpParam object.
   * 
   * @param matchorwarpParam
   */
  public void setMatchorwarpParams(ConstMatchorwarpParam matchorwarpParam) {
    ltfWarpLimit.setText(matchorwarpParam.getWarpLimit());
    ltfRefineLimit.setText(matchorwarpParam.getRefineLimit());

    if (matchorwarpParam.getXLowerExclude() > 0) {
      ltfXLowerExclude.setText(matchorwarpParam.getXLowerExclude());
    }
    if (matchorwarpParam.getXUpperExclude() > 0) {
      ltfXUpperExclude.setText(matchorwarpParam.getXUpperExclude());
    }

    if (matchorwarpParam.getZLowerExclude() > 0) {
      ltfZLowerExclude.setText(matchorwarpParam.getZLowerExclude());
    }

    if (matchorwarpParam.getZUpperExclude() > 0) {
      ltfZUpperExclude.setText(matchorwarpParam.getZUpperExclude());
    }

    cbUseLinearInterpolation.setSelected(matchorwarpParam
      .isUseLinearInterpolation());
      
    //when loading into the dialog, matchorwarp takes precidence over patchcorr
    cbUsePatchRegionModel.setSelected(matchorwarpParam.isUseModelFile());
  }

  /**
   * Set the MatchorwarpParam object values from the UI values.
   * 
   * @param matchorwarpParam
   * @throws NumberFormatException
   */
  public void getMatchorwarpParams(MatchorwarpParam matchorwarpParam)
    throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = cbUsePatchRegionModel.getText();
      if (cbUsePatchRegionModel.isSelected()) {
        matchorwarpParam.setDefaultModelFile();
      }
      else {
        matchorwarpParam.setModelFile("");
      }

      badParameter = ltfWarpLimit.getLabel();
      matchorwarpParam.setWarpLimit(ltfWarpLimit.getText());

      badParameter = ltfRefineLimit.getLabel();
      matchorwarpParam.setRefineLimit(Double.parseDouble(ltfRefineLimit
        .getText()));

      badParameter = ltfXLowerExclude.getLabel();
      String text = ltfXLowerExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setXLowerExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setXLowerExclude(0);
      }

      badParameter = ltfXUpperExclude.getLabel();
      text = ltfXUpperExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setXUpperExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setXUpperExclude(0);
      }

      badParameter = ltfZLowerExclude.getLabel();
      text = ltfZLowerExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setZLowerExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setZLowerExclude(0);
      }

      badParameter = ltfZUpperExclude.getLabel();
      text = ltfZUpperExclude.getText();
      if (text.matches("\\S+")) {
        matchorwarpParam.setZUpperExclude(Integer.parseInt(text));
      }
      else {
        matchorwarpParam.setZUpperExclude(0);
      }
      badParameter = cbUseLinearInterpolation.getText();
      matchorwarpParam.setUseLinearInterpolation(cbUseLinearInterpolation
        .isSelected());
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  /**
   * Get the combine parameters from the UI
   * @param combineParams
   */
  public void getCombineParameters(CombineParams combineParams) {
    if (cbUsePatchRegionModel.isSelected()) {
      combineParams.setDefaultPatchRegionModel();
    }
    else {
      combineParams.setPatchRegionModel("");
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = {"Patchcrawl3d", "Matchorwarp"};
    String[] manPage = {"patchcrawl3d.html", "matchorwarp.html"};
    String[] logFileLabel = {"Patchcorr", "Matchorwarp", "Volcombine"};
    String[] logFile = {"patchcorr.log", "matchorwarp.log", "volcombine.log"};
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent,
      "Patch Problems in Combining", ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel,
      logFile, applicationManager);
  }
  
  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }
  
  private void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnPatchRegionModel.getActionCommand())) {
      applicationManager.imodPatchRegionModel(menuOptions);
    }
    if (command.equals(btnImodMatchedTo.getActionCommand())) {
      applicationManager.imodMatchedToTomogram(menuOptions);
    }
    if (command.equals(btnImodCombined.getActionCommand())) {
      applicationManager.imodCombinedTomogram(menuOptions);
    }
  }
  
  public final void getParameters(ProcesschunksParam param) {
    param.setRootName(ProcessName.VOLCOMBINE.toString());
  }

  private void buttonAction(ActionEvent event) {
    // Synchronize this panel with the others
    tomogramCombinationDialog.synchronize(TomogramCombinationDialog.lblFinal,
      true, TomogramCombinationDialog.ALL_FIELDS);

    String command = event.getActionCommand();
    // Decrease patch sizes by 20%
    // and then round to ints
    // since they are in
    // pixels
    if (command.equals(btnPatchsizeDecrease.getActionCommand())) {
      ltfXPatchSize.setText(Math.round(Integer
        .parseInt(ltfXPatchSize.getText()) / 1.2f));
      ltfYPatchSize.setText(Math.round(Integer
        .parseInt(ltfYPatchSize.getText()) / 1.2f));
      ltfZPatchSize.setText(Math.round(Integer
        .parseInt(ltfZPatchSize.getText()) / 1.2f));
    }
    //  Increase patch sizes by 20% and then round to ints since they are
    // in
    // pixels
    else if (command.equals(btnPatchsizeIncrease.getActionCommand())) {
      ltfXPatchSize.setText(Math.round(Integer
        .parseInt(ltfXPatchSize.getText()) * 1.2f));
      ltfYPatchSize.setText(Math.round(Integer
        .parseInt(ltfYPatchSize.getText()) * 1.2f));
      ltfZPatchSize.setText(Math.round(Integer
        .parseInt(ltfZPatchSize.getText()) * 1.2f));
    }
    else if (command.equals(btnPatchcorrRestart.getActionCommand())) {
      applicationManager.patchcorrCombine();
    }
    else if (command.equals(btnMatchorwarpRestart.getActionCommand())) {
      applicationManager.matchorwarpCombine();
    }
    else if (command.equals(btnMatchorwarpTrial.getActionCommand())) {
      applicationManager.matchorwarpTrial();
    }
    else if (command.equals(btnVolcombineRestart.getActionCommand())) {
      if (cbParallelProcess.isSelected()) {
          applicationManager.splitcombine();
      }
      else {
        applicationManager.volcombine();
      }
    }
    else if (command.equals(btnPatchVectorModel.getActionCommand())) {
      applicationManager.imodPatchVectorModel();
    }
    else if (command.equals(btnReplacePatchOut.getActionCommand())) {
      applicationManager.modelToPatch();
    }
    else if (command.equals(cbParallelProcess.getActionCommand())) {
      tomogramCombinationDialog.updateParallelProcess();
    }
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  class ButtonActionListener implements ActionListener {
    FinalCombinePanel listenee;

    ButtonActionListener(FinalCombinePanel finalCombinePanel) {
      listenee = finalCombinePanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.buttonAction(event);
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    Autodoc autodoc = null;

    try {
      autodoc = Autodoc.getInstance(Autodoc.COMBINE_FFT, AxisID.ONLY);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }

    text = "Size of correlation patches in X.";
    ltfXPatchSize.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Size of correlation patches in Y.";
    ltfYPatchSize.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Size of correlation patches in Z.";
    ltfZPatchSize.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Increase all patch dimensions by 20%.";
    btnPatchsizeIncrease
      .setToolTipText(tooltipFormatter.setText(text).format());

    text = "Decrease all patch dimensions by 20%.";
    btnPatchsizeDecrease
      .setToolTipText(tooltipFormatter.setText(text).format());

    text = "Number of patches to correlate in the X dimension.";
    ltfXNPatches.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Number of patches to correlate in the Y dimension.";
    ltfYNPatches.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Number of patches to correlate in the Z dimension.";
    ltfZNPatches.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Minimum X coordinate for left edge of correlation patches.";
    ltfXLow.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Minimum Y coordinate for upper edge of correlation patches.";
    ltfYLow.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Minimum Z coordinate for top edge of correlation patches.";
    ltfZLow.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum X coordinate for right edge of correlation patches.";
    ltfXHigh.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum Y coordinate for lower edge of correlation patches.";
    ltfYHigh.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum Z coordinate for bottom edge of correlation patches.";
    ltfZHigh.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Compute new displacements between patches by cross-correlation.";
    btnPatchcorrRestart.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Use a model with contours around the areas where patches should be "
      + "correlated to prevent bad patches outside those areas.";
    cbUsePatchRegionModel.setToolTipText(tooltipFormatter.setText(text)
      .format());

    text = "Open the volume being matched to and create the patch region model.";
    btnPatchRegionModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Enter a comma-separate series of mean residual limits to try in "
      + "succession when fitting warping transformations to the patch "
      + "displacements.";
    ltfRefineLimit.setToolTipText(tooltipFormatter.setText(text).format());
    text = "The mean residual limit for fit all patch displacements to a single "
      + "linear transformation.";
    ltfWarpLimit.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Exclude columns of patches on the left from the fits. Number of columns "
      + "of patches on the left to exclude from the fits.";
    ltfXLowerExclude.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Exclude columns of patches on the right from the fits. Number of columns"
      + " of patches on the right to exclude from the fits.";
    ltfXUpperExclude.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Exclude rows of patches on the bottom from the fits. Number of rows of "
      + "patches on the bottom in Y to exclude from the fits.";
    ltfZLowerExclude.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Exclude rows of patches on the top from the fits. Number of rows of "
      + "patches on the top in Y to exclude from the fits.";
    ltfZUpperExclude.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Uses linear instead of quadratic interpolation for transforming"
      + "the volume with Matchvol or Warpvol.";
    cbUseLinearInterpolation.setToolTipText(tooltipFormatter.setText(text)
      .format());

    text = "Restart the combine operation at Matchorwarp, which tries to fit "
      + "transformations to the patch displacements.";
    btnMatchorwarpRestart.setToolTipText(tooltipFormatter.setText(text)
      .format());

    text = "Restart the combine operation at Volcombine, which combines volumes.";
    btnVolcombineRestart
      .setToolTipText(tooltipFormatter.setText(text).format());

    text = "Run Matchorwarp in trial mode; find transformations then stop.";
    btnMatchorwarpTrial.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the patch displacement vectors in and possibly delete bad vectors.";
    btnPatchVectorModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Replace the patch displacements with the vectors from the edited model.";
    btnReplacePatchOut.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the volume being matched to in 3dmod.";
    btnImodMatchedTo.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the final combined volume.";
    btnImodCombined.setToolTipText(tooltipFormatter.setText(text).format());
    
    if (autodoc != null) {
      text = EtomoAutodoc.getTooltip(autodoc, "ReductionFraction");
      if (text != null) {
        ltfReductionFactor.setToolTipText(tooltipFormatter.setText(text)
            .format());
      }
    }
  }
}