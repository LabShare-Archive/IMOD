package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;

import etomo.ApplicationManager;
import etomo.storage.TomopitchLog;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: Tomogram Positioning User Interface</p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.52  2006/07/28 20:14:28  sueh
 * <p> bug# 868 Changed isFiduciallessAlignment to isFiducialess
 * <p>
 * <p> Revision 3.51  2006/07/20 17:23:17  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 3.50  2006/07/18 18:01:45  sueh
 * <p> bug# 906 Added isAlignButtonEnabled().
 * <p>
 * <p> Revision 3.49  2006/07/05 23:26:51  sueh
 * <p> Added tooltips.
 * <p>
 * <p> Revision 3.48  2006/07/04 18:04:16  sueh
 * <p> bug# 896 Fixed done() - don't remove action listeners if done returns false.
 * <p>
 * <p> Revision 3.47  2006/07/04 05:35:12  mast
 * <p> Fixed double colon on Z shift display
 * <p>
 * <p> Revision 3.46  2006/06/30 20:04:25  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog done() functions,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.
 * <p>
 * <p> Revision 3.45  2006/06/21 15:55:26  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.44  2006/06/09 19:51:29  sueh
 * <p> bug# 870 Added isTomopitchButtonSelected() and isAlignButtonSelected().
 * <p>
 * <p> Revision 3.43  2006/05/19 19:51:27  sueh
 * <p> bug# 866 Moved set/getParam functions, and updateDisplay functions to TomogramPositioningExpert.
 * <p> Added field-level set, get, and setEnabled functions.
 * <p>
 * <p> Revision 3.42  2006/05/11 19:32:29  sueh
 * <p> bug# 838 Added CalcPanel - a three variable panel which shows the
 * <p> calculation of angles calculated by tomopitch.  Added x axis tilt and
 * <p> thickness (separate from the sample thickness.)  Added extra thickness.
 * <p>
 * <p> Revision 3.41  2006/02/06 21:22:42  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.40  2006/01/26 22:09:00  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.39  2006/01/20 21:13:46  sueh
 * <p> bug# 401 Saving the state of btnSample.
 * <p>
 * <p> Revision 3.38  2006/01/04 00:01:21  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.
 * <p>
 * <p> Revision 3.37  2005/11/14 22:29:13  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 3.36  2005/08/12 00:01:26  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Prevent context menu from popping up when button is
 * <p> disabled.  Get rid of duplicate code by running the 3dmods from a private
 * <p> function called run3dmod(String, Run3dmodMenuOptions).  It can be
 * <p> called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and the
 * <p> action function.
 * <p>
 * <p> Revision 3.35  2005/08/10 20:48:26  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.34  2005/08/09 21:11:23  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * <p> MultiLineButton from JButton.
 * <p>
 * <p> Revision 3.33  2005/08/04 20:17:32  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.32  2005/07/14 22:16:51  sueh
 * <p> bug# 626 Enabling whole tomogram for montage view.  Setting binning in
 * <p> set and getParameters(BlendmontParam).
 * <p>
 * <p> Revision 3.31  2005/06/13 23:40:13  sueh
 * <p> bug# 675 Using NamedCheckBox instead of JCheckBox to try out
 * <p> jfcUnit.  Bug# 583 Always save the screen value of binning in metaData.
 * <p>
 * <p> Revision 3.30  2005/06/11 03:01:20  sueh
 * <p> bug# 583, bug# 682, bug# 677, bug# 584  Moved binning calculation to
 * <p> ApplicationManager.  Storing screen binning for Tomo Pos and Tomo
 * <p> Gen in MetaData separately (Tomo Pos default is 3).  Upgraded tilt.com
 * <p> to have all unbinned parameters and a binning value.  No longer
 * <p> managing full image size in tilt.com, except to upgrade the file.  Fixed
 * <p> potential divide by 0 errors and incorrect binning calculation errors in
 * <p> Tomo Pos.  Added member variable:  getBinningFromNewst.  Removed
 * <p> member variables:  fullImageSize, prealiHeader, rawstackheader.  Add
 * <p> functions:  setParameters(ConstMetaData),
 * <p> setParameters(ConstNewstParam).  Modified functions:
 * <p> TomogramPostioningDialog(), getAlignParams, getBinning,
 * <p> getNewstParamst, getTiltParams, getTomopitchParams,
 * <p> setAlignParams, setTiltParams, updateMetaData.  Deleted functions:
 * <p> getFullImageSize, setFullImageSize, setNewstParams.
 * <p>
 * <p> Revision 3.29  2005/06/01 21:28:01  sueh
 * <p> bug# 667 Standardizing getMetaData function names.
 * <p>
 * <p> Revision 3.28  2005/04/21 20:55:25  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.27  2005/04/16 02:05:36  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.26  2005/03/29 19:54:59  sueh
 * <p> bug# 623 Added getBinning().
 * <p>
 * <p> Revision 3.25  2005/03/02 00:14:11  sueh
 * <p> bug# 533 disabled whole tomogram for montaging.
 * <p>
 * <p> Revision 3.24  2005/02/19 00:31:30  sueh
 * <p> bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * <p> and localalignments.  Add them for A and B.
 * <p>
 * <p> Revision 3.23  2005/01/14 03:11:45  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.22  2004/12/29 00:16:56  sueh
 * <p> bug# 567 Adapting to new tiltalignParam.
 * <p>
 * <p> Revision 3.21  2004/12/09 04:58:45  sueh
 * <p> bug# 565 Removed BaseManager.isDataParamDirty.  Automatically
 * <p> saving to param file on exit.
 * <p>
 * <p> Revision 3.20  2004/12/03 02:41:43  sueh
 * <p> bug# Added updateMetaData().  For new it just copies wholeTomogram to
 * <p> meta data.  Calling updateMetaData() from all getParam() functions.
 * <p>
 * <p> Revision 3.19  2004/12/02 20:42:52  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.18  2004/11/20 00:06:50  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.17.2.1  2004/10/11 02:19:56  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.17  2004/07/02 17:43:11  sueh
 * <p> bug# 461 scale z shift by preali binning
 * <p>
 * <p> Revision 3.16  2004/06/28 20:44:45  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.15  2004/06/28 20:40:44  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.14  2004/06/22 02:19:39  sueh
 * <p> bug# 459 switch tooltips on btnSample when switch button
 * <p> title.
 * <p>
 * <p> Revision 3.13  2004/06/22 02:07:12  sueh
 * <p> bug# 481 Fixed display error.
 * <p>
 * <p> Revision 3.12  2004/06/17 20:18:53  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.11  2004/06/17 18:50:08  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.10  2004/06/01 18:56:32  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.9  2004/05/25 23:25:52  rickg
 * <p> Bug #391 added fiducialess parameter interface and UI objects
 * <p>
 * <p> Revision 3.8  2004/05/03 18:24:57  sueh
 * <p> bug# 427 set TomopitchParam.ScaleFactor from binning
 * <p>
 * <p> Revision 3.7  2004/04/29 20:25:57  sueh
 * <p> bug# 427 added commented out testing code
 * <p>
 * <p> Revision 3.6  2004/04/27 01:02:46  sueh
 * <p> bugt# 427 added set and get TomopitchParam
 * <p>
 * <p> Revision 3.5  2004/04/26 18:32:05  rickg
 * <p> bug #426 Added full image code
 * <p>
 * <p> Revision 3.4  2004/04/26 00:20:21  rickg
 * <p> bug #426 Implemented full tomogram sampling
 * <p>
 * <p> Revision 3.3  2004/04/22 23:25:05  rickg
 * <p> bug #391 adding non-fid alignement capability
 * <p>
 * <p> Revision 3.2  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.1  2004/01/30 22:45:28  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.10  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.9  2003/10/29 00:28:32  rickg
 * <p> Bug# 297 Tooltips
 * <p>
 * <p> Revision 2.8  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.7  2003/10/28 00:23:47  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.6  2003/10/27 23:58:18  rickg
 * <p> Bug# 284 tooltips
 * <p>
 * <p> Revision 2.5  2003/10/14 23:15:24  rickg
 * <p> Bug# 282 Label fixes
 * <p>
 * <p> Revision 2.4  2003/06/05 21:07:12  rickg
 * <p> Label change to match log file
 * <p>
 * <p> Revision 2.3  2003/05/23 22:13:47  rickg
 * <p> Removed any extensions from log file labels in context menu
 * <p>
 * <p> Revision 2.2  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.7.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.7  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.6  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.5  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.4  2002/10/17 22:40:29  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.3  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.2  2002/09/19 21:37:57  rickg
 * <p> Removed stdout messages
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public final class TomogramPositioningDialog extends ProcessDialog implements
    ContextMenu, FiducialessParams, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private JPanel pnlPosition = new JPanel();

  private JPanel pnlTomoParams = new JPanel();

  private final LabeledTextField ltfSampleThickness = new LabeledTextField(
      "Sample tomogram thickness: ");
  private final LabeledTextField ltfExtraThickness = new LabeledTextField(
      "Added border thickness (unbinned): ");
  private final LabeledTextField ltfThickness = new LabeledTextField(
      "Final Tomogram Thickness: ");

  private CheckBox cbFiducialess = new CheckBox("Fiducialless alignment");
  private LabeledTextField ltfRotation = new LabeledTextField(
      "Tilt axis rotation:");

  private JPanel pnlWholeTomogram = new JPanel();

  private LabeledSpinner spinBinning;

  private CheckBox cbWholeTomogram = new CheckBox("Use whole tomogram");

  private JPanel pnlPositionButtons = new JPanel();

  private final MultiLineButton btnSample;

  private Run3dmodButton btnCreateBoundary = new Run3dmodButton(
      "<html><b>Create Boundary Model</b>", this);

  private final MultiLineButton btnTomopitch;
  private JPanel pnlFinalAlign = new JPanel();
  private final CalcPanel cpAngleOffset = new CalcPanel("Angle offset");
  private CalcPanel cpTiltAxisZShift = new CalcPanel("Z shift");
  private final CalcPanel cpXAxisTilt = new CalcPanel("X axis tilt");
  private final MultiLineButton btnAlign;
  private final TomogramPositioningExpert expert;
  static final String SAMPLE_TOMOGRAMS_TOOLTIP = "Build 3 sample tomograms for finding location and angles of section.";
  private final LocalActionListener localActionListener;
  private final CalcPanel cpTiltAngleOffset = new CalcPanel(
      "Tilt angle offset");
  private final CalcPanel cpZShift = new CalcPanel("Z shift");

  public TomogramPositioningDialog(ApplicationManager appMgr,
      TomogramPositioningExpert expert, AxisID axisID) {
    super(appMgr, axisID, DialogType.TOMOGRAM_POSITIONING);
    this.expert = expert;
    fixRootPanel(rootSize);
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnSample = (MultiLineButton) displayFactory.getSampleTomogram();
    btnTomopitch = (MultiLineButton) displayFactory.getComputePitch();
    btnAlign = (MultiLineButton) displayFactory.getFinalAlignment();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");
    //  Construct the binning spinner
    SpinnerModel integerModel = new SpinnerNumberModel(1, 1, 8, 1);
    spinBinning = new LabeledSpinner("   Binning ", integerModel);
    spinBinning
        .setTextMaxmimumSize(UIParameters.INSTANCE.getSpinnerDimension());

    //  Create the primary panels
    pnlWholeTomogram
        .setLayout(new BoxLayout(pnlWholeTomogram, BoxLayout.X_AXIS));
    //if (appMgr.getMetaData().getViewType() == ViewType.MONTAGE) {
    //  cbWholeTomogram.setEnabled(false);
    //}
    pnlWholeTomogram.add(cbWholeTomogram);
    pnlWholeTomogram.add(spinBinning.getContainer());

    pnlTomoParams.setLayout(new BoxLayout(pnlTomoParams, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlTomoParams, ltfSampleThickness.getContainer());
    UIUtilities.addWithYSpace(pnlTomoParams, cbFiducialess);
    UIUtilities.addWithYSpace(pnlTomoParams, ltfRotation.getContainer());
    UIUtilities.addWithYSpace(pnlTomoParams, pnlWholeTomogram);
    UIUtilities.alignComponentsX(pnlTomoParams, Component.LEFT_ALIGNMENT);

    pnlFinalAlign.setLayout(new BoxLayout(pnlFinalAlign, BoxLayout.Y_AXIS));
    pnlFinalAlign.setBorder(new EtchedBorder("Final Alignment").getBorder());
    UIUtilities.addWithYSpace(pnlFinalAlign, cpAngleOffset.getContainer());
    UIUtilities.addWithYSpace(pnlFinalAlign, cpTiltAxisZShift.getContainer());
    btnAlign.setSize();
    UIUtilities.addWithSpace(pnlFinalAlign, btnAlign.getComponent(),
        FixedDim.x0_y10);
    UIUtilities.alignComponentsX(pnlFinalAlign, Component.CENTER_ALIGNMENT);

    pnlPosition
        .setBorder(new BeveledBorder("Tomogram Positioning").getBorder());
    pnlPosition.setLayout(new BoxLayout(pnlPosition, BoxLayout.Y_AXIS));

    UIUtilities.addWithYSpace(pnlPosition, pnlTomoParams);
    UIUtilities.addWithSpace(pnlPosition, btnSample.getComponent(),
        FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlPosition, btnCreateBoundary.getComponent(),
        FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlPosition, ltfExtraThickness.getContainer(),
        FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlPosition, btnTomopitch.getComponent(),
        FixedDim.x0_y10);
    UIUtilities.addWithYSpace(pnlPosition, pnlFinalAlign);

    SpacedPanel pnlTiltParameters = new SpacedPanel();
    pnlTiltParameters.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTiltParameters
        .setBorder(new EtchedBorder("Tilt Parameters").getBorder());
    pnlTiltParameters.add(cpTiltAngleOffset.getContainer());
    pnlTiltParameters.add(cpZShift.getContainer());
    pnlTiltParameters.add(cpXAxisTilt.getContainer());
    pnlTiltParameters.add(ltfThickness);
    pnlPosition.add(pnlTiltParameters.getContainer());

    UIUtilities.alignComponentsX(pnlPosition, Component.CENTER_ALIGNMENT);
    UIUtilities.setButtonSizeAll(pnlPosition, UIParameters.INSTANCE
        .getButtonDimension());

    //  Create dialog content pane
    rootPanel.add(pnlPosition);
    addExitButtons();

    // Bind the buttons to the action listener
    localActionListener = new LocalActionListener(this);
    cbFiducialess.addActionListener(localActionListener);
    cbWholeTomogram.addActionListener(localActionListener);
    btnSample.addActionListener(localActionListener);
    btnCreateBoundary.addActionListener(localActionListener);
    btnTomopitch.addActionListener(localActionListener);
    btnAlign.addActionListener(localActionListener);
    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }

  public static ProcessResultDisplay getSampleTomogramDisplay() {
    return MultiLineButton.getToggleButtonInstance("Create Sample Tomograms",
        DialogType.TOMOGRAM_POSITIONING);
  }

  public static ProcessResultDisplay getComputePitchDisplay() {
    return MultiLineButton.getToggleButtonInstance(
        "Compute Z Shift & Pitch Angles", DialogType.TOMOGRAM_POSITIONING);
  }

  public static ProcessResultDisplay getFinalAlignmentDisplay() {
    return MultiLineButton.getToggleButtonInstance("Create Final Alignment",
        DialogType.TOMOGRAM_POSITIONING);
  }

  public void setFiducialessAlignment(boolean fiducialess) {
    cbFiducialess.setSelected(fiducialess);
    expert.updateFiducialessDisplay(fiducialess);
  }

  public void setFinalAlignmentPanelVisible(boolean visible) {
    pnlFinalAlign.setVisible(visible);
  }

  public boolean isFiducialess() {
    return cbFiducialess.isSelected();
  }

  public void setImageRotation(float tiltAxisAngle) {
    ltfRotation.setText(tiltAxisAngle);
  }

  public float getImageRotation() throws NumberFormatException {
    return Float.parseFloat(ltfRotation.getText());
  }
  
  String getOrigTiltAngleOffset() {
    return cpTiltAngleOffset.getOriginal();
  }
  
  String getOrigZShift() {
    return cpZShift.getOriginal();
  }

  void setAngleOffset(ConstEtomoNumber angleOffset) {
    cpAngleOffset.set(angleOffset);
  }

  void setTiltAxisZShift(ConstEtomoNumber axisZShift) {
    cpTiltAxisZShift.set(axisZShift);
  }

  void setXAxisTilt(double xAxisTilt) {
    cpXAxisTilt.set(xAxisTilt);
  }

  void setThickness(int thickness) {
    ltfThickness.setText(thickness);
  }
  
  void setThicknessEnabled(boolean enable) {
    ltfThickness.setEnabled(enable);
  }
  
  void setTiltAngleOffset(ConstEtomoNumber tiltAngleOffset) {
    cpTiltAngleOffset.set(tiltAngleOffset);
  }
  
  boolean setTiltAngleOffset(TomopitchLog log) {
    return cpTiltAngleOffset.set(log.getAngleOffsetOriginal(), log
        .getAngleOffsetAdded(), log.getAngleOffsetTotal());
  }
  
  void setTiltAngleOffsetEnabled(boolean enable) {
    cpTiltAngleOffset.setEnabled(enable);
  }
  
  void setTiltAngleOffsetVisible(boolean visible) {
    cpTiltAngleOffset.setVisible(visible);
  }

  void setThickness(ConstEtomoNumber thickness) {
    ltfThickness.setText(thickness);
  }

  String getAngleOffsetTotal() {
    return cpAngleOffset.getTotal();
  }

  String getTiltAxisZShiftTotal() {
    return cpTiltAxisZShift.getTotal();
  }

  String getExtraThickness() {
    return ltfExtraThickness.getText();
  }

  String getXAxisTiltTotal() {
    return cpXAxisTilt.getTotal();
  }
  
  String getZShift() {
    return cpZShift.getTotal();
  }

  String getThickness() {
    return ltfThickness.getText();
  }
  
  String getTiltAngleOffset() {
    return cpTiltAngleOffset.getTotal();
  }

  String getSampleThickness() {
    return ltfSampleThickness.getText();
  }

  boolean isWholeTomogram() {
    return cbWholeTomogram.isSelected();
  }

  int getBinningValue() {
    return ((Integer) spinBinning.getValue()).intValue();
  }

  void setBinning(ConstEtomoNumber binning) {
    spinBinning.setValue(binning);
  }

  void setBinning(int binning) {
    spinBinning.setValue(binning);
  }

  void setSampleThickness(ConstEtomoNumber sampleThickness) {
    ltfSampleThickness.setText(sampleThickness.toString());
  }

  boolean isTomopitchButton() {
    return btnTomopitch.isSelected();
  }

  boolean isAlignButton() {
    return btnAlign.isSelected();
  }

  boolean isAlignButtonEnabled() {
    return btnAlign.isEnabled();
  }

  void setSampleButtonState(ReconScreenState screenState) {
    btnSample.setButtonState(screenState.getButtonState(btnSample
        .getButtonStateKey()));
  }

  void setTomopitchButtonState(ReconScreenState screenState) {
    btnTomopitch.setButtonState(screenState.getButtonState(btnTomopitch
        .getButtonStateKey()));
  }
  
  void setTomopitchEnabled(boolean enable) {
    btnTomopitch.setEnabled(enable);
  }

  void setAlignButtonState(ReconScreenState screenState) {
    btnAlign.setButtonState(screenState.getButtonState(btnAlign
        .getButtonStateKey()));
  }

  void updateTiltAngleOffsetDisplay(boolean more) {
    cpAngleOffset.updateDisplay(more);
  }

  void updateTiltAxisZShiftDisplay(boolean more) {
    cpTiltAxisZShift.updateDisplay(more);
  }

  void updateXAxisTiltDisplay(boolean more) {
    cpXAxisTilt.updateDisplay(more);
  }

  boolean setAngleOffset(TomopitchLog log) {
    return cpAngleOffset.set(log.getAngleOffsetOriginal(), log
        .getAngleOffsetAdded(), log.getAngleOffsetTotal());
  }

  boolean setTiltAxisZShift(TomopitchLog log) {
    return cpTiltAxisZShift.set(log.getAxisZShiftOriginal(), log
        .getAxisZShiftAdded(), log.getAxisZShiftTotal());
  }

  boolean setXAxisTilt(TomopitchLog log) {
    return cpXAxisTilt.set(log.getXAxisTiltOriginal(), log.getXAxisTiltAdded(),
        log.getXAxisTiltTotal());
  }

  void setExtraThickness(String extraThickness) {
    ltfExtraThickness.setText(extraThickness);
  }

  /**
   * Set the whole tomogram sampling state
   * @param state
   */
  public void setWholeTomogram(boolean state) {
    cbWholeTomogram.setSelected(state);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Tomopitch", "Newstack", "3dmod", "Tilt" };
    String[] manPage = { "tomopitch.html", "newstack.html", "3dmod.html",
        "tilt.html" };
    String[] logFileLabel = { "Tomopitch", "Sample" };
    String[] logFile = new String[2];
    logFile[0] = "tomopitch" + axisID.getExtension() + ".log";
    logFile[1] = "sample" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "TOMOGRAM POSITIONING", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }

  private void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnCreateBoundary.getActionCommand())) {
      expert.createBoundary(menuOptions);
    }
  }

  //  Button action handler methods
  protected void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnSample.getActionCommand())) {
      expert.sampleAction(btnSample);
    }
    else if (command.equals(btnTomopitch.getActionCommand())) {
      expert.tomopitch(btnTomopitch);
    }
    else if (command.equals(btnAlign.getActionCommand())) {
      expert.finalAlign(btnAlign);
    }
    else if (command.equals(cbFiducialess.getActionCommand())) {
      expert.fiducialessAction();
    }
    else if (command.equals(cbWholeTomogram.getActionCommand())) {
      expert.updateFiducialessDisplay(cbFiducialess.isSelected());
    }
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  void setAngleOffsetEnabled(boolean enabled) {
    cpAngleOffset.setEnabled(enabled);
  }

  void setTiltAxisZShiftEnabled(boolean enabled) {
    cpTiltAxisZShift.setEnabled(enabled);
  }

  void setXAxisTiltEnabled(boolean enabled) {
    cpXAxisTilt.setEnabled(enabled);
  }
  
  void setZShift(ConstEtomoNumber zShift) {
    cpZShift.set(zShift);
  }
  
  boolean setZShift(TomopitchLog log) {
    return cpZShift.set(log.getAxisZShiftOriginal(), log
        .getAxisZShiftAdded(), log.getAxisZShiftTotal());
  }
  
  void setZShiftEnabled(boolean enable) {
    cpZShift.setEnabled(enable);
  }
  
  void setZShiftVisible(boolean visible) {
    cpZShift.setVisible(visible);
  }

  void setAlignButtonEnabled(boolean enabled) {
    btnAlign.setEnabled(enabled);
  }

  void setRotationEnabled(boolean enabled) {
    ltfRotation.setEnabled(enabled);
  }

  void setBinningEnabled(boolean enabled) {
    spinBinning.setEnabled(enabled);
  }

  void setSampleButton(String label) {
    btnSample.setText(label);
  }

  void setSampleButtonToolTip(String formattedToolTip) {
    btnSample.setToolTipText(formattedToolTip);
  }

  public void done() {
    if (expert.doneDialog()) {
      btnSample.removeActionListener(localActionListener);
      btnTomopitch.removeActionListener(localActionListener);
      btnAlign.removeActionListener(localActionListener);
      setDisplayed(false);
    }
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  private void updateAdvanced() {
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  //
  //	Action listener adapters
  //
  class LocalActionListener implements ActionListener {
    TomogramPositioningDialog adaptee;

    LocalActionListener(TomogramPositioningDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    text = "Thickness of sample slices.  Make this much larger than expected section"
        + " thickness to see borders of section.";
    ltfSampleThickness.setToolTipText(tooltipFormatter.setText(text).format());
    text = SAMPLE_TOMOGRAMS_TOOLTIP;
    btnSample.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Open samples in 3dmod to make a model with lines along top and bottom "
        + "edges of the section in each sample.";
    btnCreateBoundary.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Run tomopitch.  You need to examine the log file to determine the"
        + "Z shift, additional angle offset, and X-axis tilt.";
    btnTomopitch.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Add the additional offset from tomopitch to the amount already "
        + "shown here to get the total offset.";
    cpAngleOffset.setToolTipText(tooltipFormatter.setText(text).format());
    cpXAxisTilt.setToolTipText(tooltipFormatter.setText(
        TomogramGenerationDialog.X_AXIS_TILT_TOOLTIP).format());
    text = "Add the additional shift from tomopitch to the amount shown here to get "
        + "the total shift.";
    cpTiltAxisZShift.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Run tiltalign with these final offset parameters.";
    btnAlign.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Generate an entire tomogram instead of 3 samples and draw boundary "
        + "lines in this tomogram.";
    cbWholeTomogram.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Set the binning for the whole tomogram to be used for positioning."
        + "  With a binned tomogram, the tomopitch output and entries for "
        + "offset and thickness will still be in unbinned pixels.";
    spinBinning.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Use cross-correlation alignment only.";
    cbFiducialess.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Rotation angle of tilt axis for generating aligned stack from "
        + "cross-correlation alignment only.";
    ltfRotation.setToolTipText(tooltipFormatter.setText(text).format());
    ltfExtraThickness
        .setToolTipText(tooltipFormatter
            .setText(
                "Extra thickness to be added to the top and bottom of the final tomogram.")
            .format());
    ltfThickness.setToolTipText(tooltipFormatter.setText(
        "The thickness of the final tomogram.").format());
  }

  public static final class CalcPanel {
    public static final String ADDED_KEY = "Added";
    public static final int MAX_DIGITS = 6;
    private final SpacedPanel panel = new SpacedPanel();
    private final JLabel label;
    private final LabeledTextField ltfOriginal = new LabeledTextField(
        "Original:");
    private final LabeledTextField ltfAdded = new LabeledTextField(
        ADDED_KEY + ':');
    private final LabeledTextField ltfTotal = new LabeledTextField("Total:");
    //utility field
    private final EtomoNumber number = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    private boolean more = true;

    CalcPanel(String label) {
      this.label = new JLabel(label + ":");
      panel.setBoxLayout(BoxLayout.X_AXIS);
      panel.add(this.label);
      panel.addRigidArea();
      panel.add(ltfOriginal);
      panel.add(ltfAdded);
      panel.add(ltfTotal);
      ltfOriginal.setEditable(false);
      ltfOriginal.setColumns(MAX_DIGITS);
      ltfAdded.setEditable(false);
      ltfAdded.setColumns(MAX_DIGITS);
      ltfTotal.setColumns(MAX_DIGITS);
      ltfOriginal.setText("0.0");
      ltfAdded.setText("0.0");
      ltfTotal.setText("0.0");
      updateDisplay(false);
    }

    Container getContainer() {
      return panel.getContainer();
    }

    void setEnabled(boolean enabled) {
      ltfTotal.setEnabled(enabled);
    }

    void setToolTipText(String text) {
      label.setToolTipText(text);
      ltfOriginal.setToolTipText(text);
      ltfAdded.setToolTipText(text);
      ltfTotal.setToolTipText(text);
    }
    
    void setVisible(boolean visible) {
      panel.setVisible(visible);
    }

    void set(ConstEtomoNumber total) {
      updateDisplay(false);
      set(total, ltfTotal);
    }

    void set(double total) {
      number.set(total);
      set(number);
    }

    boolean set(ConstEtomoNumber original, ConstEtomoNumber added,
        ConstEtomoNumber total) {
      if (total.isNull()) {
        return false;
      }
      updateDisplay(true);
      set(original, ltfOriginal);
      set(added, ltfAdded);
      set(total, ltfTotal);
      return true;
    }

    private void set(ConstEtomoNumber number, LabeledTextField field) {
      if (number == null || number.isNull() || !number.isValid()) {
        field.setText("0.0");
      }
      else {
        field.setText(number.toString());
      }
    }

    void updateDisplay(boolean more) {
      if (this.more != more) {
        this.more = more;
        ltfOriginal.setVisible(more);
        ltfAdded.setVisible(more);
      }
    }

    String getOriginal() {
      return ltfOriginal.getText();
    }

    String getAdded() {
      return ltfAdded.getText();
    }

    String getTotal() {
      return ltfTotal.getText();
    }

    double getTotalDouble() {
      return number.set(ltfTotal.getText()).getDouble();
    }
  }
}
