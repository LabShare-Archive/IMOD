package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.TiltParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.util.InvalidParameterException;

/**
 * <p>
 * Description:
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
 * Revision 3.2  2004/02/13 00:09:26  rickg
 * Updated for PIP based newstack
 *
 * Revision 3.1  2004/01/30 22:45:23  sueh
 * bug# 356 Changing buttons with html labels to
 * MultiLineButton and MultiLineToggleButton
 *
 * Revision 3.0  2003/11/07 23:19:01  rickg
 * Version 1.0.0
 *
 * Revision 2.19  2003/11/07 19:54:02  rickg
 * Don't delete preali in delete aligned stacks code.
 *
 * Revision 2.18  2003/10/30 01:43:44  rickg
 * Bug# 338 Remapped context menu entries
 *
 * Revision 2.17  2003/10/28 23:35:48  rickg
 * Bug# 336 Context menu label capitalization
 *
 * Revision 2.16  2003/10/22 21:31:02  rickg
 * Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 *
 * Revision 2.15  2003/10/21 23:41:28  rickg
 * Bug# 288 Tooltips
 * Bug# 296 Added button to delete .preal and .ali
 *
 * <p>
 * Revision 2.14 2003/10/14 23:45:01 rickg
 * <p>
 * Bug# 285 Added view aligned stack button
 * <p>
 * <p>
 * Revision 2.13 2003/10/14 22:53:55 rickg
 * <p>
 * Bug #286 Label changes
 * <p>
 * <p>
 * Revision 2.12 2003/10/02 18:57:47 sueh
 * <p>
 * bug236 added testing:
 * <p>
 * NewstParamTest
 * <p>
 * ComScriptTest
 * <p>
 * <p>
 * Removed marks
 * <p>
 * <p>
 * Revision 2.11 2003/09/29 23:34:57 sueh
 * <p>
 * bug236 Added UseLinearInterpolation to
 * <p>
 * TomogramGenerationDialog.
 * <p>
 * <p>
 * UseLinearInterpolation:
 * <p>
 * check box
 * <p>
 * Advanced
 * <p>
 * newst -linear
 * <p>
 * <p>
 * Files:
 * <p>
 * ComScriptManager.java
 * <p>
 * ConstNewstParam.java
 * <p>
 * NewstParam.java
 * <p>
 * TomogramGenerationDialog.java
 * <p>
 * ApplicationManager.java
 * <p>
 * <p>
 * Revision 2.10 2003/09/08 22:51:25 rickg
 * <p>
 * Added commit test volume action
 * <p>
 * <p>
 * Revision 2.9 2003/09/08 05:47:09 rickg
 * <p>
 * Added trial tilt
 * <p>
 * Output for a single axis tomogram is changed to
 * <p>
 * dataset_full.rec
 * <p>
 * <p>
 * Revision 2.8 2003/06/25 22:14:57 rickg
 * <p>
 * Constructed a panel for the tilt parameters
 * <p>
 * <p>
 * Revision 2.7 2003/06/23 23:26:59 rickg
 * <p>
 * Added advanced options/parameters
 * <p>
 * <p>
 * Revision 2.6 2003/05/23 22:14:11 rickg
 * <p>
 * Removed any extensions from log file labels in context menu
 * <p>
 * <p>
 * Revision 2.5 2003/05/23 21:26:55 rickg
 * <p>
 * *** empty log message ***
 * <p>
 * <p>
 * Revision 2.4 2003/04/28 23:25:25 rickg
 * <p>
 * Changed visible imod references to 3dmod
 * <p>
 * <p>
 * Revision 2.3 2003/04/24 17:46:54 rickg
 * <p>
 * Changed fileset name to dataset name
 * <p>
 * <p>
 * Revision 2.2 2003/03/02 23:30:41 rickg
 * <p>
 * Combine layout in progress
 * <p>
 * <p>
 * Revision 2.1 2003/01/24 21:04:18 rickg
 * <p>
 * AxisID bug fix from single buttonAction function
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.6.2.1 2003/01/24 18:43:37 rickg
 * <p>
 * Single window GUI layout initial revision
 * <p>
 * <p>
 * Revision 1.6 2002/12/19 17:45:22 rickg
 * <p>
 * Implemented advanced dialog state processing
 * <p>
 * including:
 * <p>
 * default advanced state set on start up
 * <p>
 * advanced button management now handled by
 * <p>
 * super class
 * <p>
 * <p>
 * Revision 1.5 2002/12/19 00:30:26 rickg
 * <p>
 * app manager and root pane moved to super class
 * <p>
 * <p>
 * Revision 1.4 2002/11/14 21:18:37 rickg
 * <p>
 * Added anchors into the tomoguide
 * <p>
 * t
 * <p>
 * Revision 1.3 2002/10/17 22:40:22 rickg
 * <p>
 * Added fileset name to window title
 * <p>
 * this reference removed applicationManager messages
 * <p>
 * <p>
 * Revision 1.2 2002/10/07 22:31:18 rickg
 * <p>
 * removed unused imports
 * <p>
 * reformat after emacs trashed it
 * <p>
 * <p>
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * <p>
 * Initial CVS entry, basic functionality not including combining
 * <p>
 * </p>
 */
public class TomogramGenerationDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel pnlTilt = new JPanel();
  private JPanel pnlNewstParams = new JPanel();
  private BeveledBorder border = new BeveledBorder("Tomogram Generation");

  private JCheckBox cbBoxUseLinearInterpolation =
    new JCheckBox("Use linear interpolation");

  private JPanel pnlAlignedStack = new JPanel();
  private MultiLineToggleButton btnNewst =
    new MultiLineToggleButton("<html><b>Create Full<br>Aligned Stack</b>");
  private MultiLineButton btn3dmodFull =
    new MultiLineButton("<html><b>View Full<br>Aligned Stack</b>");

  private JPanel pnlTiltParams = new JPanel();

  private LabeledTextField ltfXOffset = new LabeledTextField("X offset: ");
  private LabeledTextField ltfZOffset = new LabeledTextField("Z offset: ");
  private LabeledTextField ltfSliceStart =
    new LabeledTextField("First slice in Y: ");
  private LabeledTextField ltfSliceStop =
    new LabeledTextField("Last slice in Y: ");
  private LabeledTextField ltfSliceIncr =
    new LabeledTextField("Slice step in Y: ");

  private LabeledTextField ltfTomoWidth =
    new LabeledTextField("Tomogram width in X: ");
  private LabeledTextField ltfTomoThickness =
    new LabeledTextField("Tomogram thickness in Z: ");

  private LabeledTextField ltfXAxisTilt = new LabeledTextField("X axis tilt: ");

  private LabeledTextField ltfTiltAngleOffset =
    new LabeledTextField("Tilt angle offset: ");

  private LabeledTextField ltfRadialMax =
    new LabeledTextField("Radial filter cutoff: ");
  private LabeledTextField ltfRadialFallOff =
    new LabeledTextField("Radial filter falloff: ");
  private LabeledTextField ltfDensityOffset =
    new LabeledTextField("Output density offset: ");
  private LabeledTextField ltfDensityScale =
    new LabeledTextField("Output density scaling factor: ");
  private LabeledTextField ltfLogOffset = new LabeledTextField("Log offset: ");

  private JCheckBox cbBoxUseLocalAlignment =
    new JCheckBox("Use local alignments");

  private JPanel pnlTrial = new JPanel();
  private JPanel pnlTrialTomogramName = new JPanel();
  private JLabel lblTrialTomogramName = new JLabel("Trial tomogram filename: ");
  private JComboBox cmboTrialTomogramName = new JComboBox();

  private Vector trialTomogramList = new Vector();

  private JPanel pnlTrialButtons = new JPanel();
  private MultiLineButton btnTrial =
    new MultiLineButton("<html><b>Generate Trial Tomogram</b>");
  private MultiLineButton btn3dmodTrial =
    new MultiLineButton("<html><b>View Trial in 3dmod</b>");
  private MultiLineToggleButton btnUseTrial =
    new MultiLineToggleButton("<html><b>Use Current Trial Tomogram</b>");

  private MultiLineToggleButton btnTilt =
    new MultiLineToggleButton("<html><b>Generate Tomogram</b>");
  private MultiLineButton btn3dmodTomogram =
    new MultiLineButton("<html><b>View Tomogram In 3dmod</b>");

  private MultiLineToggleButton btnDeleteStacks =
    new MultiLineToggleButton("<html><b>Delete Aligned Image Stack</b>");

  public TomogramGenerationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");

    cbBoxUseLinearInterpolation.setAlignmentX(Component.CENTER_ALIGNMENT);
    cbBoxUseLocalAlignment.setAlignmentX(Component.CENTER_ALIGNMENT);
    //btnTrial.setAlignmentX(Component.CENTER_ALIGNMENT);
    //btn3dmodTrial.setAlignmentX(Component.CENTER_ALIGNMENT);
    //btnUseTrial.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTilt.setAlignmentX(Component.CENTER_ALIGNMENT);
    btn3dmodTomogram.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnDeleteStacks.setAlignmentX(Component.CENTER_ALIGNMENT);

    //  Set the button sizes
    Dimension dimButton = UIParameters.getButtonDimension();
    btnNewst.setPreferredSize(dimButton);
    btnNewst.setMaximumSize(dimButton);
    btn3dmodFull.setPreferredSize(dimButton);
    btn3dmodFull.setMaximumSize(dimButton);
    btnTrial.setPreferredSize(dimButton);
    btnTrial.setMaximumSize(dimButton);
    btn3dmodTrial.setPreferredSize(dimButton);
    btn3dmodTrial.setMaximumSize(dimButton);
    btnUseTrial.setPreferredSize(dimButton);
    btnUseTrial.setMaximumSize(dimButton);
    btnTilt.setPreferredSize(dimButton);
    btnTilt.setMaximumSize(dimButton);
    btn3dmodTomogram.setPreferredSize(dimButton);
    btn3dmodTomogram.setMaximumSize(dimButton);
    btnDeleteStacks.setPreferredSize(dimButton);
    btnDeleteStacks.setMaximumSize(dimButton);

    // Bind the buttons to the action listener
    ButtonListener tomogramGenerationListener = new ButtonListener(this);
    btnNewst.addActionListener(tomogramGenerationListener);
    btn3dmodFull.addActionListener(tomogramGenerationListener);
    btnTrial.addActionListener(tomogramGenerationListener);
    btn3dmodTrial.addActionListener(tomogramGenerationListener);
    btnUseTrial.addActionListener(tomogramGenerationListener);
    btnTilt.addActionListener(tomogramGenerationListener);
    btn3dmodTomogram.addActionListener(tomogramGenerationListener);
    btnDeleteStacks.addActionListener(tomogramGenerationListener);

    // Layout the newst button panel
    pnlAlignedStack.setLayout(new BoxLayout(pnlAlignedStack, BoxLayout.X_AXIS));
    pnlAlignedStack.add(btnNewst);
    pnlAlignedStack.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlAlignedStack.add(btn3dmodFull);

    // Layout the trial panel
    pnlTrial.setLayout(new BoxLayout(pnlTrial, BoxLayout.Y_AXIS));
    pnlTrial.setBorder(new EtchedBorder("Trial Mode").getBorder());
    cmboTrialTomogramName.setEditable(true);
    pnlTrialTomogramName.setLayout(
      new BoxLayout(pnlTrialTomogramName, BoxLayout.X_AXIS));
    pnlTrialTomogramName.add(lblTrialTomogramName);
    pnlTrialTomogramName.add(cmboTrialTomogramName);

    pnlTrialButtons.setLayout(new BoxLayout(pnlTrialButtons, BoxLayout.X_AXIS));
    pnlTrialButtons.add(btnTrial);
    pnlTrialButtons.add(btn3dmodTrial);
    pnlTrialButtons.add(btnUseTrial);

    pnlTrial.add(pnlTrialTomogramName);
    pnlTrial.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTrial.add(pnlTrialButtons);

    layoutTiltPanel();
    layoutNewstPanel();

    pnlTilt.setBorder(border.getBorder());
    pnlNewstParams.setLayout(new BoxLayout(pnlNewstParams, BoxLayout.Y_AXIS));
    pnlTilt.setLayout(new BoxLayout(pnlTilt, BoxLayout.Y_AXIS));
    pnlTiltParams.setLayout(new BoxLayout(pnlTiltParams, BoxLayout.Y_AXIS));

    pnlTilt.add(pnlNewstParams);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTilt.add(pnlAlignedStack);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTilt.add(pnlTiltParams);
    pnlTilt.add(btnTilt);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTilt.add(btn3dmodTomogram);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTilt.add(btnDeleteStacks);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));

    rootPanel.add(pnlTilt);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }

  public void setNewstParams(ConstNewstParam newstParam) {
    cbBoxUseLinearInterpolation.setSelected(
      newstParam.isLinearInterpolation());
  }
  /**
   * Populate the dialog box with the tilt paramaters
   */
  public void setTiltParams(ConstTiltParam tiltParam) {
    if (tiltParam.hasWidth()) {
      ltfTomoWidth.setText(tiltParam.getWidth());
    }
    if (tiltParam.hasThickness()) {
      ltfTomoThickness.setText(tiltParam.getThickness());
    }
    if (tiltParam.hasXOffset()) {
      ltfXOffset.setText(tiltParam.getXOffset());
    }
    if (tiltParam.hasZOffset()) {
      ltfZOffset.setText(tiltParam.getZOffset());
    }
    if (tiltParam.hasSlice()) {
      ltfSliceStart.setText(tiltParam.getIdxSliceStart());
      ltfSliceStop.setText(tiltParam.getIdxSliceStop());
    }
    if (tiltParam.hasSliceIncr()) {
      ltfSliceIncr.setText(tiltParam.getIdxSliceIncr());
    }
    if (tiltParam.hasXAxisTilt()) {
      ltfXAxisTilt.setText(tiltParam.getXAxisTilt());
    }
    if (tiltParam.hasTiltAngleOffset()) {
      ltfTiltAngleOffset.setText(tiltParam.getTiltAngleOffset());
    }
    if (tiltParam.hasRadialWeightingFunction()) {
      ltfRadialMax.setText(tiltParam.getRadialBandwidth());
      ltfRadialFallOff.setText(tiltParam.getRadialFalloff());
    }
    if (tiltParam.hasScale()) {
      ltfDensityOffset.setText(tiltParam.getScaleFLevel());
      ltfDensityScale.setText(tiltParam.getScaleCoeff());
    }
    if (tiltParam.hasLogOffset()) {
      ltfLogOffset.setText(tiltParam.getLogShift());
    }
    cbBoxUseLocalAlignment.setSelected(tiltParam.getUseLocalAlignFile());
  }

  public void getNewstParams(NewstParam newstParam) {
    newstParam.setLinearInterpolation(
      cbBoxUseLinearInterpolation.isSelected());
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  public void getTiltParams(TiltParam tiltParam)
    throws NumberFormatException, InvalidParameterException {
    String badParameter = "";
    try {

      if (ltfTomoWidth.getText().matches("\\S+")) {
        badParameter = ltfTomoWidth.getLabel();
        tiltParam.setWidth(Integer.parseInt(ltfTomoWidth.getText()));
      }
      else {
        tiltParam.useWidth(false);
      }

      if (ltfXOffset.getText().matches("\\S+")) {
        badParameter = ltfXOffset.getLabel();
        tiltParam.setXOffset(Double.parseDouble(ltfXOffset.getText()));
        if (ltfZOffset.getText().matches("\\S+")) {
          badParameter = ltfZOffset.getLabel();
          tiltParam.setZOffset(Double.parseDouble(ltfZOffset.getText()));
        }
        else {
          tiltParam.useZOffset(false);
        }
      }
      else {
        tiltParam.useXOffset(false);
        if (ltfZOffset.getText().matches("\\S+")) {
          throw (
            new InvalidParameterException("You must supply an X offset to supply a Z offset"));
        }
        else {
          tiltParam.useZOffset(false);
        }
      }
      boolean sliceRangeSpecified = false;
      if (ltfSliceStart.getText().matches("\\S+")
        && ltfSliceStop.getText().matches("\\S+")) {
        badParameter = ltfSliceStart.getLabel();
        tiltParam.setIdxSliceStart(Integer.parseInt(ltfSliceStart.getText()));
        badParameter = ltfSliceStop.getLabel();
        tiltParam.setIdxSliceStop(Integer.parseInt(ltfSliceStop.getText()));
        sliceRangeSpecified = true;
      }
      else if (
        ltfSliceStart.getText().matches("^\\s*$")
          && ltfSliceStop.getText().matches("^\\s*$")) {
        tiltParam.useSlice(false);
      }
      else {
        throw (
          new InvalidParameterException("You must supply both the first and last slices if you want to specify either."));
      }
      if (ltfSliceIncr.getText().matches("\\S+")) {
        if (sliceRangeSpecified) {
          badParameter = ltfSliceIncr.getLabel();
          tiltParam.setIdxSliceIncr(Integer.parseInt(ltfSliceIncr.getText()));
        }
        else {
          throw (
            new InvalidParameterException("You must supply both the first and last slices to specify the slice step."));
        }
      }
      else {
        tiltParam.useSliceIncr(false);
      }

      if (ltfTomoThickness.getText().matches("\\S+")) {
        badParameter = ltfTomoThickness.getLabel();
        tiltParam.setThickness(Integer.parseInt(ltfTomoThickness.getText()));
      }
      else {
        tiltParam.useThickness(false);
      }

      if (ltfXAxisTilt.getText().matches("\\S+")) {
        badParameter = ltfXAxisTilt.getLabel();
        tiltParam.setXAxisTilt(Double.parseDouble(ltfXAxisTilt.getText()));
      }
      else {
        tiltParam.useXAxisTilt(false);
      }

      if (ltfTiltAngleOffset.getText().matches("\\S+")) {
        badParameter = ltfTiltAngleOffset.getLabel();
        tiltParam.setTiltAngleOffset(
          Double.parseDouble(ltfTiltAngleOffset.getText()));
      }
      else {
        tiltParam.useAngleOffsets(false);
      }

      //    TODO: Error checking to be sure that all parameters are supplied
      if (ltfRadialMax.getText().matches("\\S+")
        || ltfRadialFallOff.getText().matches("\\S+")) {
        badParameter = ltfRadialMax.getLabel();
        tiltParam.setRadialBandwidth(
          Double.parseDouble(ltfRadialMax.getText()));
        badParameter = ltfRadialFallOff.getLabel();
        tiltParam.setRadialFalloff(
          Double.parseDouble(ltfRadialFallOff.getText()));
      }
      else {
        tiltParam.useRadialWeightingFunction(false);
      }

      //    TODO: Error checking to be sure that all parameters are supplied
      if (ltfDensityOffset.getText().matches("\\S+")
        || ltfDensityScale.getText().matches("\\S+")) {
        badParameter = ltfDensityScale.getLabel();
        tiltParam.setScaleCoeff(Double.parseDouble(ltfDensityScale.getText()));
        badParameter = ltfDensityOffset.getLabel();
        tiltParam.setScaleFLevel(
          Double.parseDouble(ltfDensityOffset.getText()));
      }
      else {
        tiltParam.useScale(false);
      }

      if (ltfLogOffset.getText().matches("\\S+")) {
        badParameter = ltfLogOffset.getLabel();
        tiltParam.setLogShift(Double.parseDouble(ltfLogOffset.getText()));
      }
      else {
        tiltParam.useLogOffset(false);
      }

      if (cbBoxUseLocalAlignment.isSelected()) {
        tiltParam.setLocalAlignFile(
          applicationManager.getDatasetName()
            + axisID.getExtension()
            + "local.xf");
      }
      else {
        tiltParam.setLocalAlignFile("");
      }
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  /**
   * Return the selected trial tomogram name
   * 
   * @return
   */
  public String getTrialTomogramName() {
    String trialTomogramName = (String) cmboTrialTomogramName.getSelectedItem();
    if (trialTomogramName == null) {
      trialTomogramName = "";
    }
    return trialTomogramName;
  }
  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    layoutTiltPanel();
    layoutNewstPanel();
    applicationManager.packMainWindow();
  }

  private void layoutNewstPanel() {
    pnlNewstParams.removeAll();
    if (isAdvanced) {
      pnlNewstParams.add(cbBoxUseLinearInterpolation);
    }
  }

  private void layoutTiltPanel() {
    pnlTiltParams.removeAll();
    if (isAdvanced) {
      pnlTiltParams.add(ltfLogOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfDensityScale.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfDensityOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfTomoThickness.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfTomoWidth.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStart.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStop.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceIncr.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfXOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfZOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfXAxisTilt.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfTiltAngleOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfRadialMax.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfRadialFallOff.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(cbBoxUseLocalAlignment);
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(pnlTrial);
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    }
    else {
      pnlTiltParams.add(ltfTomoThickness.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
      pnlTiltParams.add(ltfXAxisTilt.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
      pnlTiltParams.add(ltfRadialMax.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
      pnlTiltParams.add(ltfRadialFallOff.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
      pnlTiltParams.add(cbBoxUseLocalAlignment);
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Newst", "Tilt", "3dmod" };
    String[] manPage = { "newst.html", "tilt.html", "3dmod.html" };

    String[] logFileLabel = { "Newst", "Tilt" };
    String[] logFile = new String[2];
    logFile[0] = "newst" + axisID.getExtension() + ".log";
    logFile[1] = "tilt" + axisID.getExtension() + ".log";

    ContextPopup contextPopup =
      new ContextPopup(
        rootPanel,
        mouseEvent,
        "TOMOGRAM GENERATION",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  //  Action function overides for process state buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneTomogramGenerationDialog(axisID);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneTomogramGenerationDialog(axisID);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneTomogramGenerationDialog(axisID);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  //  Button handler function
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnNewst.getActionCommand())) {
      applicationManager.newst(axisID);
    }
    else if (command.equals(btn3dmodFull.getActionCommand())) {
      applicationManager.imodFineAlign(axisID);
    }
    else if (command.equals(btnTrial.getActionCommand())) {
      String trialTomogramName = getTrialTomogramName();
      if (trialTomogramName == "") {
        String[] errorMessage = new String[2];
        errorMessage[0] = "Missing trial tomogram filename:";
        errorMessage[1] =
          "A filename for the trial tomogram must be entered in the Trial"
            + " tomogram filename edit box.";
        applicationManager.openMessageDialog(
          errorMessage,
          "Tilt Parameter Syntax Error");
        return;
      }
      if (!trialTomogramList.contains(trialTomogramName)) {
        trialTomogramList.add(trialTomogramName);
        cmboTrialTomogramName.addItem(trialTomogramName);
      }
      applicationManager.trialTilt(axisID);
    }
    else if (command.equals(btn3dmodTrial.getActionCommand())) {
      applicationManager.imodTestVolume(axisID);
    }
    else if (command.equals(btnUseTrial.getActionCommand())) {
      applicationManager.commitTestVolume(axisID);
    }
    else if (command.equals(btnTilt.getActionCommand())) {
      applicationManager.tilt(axisID);
    }
    else if (command.equals(btn3dmodTomogram.getActionCommand())) {
      applicationManager.imodFullVolume(axisID);
    }
    else if (command.equals(btnDeleteStacks.getActionCommand())) {
      applicationManager.deleteAlignedStacks(axisID);
    }
  }

  private class ButtonListener implements ActionListener {

    TomogramGenerationDialog adaptee;

    ButtonListener(TomogramGenerationDialog adaptee) {
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
    text =
      "Make aligned stack with linear instead of cubic interpolation to "
        + "reduce noise.";
    cbBoxUseLinearInterpolation.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Generate the complete aligned stack for input into the tilt process."
        + "  This runs the newst.com script.";
    btnNewst.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Open the complete aligned stack in 3dmod";
    btn3dmodFull.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Thickness, in pixels, along the z-axis of the reconstructed volume.";
    ltfTomoThickness.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "The first slice in the Y dimension to include in the reconstructed "
        + " volume.  Slices are numbered from 0, a last slice must also "
        + "be specified.";
    ltfSliceStart.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "The last slice in the Y dimension to include in the reconstructed "
        + " volume.  Slices are numbered from 0, a first slice must also "
        + "be specified.";
    ltfSliceStop.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Step between slices in the Y dimension.  A first and last slice must "
        + "also be entered. Default is 1.";
    ltfSliceIncr.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This entry specifies the width of the output image; the default is the "
        + "width of the input image.";
    ltfTomoWidth.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Amount to shift the reconstructed slices in X before output.  A "
        + "positive offset will shift the slice to the right, and the "
        + "output will contain the left part of the whole potentially "
        + "reconstructable area.";
    ltfXOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Amount to shift the reconstructed slices in Z before output.  A "
        + "positive offset will shift the slice upward.";
    ltfZOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This line allows one to rotate the reconstruction around the X axis, so "
        + "that a section that appears to be tilted around the X axis can be "
        + "made flat to fit into a smaller volume.  The ANGLE should be the "
        + "tilt of the section relative to the X-Y plane in an unrotated "
        + "reconstruction.  For example, if the reconstruction extends 500 "
        + "slices, and the section is 5 pixels below the middle in the first "
        + "slice and 5 pixels above the middle in the last slice, ANGLE should"
        + " be 1.1 (the arc sine of 10/500).";
    ltfXAxisTilt.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Offset in degrees to apply to the tilt angles; a positive offset will "
        + "rotate the reconstructed slices counterclockwise.  Do not use "
        + "this option for a tomogram that is part of a dual-axis series.";
    ltfTiltAngleOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "The spatial frequency at which to switch from the R-weighted radial "
        + "filter to a Gaussian falloff.  Frequency is in cycles/pixel and "
        + "ranges from 0-0.5.  Both a cutoff and a falloff must be entered.";
    ltfRadialMax.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "The sigma value of a Gaussian which determines how fast the radial "
        + "filter falls off at spatial frequencies above the cutoff frequency."
        + "  Frequency is in cycles/pixel and ranges from 0-0.5.  Both a "
        + "cutoff and a falloff must be entered ";
    ltfRadialFallOff.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Amount to add to reconstructed density values before multiplying by"
        + " the scale factor and outputting the values.";
    ltfDensityOffset.setToolTipText(tooltipFormatter.setText(text).format());
    text =
      "Amount to multiply reconstructed density values by, after adding the "
        + "offset value.";
    ltfDensityScale.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This parameter allows one to generate a reconstruction using the "
        + "logarithm of the densities in the input file, with the value "
        + "specified added before taking the logarithm.  If no parameter is "
        + "specified the logarithm of the input data is not taken.";
    ltfLogOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Select this checkbox to use local alignments.  You must have "
        + "created the local alignments in the Fine Alignment step";
    cbBoxUseLocalAlignment.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Compute the tomogram from the full aligned stack.  This runs "
        + "the tilt.com script.";
    btnTilt.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the reconstructed volume in 3dmod.";
    btn3dmodTomogram.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Current name of trial tomogram, which will be generated, viewed, or"
        + " used by the buttons below.";
    lblTrialTomogramName.setToolTipText(
      tooltipFormatter.setText(text).format());
    cmboTrialTomogramName.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Compute a trial tomogram with the current parameters, using the "
        + "filename in the \" Trial tomogram filename \" box.";
    btnTrial.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "View the trial tomogram whose name is shown in \"Trial "
        + "tomogram filename\" box.";
    btn3dmodTrial.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Rename the trial tomogram whose name is shown in the \"Trial "
        + "tomogram filename\" box to be the final tomogram.";
    btnUseTrial.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Delete the pre-aligned and aligned stack for this axis.  Once the "
        + "tomogram is calculated these intermediate files are not used and can be "
        + ""
        + "deleted to free up disk space.";
    btnDeleteStacks.setToolTipText(tooltipFormatter.setText(text).format());
  }
}