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
 * Revision 3.8  2004/04/12 17:16:47  sueh
 * bug# 409  Change HighFrequencyRadiusSigma to LowPassRadiusSigma.
 *
 * Revision 3.7  2004/03/30 17:43:16  sueh
 * bug# 409 adding tooltips to new fields
 *
 * Revision 3.6  2004/03/29 20:57:09  sueh
 * bug# 409 added actions for MTF Filter buttons, shortened screen by putting some
 * numeric fields side-by-side, stopped recreating the panel each time Basic/Advanced
 * is pressed to prevent the panels from moving
 *
 * Revision 3.5  2004/03/24 18:17:12  sueh
 * bug# 409 added some MTF filter fields without placing anything on the screen,
 * reformatted
 *
 * Revision 3.4  2004/03/24 03:03:26  rickg
 * Bug# 395 Implemented ability to create binned tomogram
 *
 * Revision 3.3  2004/03/15 20:33:55  rickg
 * button variable name changes to btn...
 *
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

package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Vector;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import etomo.ApplicationManager;
import etomo.comscript.ConstMTFFilterParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.TiltParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.storage.MtfFileFilter;
import etomo.type.AxisID;
import etomo.util.InvalidParameterException;

public class TomogramGenerationDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel pnlTilt = new JPanel();

  private JPanel pnlNewstParams = new JPanel();

  private BeveledBorder border = new BeveledBorder("Tomogram Generation");

  private LabeledSpinner spinBinning;

  private Dimension fullImageSize = new Dimension();

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
    new LabeledTextField("First slice: ");

  private LabeledTextField ltfSliceStop =
    new LabeledTextField("Last slice: ");
    
  private JLabel lblInY = new JLabel(" in Y");
  
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
    new LabeledTextField("Falloff: ");

  private LabeledTextField ltfDensityOffset =
    new LabeledTextField("Offset: ");

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

  //MTF Filter
  private JPanel pnlFilter = new JPanel();
  private EtchedBorder filterBorder = new EtchedBorder("MTF Filtering");
  private LabeledTextField ltfLowPassRadiusSigma =
    new LabeledTextField("Low pass (radius,sigma): ");
  private JPanel pnlInverseFilter = new JPanel();
  private EtchedBorder inverseFilterBorder =
    new EtchedBorder("Inverse Filtering Parameters: ");
  private ImageIcon iconFolder =
    new ImageIcon(ClassLoader.getSystemResource("images/openFile.gif"));
  private JPanel pnlMtfFile = new JPanel();
  private LabeledTextField ltfMtfFile = new LabeledTextField("MTF file: ");
  private JButton btnMtfFile = new JButton(iconFolder);
  private JPanel pnlInverseFilter1 = new JPanel();
  private LabeledTextField ltfMaximumInverse =
    new LabeledTextField("Maximum Inverse: ");
  private LabeledTextField ltfInverseRolloffRadiusSigma =
    new LabeledTextField("Rolloff (radius,sigma): ");
  private JPanel pnlFilterButtons = new JPanel();
  private MultiLineToggleButton btnFilter = new MultiLineToggleButton("Filter");
  private MultiLineButton btnViewFilter =
    new MultiLineButton("View Filtered Stack");
  private MultiLineToggleButton btnUseFilter =
    new MultiLineToggleButton("Use Filtered Stack");

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

  private JPanel pnlDensityScaling = new JPanel();
  private JPanel pnlSlicesInY = new JPanel();
  private JPanel pnlOffset = new JPanel();
  private JPanel pnlRadialFilter = new JPanel();
  private JPanel pnlUseLocalAlignment = new JPanel();
  private JPanel pnlAdvanced1 = new JPanel();
  private JPanel pnlAdvanced2 = new JPanel();
  private JPanel pnlAdvanced3 = new JPanel();
  private JPanel pnlAdvanced4 = new JPanel();

  public TomogramGenerationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");

    //  Setup the binning spinner for newstack
    SpinnerModel integerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinBinning =
      new LabeledSpinner("Aligned image stack binning ", integerModel);

    // Object alignments
    cbBoxUseLinearInterpolation.setAlignmentX(Component.CENTER_ALIGNMENT);
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
    btnFilter.setPreferredSize(dimButton);
    btnFilter.setMaximumSize(dimButton);
    btnViewFilter.setPreferredSize(dimButton);
    btnViewFilter.setMaximumSize(dimButton);
    btnUseFilter.setPreferredSize(dimButton);
    btnUseFilter.setMaximumSize(dimButton);
    spinBinning.setTextMaxmimumSize(UIParameters.getSpinnerDimension());
    btnMtfFile.setPreferredSize(FixedDim.folderButton);
    btnMtfFile.setMaximumSize(FixedDim.folderButton);

    // Bind the buttons to the action listener
    ButtonListener tomogramGenerationListener = new ButtonListener(this);
    btnNewst.addActionListener(tomogramGenerationListener);
    btn3dmodFull.addActionListener(tomogramGenerationListener);
    btnFilter.addActionListener(tomogramGenerationListener);
    btnViewFilter.addActionListener(tomogramGenerationListener);
    btnUseFilter.addActionListener(tomogramGenerationListener);
    btnTrial.addActionListener(tomogramGenerationListener);
    btn3dmodTrial.addActionListener(tomogramGenerationListener);
    btnUseTrial.addActionListener(tomogramGenerationListener);
    btnTilt.addActionListener(tomogramGenerationListener);
    btn3dmodTomogram.addActionListener(tomogramGenerationListener);
    btnDeleteStacks.addActionListener(tomogramGenerationListener);
    btnMtfFile.addActionListener(new MtfFileActionListener(this));

    // Layout the newst button panel
    pnlAlignedStack.setLayout(new BoxLayout(pnlAlignedStack, BoxLayout.X_AXIS));
    pnlAlignedStack.add(btnNewst);
    pnlAlignedStack.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlAlignedStack.add(btn3dmodFull);
    layoutFilterPanel();

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
    pnlTrialButtons.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlTrialButtons.add(btn3dmodTrial);
    pnlTrialButtons.add(Box.createRigidArea(FixedDim.x5_y0));
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
    pnlDensityScaling.setLayout(new BoxLayout(pnlDensityScaling, BoxLayout.X_AXIS));
    pnlSlicesInY.setLayout(new BoxLayout(pnlSlicesInY, BoxLayout.X_AXIS));
    pnlOffset.setLayout(new BoxLayout(pnlOffset, BoxLayout.X_AXIS));
    pnlRadialFilter.setLayout(new BoxLayout(pnlRadialFilter, BoxLayout.X_AXIS));
    pnlAdvanced1.setLayout(new BoxLayout(pnlAdvanced1, BoxLayout.Y_AXIS));
    pnlAdvanced2.setLayout(new BoxLayout(pnlAdvanced2, BoxLayout.Y_AXIS));
    pnlAdvanced3.setLayout(new BoxLayout(pnlAdvanced3, BoxLayout.Y_AXIS));
    pnlAdvanced4.setLayout(new BoxLayout(pnlAdvanced4, BoxLayout.Y_AXIS));
    pnlTiltParams.setBorder(new EtchedBorder("Tilt Parameters").getBorder());
    pnlTilt.add(pnlNewstParams);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTilt.add(pnlAlignedStack);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTilt.add(pnlFilter);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y5));
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
    cbBoxUseLinearInterpolation.setSelected(newstParam.isLinearInterpolation());
    int binning = newstParam.getBinByFactor();
    if (binning > 1) {
      spinBinning.setValue(binning);
    }
  }

  /**
   * Set the UI parameters with the specified tiltParam values
   * WARNING: be sure the setNewstParam is called first so the binning value for
   * the stack is known.  The thickness, first and last slice, width and x,y,z
   * offsets are scaled so that they are represented to the user in unbinned
   * dimensions.
   * @param tiltParam
   */
  public void setTiltParams(ConstTiltParam tiltParam) {
    int binning = ((Integer) spinBinning.getValue()).intValue();
    if (tiltParam.hasWidth()) {
      ltfTomoWidth.setText(tiltParam.getWidth() * binning);
    }
    if (tiltParam.hasThickness()) {
      ltfTomoThickness.setText(tiltParam.getThickness() * binning);
    }
    if (tiltParam.hasXOffset()) {
      ltfXOffset.setText(tiltParam.getXOffset() * binning);
    }
    if (tiltParam.hasZOffset()) {
      ltfZOffset.setText(tiltParam.getZOffset() * binning);
    }
    if (tiltParam.hasSlice()) {
      ltfSliceStart.setText(tiltParam.getIdxSliceStart() * binning);
      ltfSliceStop.setText(tiltParam.getIdxSliceStop() * binning);
    }
    if (tiltParam.hasSliceIncr()) {
      ltfSliceIncr.setText(tiltParam.getIncrSlice());
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
    cbBoxUseLocalAlignment.setSelected(tiltParam.hasLocalAlignFile());
  }

  //  Copy the newstack parameters from the GUI to the NewstParam object
  public void getNewstParams(NewstParam newstParam) {
    newstParam.setLinearInterpolation(cbBoxUseLinearInterpolation.isSelected());
    int binning = ((Integer) spinBinning.getValue()).intValue();

    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    if (binning > 1) {
      newstParam.setBinByFactor(binning);
    }
    else {
      newstParam.setBinByFactor(Integer.MIN_VALUE);
    }
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  public void getTiltParams(TiltParam tiltParam)
    throws NumberFormatException, InvalidParameterException {
    int binning = ((Integer) spinBinning.getValue()).intValue();
    String badParameter = "";

    try {
      // Set the appropriate FULLIMAGE line
      badParameter = "FULLIMAGE";
      tiltParam.setFullImageX(fullImageSize.width / binning);
      tiltParam.setFullImageY(fullImageSize.height / binning);

      if (ltfTomoWidth.getText().matches("\\S+")) {
        badParameter = ltfTomoWidth.getLabel();
        tiltParam.setWidth(Integer.parseInt(ltfTomoWidth.getText()) / binning);
      }
      else {
        tiltParam.resetWidth();
      }

      if (ltfXOffset.getText().matches("\\S+")) {
        badParameter = ltfXOffset.getLabel();
        tiltParam.setXOffset(Float.parseFloat(ltfXOffset.getText()) / binning);
        if (ltfZOffset.getText().matches("\\S+")) {
          badParameter = ltfZOffset.getLabel();
          tiltParam.setZOffset(
            Float.parseFloat(ltfZOffset.getText()) / binning);
        }
        else {
          tiltParam.resetZOffset();
        }
      }
      else {
        tiltParam.resetXOffset();
        if (ltfZOffset.getText().matches("\\S+")) {
          throw (
            new InvalidParameterException("You must supply an X offset to supply a Z offset"));
        }
        else {
          tiltParam.resetZOffset();
        }
      }

      boolean sliceRangeSpecified = false;
      if (ltfSliceStart.getText().matches("\\S+")
        && ltfSliceStop.getText().matches("\\S+")) {
        badParameter = ltfSliceStart.getLabel();
        tiltParam.setIdxSliceStart(
          Integer.parseInt(ltfSliceStart.getText()) / binning);
        badParameter = ltfSliceStop.getLabel();
        tiltParam.setIdxSliceStop(
          Integer.parseInt(ltfSliceStop.getText()) / binning);
        sliceRangeSpecified = true;
      }
      else if (
        ltfSliceStart.getText().matches("^\\s*$")
          && ltfSliceStop.getText().matches("^\\s*$")) {
        tiltParam.resetIdxSlice();
      }
      else {
        throw (
          new InvalidParameterException("You must supply both the first and last slices if you want to specify either."));
      }
      if (ltfSliceIncr.getText().matches("\\S+")) {
        if (sliceRangeSpecified) {
          badParameter = ltfSliceIncr.getLabel();
          tiltParam.setIncrSlice(Integer.parseInt(ltfSliceIncr.getText()));
        }
        else {
          throw (
            new InvalidParameterException("You must supply both the first and last slices to specify the slice step."));
        }
      }
      else {
        tiltParam.resetIncrSlice();
      }

      if (ltfTomoThickness.getText().matches("\\S+")) {
        badParameter = ltfTomoThickness.getLabel();
        tiltParam.setThickness(
          Integer.parseInt(ltfTomoThickness.getText()) / binning);
      }
      else {
        tiltParam.resetThickness();
      }

      if (ltfXAxisTilt.getText().matches("\\S+")) {
        badParameter = ltfXAxisTilt.getLabel();
        tiltParam.setXAxisTilt(Float.parseFloat(ltfXAxisTilt.getText()));
      }
      else {
        tiltParam.resetXAxisTilt();
      }

      if (ltfTiltAngleOffset.getText().matches("\\S+")) {
        badParameter = ltfTiltAngleOffset.getLabel();
        tiltParam.setTiltAngleOffset(
          Float.parseFloat(ltfTiltAngleOffset.getText()));
      }
      else {
        tiltParam.resetTiltAngleOffset();
      }

      if (ltfRadialMax.getText().matches("\\S+")
        || ltfRadialFallOff.getText().matches("\\S+")) {
        badParameter = ltfRadialMax.getLabel();
        tiltParam.setRadialBandwidth(Float.parseFloat(ltfRadialMax.getText()));
        badParameter = ltfRadialFallOff.getLabel();
        tiltParam.setRadialFalloff(
          Float.parseFloat(ltfRadialFallOff.getText()));
      }
      else {
        tiltParam.resetRadialFilter();
      }

      if (ltfDensityOffset.getText().matches("\\S+")
        || ltfDensityScale.getText().matches("\\S+")) {
        badParameter = ltfDensityScale.getLabel();
        tiltParam.setScaleCoeff(Float.parseFloat(ltfDensityScale.getText()));
        badParameter = ltfDensityOffset.getLabel();
        tiltParam.setScaleFLevel(Float.parseFloat(ltfDensityOffset.getText()));
      }
      else {
        tiltParam.resetScale();
      }

      if (ltfLogOffset.getText().matches("\\S+")) {
        badParameter = ltfLogOffset.getLabel();
        tiltParam.setLogShift(Float.parseFloat(ltfLogOffset.getText()));
      }
      else {
        tiltParam.setLogShift(Float.NaN);
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

  public void getMTFFilterParam(MTFFilterParam mtfFilterParam)
    throws FortranInputSyntaxException {
    mtfFilterParam.setLowPassRadiusSigma(ltfLowPassRadiusSigma.getText());
    mtfFilterParam.setMtfFile(ltfMtfFile.getText());
    mtfFilterParam.setMaximumInverse(ltfMaximumInverse.getText());
    mtfFilterParam.setInverseRolloffRadiusSigma(
      ltfInverseRolloffRadiusSigma.getText());
  }

  public void setMTFFilterParam(ConstMTFFilterParam mtfFilterParam) {
    ltfMtfFile.setText(mtfFilterParam.getMtfFile());
    ltfMaximumInverse.setText(mtfFilterParam.getMaximumInverseString());
    ltfLowPassRadiusSigma.setText(
      mtfFilterParam.getLowPassRadiusSigmaString());
    ltfInverseRolloffRadiusSigma.setText(
      mtfFilterParam.getInverseRolloffRadiusSigmaString());
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
    updateAdvancedTiltPanel();
    pnlNewstParams.setVisible(isAdvanced);
    pnlFilter.setVisible(isAdvanced);
    applicationManager.packMainWindow();
  }

  /**
   * Layout the newstack panel
   */
  private void layoutNewstPanel() {
    pnlNewstParams.add(cbBoxUseLinearInterpolation);
    pnlNewstParams.add(spinBinning.getContainer());
    pnlNewstParams.setBorder(new EtchedBorder("Newstack Parameters").getBorder());
  }
  
  private void updateAdvancedTiltPanel() {
    pnlAdvanced1.setVisible(isAdvanced);
    //ltfTomoThickness - both
    pnlAdvanced2.setVisible(isAdvanced);
    //ltfXAxisTilt - both
    pnlAdvanced3.setVisible(isAdvanced);
    //pnlRadialFilter - both
    //cbBoxUseLocalAlignment - both
    pnlTrial.setVisible(isAdvanced);
  }

  private void layoutTiltPanel() {
    pnlAdvanced1.add(ltfLogOffset.getContainer());
    pnlAdvanced1.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlDensityScaling.add(ltfDensityScale.getContainer());
    pnlDensityScaling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlDensityScaling.add(ltfDensityOffset.getContainer());
    pnlAdvanced1.add(pnlDensityScaling);
    pnlAdvanced1.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTiltParams.add(pnlAdvanced1);
    pnlTiltParams.add(ltfTomoThickness.getContainer());
    pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced1.add(ltfTomoWidth.getContainer());
    pnlAdvanced1.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSlicesInY.add(ltfSliceStart.getContainer());
    pnlSlicesInY.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSlicesInY.add(ltfSliceStop.getContainer());
    pnlSlicesInY.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSlicesInY.add(lblInY);
    pnlAdvanced2.add(pnlSlicesInY);
    pnlAdvanced2.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced2.add(ltfSliceIncr.getContainer());
    pnlAdvanced2.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlOffset.add(ltfXOffset.getContainer());
    pnlOffset.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlOffset.add(ltfZOffset.getContainer());
    pnlAdvanced2.add(pnlOffset);
    pnlAdvanced2.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTiltParams.add(pnlAdvanced2);
    pnlTiltParams.add(ltfXAxisTilt.getContainer());
    pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAdvanced3.add(ltfTiltAngleOffset.getContainer());
    pnlAdvanced3.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTiltParams.add(pnlAdvanced3);
    pnlRadialFilter.add(ltfRadialMax.getContainer());
    pnlRadialFilter.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlRadialFilter.add(ltfRadialFallOff.getContainer());
    pnlTiltParams.add(pnlRadialFilter);
    pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlUseLocalAlignment.add(cbBoxUseLocalAlignment);
    pnlTiltParams.add(pnlUseLocalAlignment);
    pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTiltParams.add(pnlTrial);
  }

  protected void layoutFilterPanel() {
    pnlFilter.setBorder(filterBorder.getBorder());
    pnlFilter.setLayout(new BoxLayout(pnlFilter, BoxLayout.Y_AXIS));
    pnlFilter.add(ltfLowPassRadiusSigma.getContainer());
    pnlFilter.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlInverseFilter.setLayout(
      new BoxLayout(pnlInverseFilter, BoxLayout.Y_AXIS));
    pnlInverseFilter.setBorder(inverseFilterBorder.getBorder());
    pnlMtfFile.setLayout(new BoxLayout(pnlMtfFile, BoxLayout.X_AXIS));
    pnlMtfFile.add(ltfMtfFile.getContainer());
    pnlMtfFile.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlMtfFile.add(btnMtfFile);
    pnlInverseFilter.add(pnlMtfFile);
    pnlInverseFilter.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlInverseFilter1.setLayout(
      new BoxLayout(pnlInverseFilter1, BoxLayout.X_AXIS));
    pnlInverseFilter1.add(ltfMaximumInverse.getContainer());
    pnlInverseFilter1.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlInverseFilter1.add(ltfInverseRolloffRadiusSigma.getContainer());
    pnlInverseFilter.add(pnlInverseFilter1);
    pnlFilter.add(pnlInverseFilter);
    pnlFilter.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlFilterButtons.setLayout(
      new BoxLayout(pnlFilterButtons, BoxLayout.X_AXIS));
    pnlFilterButtons.add(btnFilter);
    pnlFilterButtons.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlFilterButtons.add(btnViewFilter);
    pnlFilterButtons.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlFilterButtons.add(btnUseFilter);
    pnlFilter.add(pnlFilterButtons);
    pnlFilter.add(Box.createRigidArea(FixedDim.x0_y5));
  }

  private void btnMtfFileAction(ActionEvent event) {
    //Open up the file chooser in the $IMOD_CALIB_DIR/Camera, if available,
    //otherwise open in the working directory
    String currentMtfDirectory = ltfMtfFile.getText();
    if (currentMtfDirectory.equals("")) {
      File calibrationDir = ApplicationManager.getIMODCalibDirectory();
      File cameraDir = new File(calibrationDir.getAbsolutePath(),
        "Camera");
      if (cameraDir.exists()) {
        currentMtfDirectory = cameraDir.getAbsolutePath();
      }
      else {
        currentMtfDirectory = System.getProperty("user.dir");
      }
    }
    JFileChooser chooser = new JFileChooser(
      new File(currentMtfDirectory));
    MtfFileFilter mtfFileFilter = new MtfFileFilter();
    chooser.setFileFilter(mtfFileFilter);
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File mtfFile = chooser.getSelectedFile();
      try {
        ltfMtfFile.setText(mtfFile.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
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
    else if (command.equals(btnFilter.getActionCommand())) {
      applicationManager.mtffilter(axisID);
      btnFilter.setSelected(true);
      btnUseFilter.setSelected(false);
    }
    else if (command.equals(btnViewFilter.getActionCommand())) {
      applicationManager.imodMTFFilter(axisID);
    }
    else if (command.equals(btnUseFilter.getActionCommand())) {
      applicationManager.useMtfFilter(axisID);
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
  
  class MtfFileActionListener implements ActionListener {
    TomogramGenerationDialog adaptee;

    MtfFileActionListener(TomogramGenerationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnMtfFileAction(event);
    }
  }


  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    Autodoc autodoc = null;

    try {
      autodoc = Autodoc.get(Autodoc.MTF_FILTER);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }

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
    if (autodoc != null) {
      text = TooltipFormatter.getText(autodoc, "LowPassRadiusSigma");
      if (text != null) {
        ltfLowPassRadiusSigma.setToolTipText(
          tooltipFormatter.setText(text).format());
      }
      text = TooltipFormatter.getText(autodoc, "MtfFile");
      if (text != null) {
        ltfMtfFile.setToolTipText(tooltipFormatter.setText(text).format());
      }
      text = TooltipFormatter.getText(autodoc, "MaximumInverse");
      if (text != null) {
        ltfMaximumInverse.setToolTipText(
          tooltipFormatter.setText(text).format());
      }
      text = TooltipFormatter.getText(autodoc, "InverseRolloffRadiusSigma");
      if (text != null) {
        ltfInverseRolloffRadiusSigma.setToolTipText(
          tooltipFormatter.setText(text).format());
      }
    }
    text = "Run mtffilter on the full aligned stack.";
    btnFilter.setToolTipText(tooltipFormatter.setText(text).format());
    text = "View the results of running mtffilter on the full aligned stack.";
    btnViewFilter.setToolTipText(tooltipFormatter.setText(text).format());
    text =
      "Use the results of running mtffilter as the new full aligned stack.";
    btnUseFilter.setToolTipText(tooltipFormatter.setText(text).format());
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

  /**
   * @return Returns the fullImageSize.
   */
  public Dimension getFullImageSize() {
    return new Dimension(fullImageSize);
  }

  /**
   * Set the full image size
   * @param width
   * @param height
   */
  public void setFullImageSize(int width, int height) {
    fullImageSize.setSize(width, height);
  }
}