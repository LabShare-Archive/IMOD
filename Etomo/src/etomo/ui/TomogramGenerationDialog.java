
package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
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
import etomo.EtomoDirector;
import etomo.comscript.ConstMTFFilterParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.TiltParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.storage.MtfFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.MetaData;
import etomo.util.InvalidParameterException;

/**
 * <p>
 * Description: Tomogram generation user interface
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
 * Revision 3.28  2005/01/11 18:08:21  sueh
 * bug# 578 Sending useZFactors state to metaData to preserve it when it is
 * disabled.
 *
 * Revision 3.27  2005/01/08 01:56:11  sueh
 * bug# 578 Added z factors checkbox and a public function to
 * enable/disable it.
 *
 * Revision 3.26  2004/12/02 20:42:40  sueh
 * bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * the join guide.  Need to specify the guide to anchor.
 *
 * Revision 3.25  2004/11/20 00:06:42  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.24.2.4  2004/10/11 02:19:11  sueh
 * bug# 520 Passed the manager to the ContextPopup object in order to get
 * the propertyUserDir.
 *
 * Revision 3.24.2.3  2004/10/08 16:41:41  sueh
 * bug# 520 Since EtomoDirector is a singleton, made all functions and
 * member variables non-static.
 *
 * Revision 3.24.2.2  2004/09/15 22:48:16  sueh
 * bug# 520 call openMessageDialog in mainPanel instead of mainFrame.
 *
 * Revision 3.24.2.1  2004/09/07 18:02:16  sueh
 * bug# 520 getting dataset name from metadata
 *
 * Revision 3.24  2004/07/20 23:28:34  sueh
 * bug# 514
 *
 * Revision 3.23  2004/07/20 23:08:33  sueh
 * bug# 502 setting fiducialess in tilt (not getting fiducialess
 * from tilt).  Use local alignment is disabled when fiducialess is
 * true
 *
 * Revision 3.22  2004/07/15 20:34:13  sueh
 * bug# 500 moving linear interpolation to Basic mode
 *
 * Revision 3.21  2004/07/15 20:17:35  sueh
 * bug# 499 added "optional" to 2d filtering title
 *
 * Revision 3.20  2004/07/02 20:42:07  sueh
 * bug# 489 added a tilt button panel to put the buttons at the bottom
 * side by side, changed the updateAdvanced() function
 *
 * Revision 3.19  2004/06/17 22:30:15  sueh
 * bug# 475 set X offset to 0 if X offset is empty and Z offset is set
 *
 * Revision 3.18  2004/06/17 21:25:47  sueh
 * bug# 473
 *
 * Revision 3.17  2004/06/17 20:18:22  sueh
 * bug# 472
 *
 * Revision 3.16  2004/06/17 18:49:55  sueh
 * bug# 472
 *
 * Revision 3.15  2004/06/01 19:02:33  rickg
 * Bug #391 moved fiducialess parameters in with newstack
 * parameters, fixed trash javadoc header
 *
 * Revision 3.14  2004/05/26 04:54:08  rickg
 * Bug #391 added fiducialess parameter interface and UI objects
 *
 * Revision 3.13  2004/04/28 16:15:18  sueh
 * bug# 409 changing border name (mast)
 *
 * Revision 3.12  2004/04/24 21:28:42  rickg
 * bug #424 Fixed and organized UI layouts
 *
 * Revision 3.11  2004/04/16 02:20:19  sueh
 * removing print statements
 *
 * Revision 3.10  2004/04/16 02:13:14  sueh
 * bug# 409 Added startingAndEndingZ
 * Added code to enable and disable filter buttons, based on a call from
 * ApplicationManager (enableFilter) and the user placing a value in
 * startingAndEndingZ
 *
 * Revision 3.9  2004/04/13 17:23:36  sueh
 * bug# 409 add file choose for mtf filter.  Automatically goes to
 * $IMOD_CALIB_DIR/Camera, if it exists.  File filter:  .mtf
 *
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
 * Revision 2.14 2003/10/14 23:45:01 rickg
 * Bug# 285 Added view aligned stack button
 *
 * Revision 2.13 2003/10/14 22:53:55 rickg
 * Bug #286 Label changes
 * 
 * Revision 2.12 2003/10/02 18:57:47 sueh
 * bug236 added testing:
 * NewstParamTest
 * ComScriptTest
 * Removed marks
 *
 * Revision 2.11 2003/09/29 23:34:57 sueh
 * bug236 Added UseLinearInterpolation to
 * TomogramGenerationDialog.
 * UseLinearInterpolation:
 * check box
 * Advanced
 * newst -linear
 *
 * Files:
 * ComScriptManager.java
 * ConstNewstParam.java
 * NewstParam.java
 * TomogramGenerationDialog.java
 * ApplicationManager.java
 *
 * Revision 2.10 2003/09/08 22:51:25 rickg
 * Added commit test volume action
 *
 * Revision 2.9 2003/09/08 05:47:09 rickg
 * Added trial tilt
 * Output for a single axis tomogram is changed to
 * dataset_full.rec
 *
 * Revision 2.8 2003/06/25 22:14:57 rickg
 * Constructed a panel for the tilt parameters
 *
 * Revision 2.7 2003/06/23 23:26:59 rickg
 * Added advanced options/parameters
 *
 * Revision 2.6 2003/05/23 22:14:11 rickg
 * Removed any extensions from log file labels in context menu
 *
 * Revision 2.5 2003/05/23 21:26:55 rickg
 * *** empty log message ***
 *
 * Revision 2.4 2003/04/28 23:25:25 rickg
 * Changed visible imod references to 3dmod
 *
 * Revision 2.3 2003/04/24 17:46:54 rickg
 * Changed fileset name to dataset name
 *
 * Revision 2.2 2003/03/02 23:30:41 rickg
 * Combine layout in progress
 *
 * Revision 2.1 2003/01/24 21:04:18 rickg
 * AxisID bug fix from single buttonAction function
 *
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * Single window merge to main branch
 *
 * Revision 1.6.2.1 2003/01/24 18:43:37 rickg
 * Single window GUI layout initial revision
 *
 * Revision 1.6 2002/12/19 17:45:22 rickg
 * Implemented advanced dialog state processing
 * including:
 * default advanced state set on start up
 * advanced button management now handled by
 * super class
 *
 * Revision 1.5 2002/12/19 00:30:26 rickg
 * app manager and root pane moved to super class
 *
 * Revision 1.4 2002/11/14 21:18:37 rickg
 * Added anchors into the tomoguide
 *
 * Revision 1.3 2002/10/17 22:40:22 rickg
 * Added fileset name to window title
 * this reference removed applicationManager messages
 *
 * Revision 1.2 2002/10/07 22:31:18 rickg
 * removed unused imports
 * reformat after emacs trashed it
 *
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * Initial CVS entry, basic functionality not including combining
 *
 * </p>
 */

public class TomogramGenerationDialog extends ProcessDialog
    implements
      ContextMenu,
      FiducialessParams {
  public static final String rcsid = "$Id$";

  private JPanel pnlTilt = new JPanel();

  // Fiducialess parameters
  private JCheckBox cbFiducialess = new JCheckBox("Fiducialless alignment");
  private LabeledTextField ltfRotation = new LabeledTextField(
      "Tilt axis rotation:");

  // Newst/Newstack objects
  private JPanel pnlNewstParams = new JPanel();
  private JCheckBox cbBoxUseLinearInterpolation = new JCheckBox(
      "Use linear interpolation");
  private LabeledSpinner spinBinning;
  private Dimension fullImageSize = new Dimension();

  //  Aligned stack buttons
  private JPanel pnlAlignedStack = new JPanel();
  private MultiLineToggleButton btnNewst = new MultiLineToggleButton(
      "<html><b>Create Full<br>Aligned Stack</b>");
  private MultiLineButton btn3dmodFull = new MultiLineButton(
      "<html><b>View Full<br>Aligned Stack</b>");

  //  Tilt objects
  private JPanel pnlTiltParams = new JPanel();
  private LabeledTextField ltfXOffset = new LabeledTextField("X offset: ");
  private LabeledTextField ltfZOffset = new LabeledTextField("Z offset: ");
  private LabeledTextField ltfSliceStart = new LabeledTextField("First slice: ");
  private LabeledTextField ltfSliceStop = new LabeledTextField("Last slice: ");
  private JLabel lblInY = new JLabel(" in Y");
  private LabeledTextField ltfSliceIncr = new LabeledTextField(
      "Slice step in Y: ");
  private LabeledTextField ltfTomoWidth = new LabeledTextField(
      "Tomogram width in X: ");
  private LabeledTextField ltfTomoThickness = new LabeledTextField(
      "Tomogram thickness in Z: ");
  private LabeledTextField ltfXAxisTilt = new LabeledTextField("X axis tilt: ");
  private LabeledTextField ltfTiltAngleOffset = new LabeledTextField(
      "Tilt angle offset: ");
  private LabeledTextField ltfRadialMax = new LabeledTextField(
      "Radial filter cutoff: ");
  private LabeledTextField ltfRadialFallOff = new LabeledTextField("Falloff: ");
  private LabeledTextField ltfDensityOffset = new LabeledTextField("Offset: ");
  private LabeledTextField ltfDensityScale = new LabeledTextField(
      "Output density scaling factor: ");
  private LabeledTextField ltfLogOffset = new LabeledTextField("Log offset: ");
  private JCheckBox cbBoxUseLocalAlignment = new JCheckBox(
      "Use local alignments");
  private JPanel pnlDensityScaling = new JPanel();
  private JPanel pnlSlicesInY = new JPanel();
  private JPanel pnlOffset = new JPanel();
  private JPanel pnlRadialFilter = new JPanel();
  private JPanel pnlUseLocalAlignment = new JPanel();
  private JPanel pnlAdvanced1 = new JPanel();
  private JPanel pnlAdvanced2 = new JPanel();
  private JPanel pnlAdvanced3 = new JPanel();

  //  Trial tomogram objects
  private JPanel pnlTrial = new JPanel();
  private JPanel pnlTrialTomogramName = new JPanel();
  private JLabel lblTrialTomogramName = new JLabel("Trial tomogram filename: ");
  private JComboBox cmboTrialTomogramName = new JComboBox();
  private Vector trialTomogramList = new Vector();
  private JPanel pnlTrialButtons = new JPanel();
  private MultiLineButton btnTrial = new MultiLineButton(
      "<html><b>Generate Trial Tomogram</b>");
  private MultiLineButton btn3dmodTrial = new MultiLineButton(
      "<html><b>View Trial in 3dmod</b>");
  private MultiLineToggleButton btnUseTrial = new MultiLineToggleButton(
      "<html><b>Use Current Trial Tomogram</b>");

  // MTF Filter objects
  private JPanel pnlFilter = new JPanel();
  private LabeledTextField ltfLowPassRadiusSigma = new LabeledTextField(
      "Low pass (cutoff,sigma): ");
  private JPanel pnlInverseFilter = new JPanel();
  private ImageIcon iconFolder = new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif"));
  private JPanel pnlMtfFile = new JPanel();
  private LabeledTextField ltfMtfFile = new LabeledTextField("MTF file: ");
  private JButton btnMtfFile = new JButton(iconFolder);
  private JPanel pnlInverseFilterParam = new JPanel();
  private LabeledTextField ltfMaximumInverse = new LabeledTextField(
      "Maximum Inverse: ");
  private LabeledTextField ltfInverseRolloffRadiusSigma = new LabeledTextField(
      "Rolloff (radius,sigma): ");
  private JPanel pnlFilterButtons = new JPanel();
  private MultiLineToggleButton btnFilter = new MultiLineToggleButton("Filter");
  private MultiLineButton btnViewFilter = new MultiLineButton(
      "View Filtered Stack");
  private MultiLineToggleButton btnUseFilter = new MultiLineToggleButton(
      "Use Filtered Stack");
  private LabeledTextField ltfStartingAndEndingZ = new LabeledTextField(
      "Starting and ending views: ");
  boolean enableFiltering = false;

  //  Tomogram generation buttons
  private MultiLineToggleButton btnTilt = new MultiLineToggleButton(
      "<html><b>Generate Tomogram</b>");
  private MultiLineButton btn3dmodTomogram = new MultiLineButton(
      "<html><b>View Tomogram In 3dmod</b>");
  private MultiLineToggleButton btnDeleteStacks = new MultiLineToggleButton(
      "<html><b>Delete Aligned Image Stack</b>");
  private JCheckBox cbUseZFactors = new JCheckBox("Use Z Factors");
      
  private JPanel pnlTiltButtons = new JPanel();

  public TomogramGenerationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");

    // Layout each of the subpanels
    layoutNewstPanel();
    layoutFilterPanel();
    layoutTiltPanel();
    layoutTrialPanel();

    //tilt buttons panel
    pnlTiltButtons.setLayout(new BoxLayout(pnlTiltButtons, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlTiltButtons, btnTilt);
    UIUtilities.addWithXSpace(pnlTiltButtons, btn3dmodTomogram);
    UIUtilities.addWithXSpace(pnlTiltButtons, btnDeleteStacks);
    UIUtilities.setButtonSizeAll(pnlTiltButtons, UIParameters.dimButton);
    
    // Layout the main panel and add it to the root panel
    pnlTilt.setBorder(new BeveledBorder("Tomogram Generation").getBorder());
    pnlTilt.setLayout(new BoxLayout(pnlTilt, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlTilt, pnlNewstParams);
    UIUtilities.addWithYSpace(pnlTilt, pnlAlignedStack);
    UIUtilities.addWithYSpace(pnlTilt, pnlFilter);
    UIUtilities.addWithYSpace(pnlTilt, pnlTiltParams);
    UIUtilities.addWithYSpace(pnlTilt, pnlTiltButtons);
    UIUtilities.alignComponentsX(pnlTilt, Component.CENTER_ALIGNMENT);

    rootPanel.add(pnlTilt);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

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
    ltfStartingAndEndingZ
        .addKeyListener(new StartingAndEndingZKeyListener(this));
    cbFiducialess.addActionListener(tomogramGenerationListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    updateFiducialess();
    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }

  public void updateFilter(boolean enable) {
    enableFiltering = enable;
    btnFilter.setEnabled(enableFiltering);
    btnViewFilter.setEnabled(enableFiltering);
    enableUseFilter();
  }

  public void setFiducialessAlignment(boolean state) {
    cbFiducialess.setSelected(state);
    updateFiducialess();
  }

  public boolean isFiducialessAlignment() {
    return cbFiducialess.isSelected();
  }

  public void setTiltAxisAngle(float tiltAxisAngle) {
    ltfRotation.setText(tiltAxisAngle);
  }

  public float getTiltAxisAngle() throws NumberFormatException {
    return Float.parseFloat(ltfRotation.getText());
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
    ConstMetaData metaData = applicationManager.getMetaData();
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
    cbBoxUseLocalAlignment.setSelected(metaData.getUseLocalAlignments());
    cbUseZFactors.setSelected(metaData.getUseZFactors().is());
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
  public void getTiltParams(TiltParam tiltParam) throws NumberFormatException,
      InvalidParameterException {
    int binning = ((Integer) spinBinning.getValue()).intValue();
    String badParameter = "";
    MetaData metaData = applicationManager.getTomogramMetaData();
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

      //set Z offset
      if (ltfZOffset.getText().matches("\\S+")) {
        badParameter = ltfZOffset.getLabel();
        tiltParam.setZOffset(Float.parseFloat(ltfZOffset.getText()) / binning);
      }
      else {
        tiltParam.resetZOffset();
      }
      
      //set X offset
      if (ltfXOffset.getText().matches("\\S+")) {
        badParameter = ltfXOffset.getLabel();
        tiltParam.setXOffset(Float.parseFloat(ltfXOffset.getText()) / binning);
      } 
      else if (ltfZOffset.getText().matches("\\S+")) {
        tiltParam.setXOffset(0);
        ltfXOffset.setText(0.0);
      }
      else {
        tiltParam.resetXOffset();
      }

      boolean sliceRangeSpecified = false;
      if (ltfSliceStart.getText().matches("\\S+")
          && ltfSliceStop.getText().matches("\\S+")) {
        badParameter = ltfSliceStart.getLabel();
        tiltParam.setIdxSliceStart(Integer.parseInt(ltfSliceStart.getText())
            / binning);
        badParameter = ltfSliceStop.getLabel();
        tiltParam.setIdxSliceStop(Integer.parseInt(ltfSliceStop.getText())
            / binning);
        sliceRangeSpecified = true;
      }
      else if (ltfSliceStart.getText().matches("^\\s*$")
          && ltfSliceStop.getText().matches("^\\s*$")) {
        tiltParam.resetIdxSlice();
      }
      else {
        throw (new InvalidParameterException(
            "You must supply both the first and last slices if you want to specify either."));
      }
      if (ltfSliceIncr.getText().matches("\\S+")) {
        if (sliceRangeSpecified) {
          badParameter = ltfSliceIncr.getLabel();
          tiltParam.setIncrSlice(Integer.parseInt(ltfSliceIncr.getText()));
        }
        else {
          throw (new InvalidParameterException(
              "You must supply both the first and last slices to specify the slice step."));
        }
      }
      else {
        tiltParam.resetIncrSlice();
      }

      if (ltfTomoThickness.getText().matches("\\S+")) {
        badParameter = ltfTomoThickness.getLabel();
        tiltParam.setThickness(Integer.parseInt(ltfTomoThickness.getText())
            / binning);
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
        tiltParam.setTiltAngleOffset(Float.parseFloat(ltfTiltAngleOffset
            .getText()));
      }
      else {
        tiltParam.resetTiltAngleOffset();
      }

      if (ltfRadialMax.getText().matches("\\S+")
          || ltfRadialFallOff.getText().matches("\\S+")) {
        badParameter = ltfRadialMax.getLabel();
        tiltParam.setRadialBandwidth(Float.parseFloat(ltfRadialMax.getText()));
        badParameter = ltfRadialFallOff.getLabel();
        tiltParam
            .setRadialFalloff(Float.parseFloat(ltfRadialFallOff.getText()));
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
        tiltParam.setLocalAlignFile(applicationManager.getMetaData()
            .getDatasetName()
            + axisID.getExtension() + "local.xf");
      }
      else {
        tiltParam.setLocalAlignFile("");
      }
      metaData.setUseLocalAlignments(cbBoxUseLocalAlignment.isSelected());
      tiltParam.setFiducialess(cbFiducialess.isSelected());
      tiltParam.setUseZFactors(cbUseZFactors.isSelected() && cbUseZFactors.isEnabled());
      metaData.setUseZFactors(cbUseZFactors.isSelected());
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  public void getMTFFilterParam(MTFFilterParam mtfFilterParam)
      throws FortranInputSyntaxException {
    mtfFilterParam.setLowPassRadiusSigma(ltfLowPassRadiusSigma.getText());
    mtfFilterParam.setStartingAndEndingZ(ltfStartingAndEndingZ.getText());
    mtfFilterParam.setMtfFile(ltfMtfFile.getText());
    mtfFilterParam.setMaximumInverse(ltfMaximumInverse.getText());
    mtfFilterParam.setInverseRolloffRadiusSigma(ltfInverseRolloffRadiusSigma
        .getText());
  }

  public void setMTFFilterParam(ConstMTFFilterParam mtfFilterParam) {
    ltfMtfFile.setText(mtfFilterParam.getMtfFile());
    ltfMaximumInverse.setText(mtfFilterParam.getMaximumInverseString());
    ltfLowPassRadiusSigma.setText(mtfFilterParam.getLowPassRadiusSigmaString());
    ltfStartingAndEndingZ.setText(mtfFilterParam.getStartingAndEndingZString());
    ltfInverseRolloffRadiusSigma.setText(mtfFilterParam
        .getInverseRolloffRadiusSigmaString());
    enableUseFilter();
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
    pnlAdvanced1.setVisible(isAdvanced);
    pnlAdvanced2.setVisible(isAdvanced);
    pnlAdvanced3.setVisible(isAdvanced);
    pnlTrial.setVisible(isAdvanced);
    ltfStartingAndEndingZ.setVisible(isAdvanced);
    pnlInverseFilter.setVisible(isAdvanced);
    applicationManager.packMainWindow();
  }

  /**
   * Layout the newstack panel
   */
  private void layoutNewstPanel() {
    //  Setup the binning spinner for newstack
    SpinnerModel integerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinBinning = new LabeledSpinner("Aligned image stack binning ",
        integerModel);

    pnlNewstParams.setLayout(new BoxLayout(pnlNewstParams, BoxLayout.Y_AXIS));
    pnlNewstParams.setBorder(new EtchedBorder("Newstack Parameters")
        .getBorder());
    UIUtilities.addWithYSpace(pnlNewstParams, cbBoxUseLinearInterpolation);
    UIUtilities.addWithYSpace(pnlNewstParams, spinBinning.getContainer());
    UIUtilities.addWithYSpace(pnlNewstParams, cbFiducialess);
    UIUtilities.addWithYSpace(pnlNewstParams, ltfRotation.getContainer());
    UIUtilities.alignComponentsX(pnlNewstParams, Component.LEFT_ALIGNMENT);

    // Layout the newst button panel
    pnlAlignedStack.setLayout(new BoxLayout(pnlAlignedStack, BoxLayout.X_AXIS));
    UIUtilities.addWithSpace(pnlAlignedStack, btnNewst, FixedDim.x10_y0);
    pnlAlignedStack.add(btn3dmodFull);
    UIUtilities.setButtonSizeAll(pnlAlignedStack, UIParameters.dimButton);
  }

  /**
   * Layout the MTF filter panel
   *
   */
  private void layoutFilterPanel() {
    pnlFilter.setBorder(new EtchedBorder("2D Filtering (optional)").getBorder());
    pnlFilter.setLayout(new BoxLayout(pnlFilter, BoxLayout.Y_AXIS));

    //  Inverse filter sub panel
    pnlMtfFile.setLayout(new BoxLayout(pnlMtfFile, BoxLayout.X_AXIS));
    pnlMtfFile.add(ltfMtfFile.getContainer());
    pnlMtfFile.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlMtfFile.add(btnMtfFile);

    pnlInverseFilterParam.setLayout(new BoxLayout(pnlInverseFilterParam,
        BoxLayout.X_AXIS));
    UIUtilities.addWithSpace(pnlInverseFilterParam, ltfMaximumInverse
        .getContainer(), FixedDim.x5_y0);
    pnlInverseFilterParam.add(ltfInverseRolloffRadiusSigma.getContainer());

    pnlInverseFilter
        .setLayout(new BoxLayout(pnlInverseFilter, BoxLayout.Y_AXIS));
    pnlInverseFilter.setBorder(new EtchedBorder(
        "Inverse Filtering Parameters: ").getBorder());
    UIUtilities.addWithYSpace(pnlInverseFilter, pnlMtfFile);
    pnlInverseFilter.add(pnlInverseFilterParam);
    UIUtilities.alignComponentsX(pnlInverseFilter, Component.CENTER_ALIGNMENT);

    //  Filter buttons sub panel
    pnlFilterButtons
        .setLayout(new BoxLayout(pnlFilterButtons, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlFilterButtons, btnFilter);
    UIUtilities.addWithXSpace(pnlFilterButtons, btnViewFilter);
    UIUtilities.addWithXSpace(pnlFilterButtons, btnUseFilter);
    UIUtilities.setButtonSizeAll(pnlFilterButtons, UIParameters.dimButton);

    // Construct the panel
    UIUtilities.addWithYSpace(pnlFilter, ltfStartingAndEndingZ.getContainer());
    UIUtilities.addWithYSpace(pnlFilter, ltfLowPassRadiusSigma.getContainer());
    UIUtilities.addWithYSpace(pnlFilter, pnlInverseFilter);
    UIUtilities.addWithYSpace(pnlFilter, pnlFilterButtons);
    UIUtilities.alignComponentsX(pnlFilter, Component.CENTER_ALIGNMENT);
  }

  /**
   * Layout the tilt panel
   */
  private void layoutTiltPanel() {
    //  output density subpanel
    pnlDensityScaling.setLayout(new BoxLayout(pnlDensityScaling,
        BoxLayout.X_AXIS));
    pnlDensityScaling.add(ltfDensityScale.getContainer());
    pnlDensityScaling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlDensityScaling.add(ltfDensityOffset.getContainer());

    //  slice first/last subpanel
    pnlSlicesInY.setLayout(new BoxLayout(pnlSlicesInY, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlSlicesInY, ltfSliceStart.getContainer());
    UIUtilities.addWithXSpace(pnlSlicesInY, ltfSliceStop.getContainer());
    pnlSlicesInY.add(lblInY);

    //  offset subpanel
    pnlOffset.setLayout(new BoxLayout(pnlOffset, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlOffset, ltfXOffset.getContainer());
    pnlOffset.add(ltfZOffset.getContainer());

    // radial filter panel
    pnlRadialFilter.setLayout(new BoxLayout(pnlRadialFilter, BoxLayout.X_AXIS));
    UIUtilities.addWithXSpace(pnlRadialFilter, ltfRadialMax.getContainer());
    pnlRadialFilter.add(ltfRadialFallOff.getContainer());

    // Advanced sub panels there multiple panels because we need to intersperse
    // the advanced parameters with the basic
    pnlAdvanced1.setLayout(new BoxLayout(pnlAdvanced1, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlAdvanced1, ltfLogOffset.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced1, pnlDensityScaling);
    UIUtilities.addWithYSpace(pnlAdvanced1, ltfTomoWidth.getContainer());
    UIUtilities.alignComponentsX(pnlAdvanced1, Component.LEFT_ALIGNMENT);

    pnlAdvanced2.setLayout(new BoxLayout(pnlAdvanced2, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlAdvanced2, pnlSlicesInY);
    UIUtilities.addWithYSpace(pnlAdvanced2, ltfSliceIncr.getContainer());
    UIUtilities.addWithYSpace(pnlAdvanced2, pnlOffset);
    UIUtilities.alignComponentsX(pnlAdvanced2, Component.LEFT_ALIGNMENT);

    pnlAdvanced3.setLayout(new BoxLayout(pnlAdvanced3, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlAdvanced3, ltfTiltAngleOffset.getContainer());
    UIUtilities.alignComponentsX(pnlAdvanced3, Component.LEFT_ALIGNMENT);

    pnlTiltParams.setBorder(new EtchedBorder("Tilt Parameters").getBorder());
    pnlTiltParams.setLayout(new BoxLayout(pnlTiltParams, BoxLayout.Y_AXIS));
    pnlTiltParams.add(pnlAdvanced1);
    UIUtilities.addWithYSpace(pnlTiltParams, ltfTomoThickness.getContainer());
    pnlTiltParams.add(pnlAdvanced2);
    UIUtilities.addWithYSpace(pnlTiltParams, ltfXAxisTilt.getContainer());
    pnlTiltParams.add(pnlAdvanced3);
    UIUtilities.addWithYSpace(pnlTiltParams, pnlRadialFilter);

    //UIUtilities.addWithYSpace(pnlUseLocalAlignment, cbBoxUseLocalAlignment);
    UIUtilities.addWithYSpace(pnlTiltParams, cbBoxUseLocalAlignment);
    UIUtilities.addWithYSpace(pnlTiltParams, cbUseZFactors);
    //pnlTiltParams.add(pnlUseLocalAlignment);
    //pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTiltParams.add(pnlTrial);

    UIUtilities.alignComponentsX(pnlTiltParams, Component.LEFT_ALIGNMENT);

  }

  /**
   * Layout the trial tomogram panel
   */
  private void layoutTrialPanel() {
    // Filename subpanel
    cmboTrialTomogramName.setEditable(true);
    pnlTrialTomogramName.setLayout(new BoxLayout(pnlTrialTomogramName,
        BoxLayout.X_AXIS));
    pnlTrialTomogramName.add(lblTrialTomogramName);
    pnlTrialTomogramName.add(cmboTrialTomogramName);

    //  Trial button subpanel
    pnlTrialButtons.setLayout(new BoxLayout(pnlTrialButtons, BoxLayout.X_AXIS));
    UIUtilities.addWithSpace(pnlTrialButtons, btnTrial, FixedDim.x5_y0);
    UIUtilities.addWithSpace(pnlTrialButtons, btn3dmodTrial, FixedDim.x5_y0);
    pnlTrialButtons.add(btnUseTrial);
    UIUtilities.setButtonSizeAll(pnlTrialButtons, UIParameters.dimButton);

    //  Construct the panel
    pnlTrial.setLayout(new BoxLayout(pnlTrial, BoxLayout.Y_AXIS));
    pnlTrial.setBorder(new EtchedBorder("Trial Mode").getBorder());
    UIUtilities.addWithYSpace(pnlTrial, pnlTrialTomogramName);
    UIUtilities.addWithYSpace(pnlTrial, pnlTrialButtons);
    UIUtilities.alignComponentsX(pnlTrial, Component.CENTER_ALIGNMENT);
  }

  private void btnMtfFileAction(ActionEvent event) {
    //Open up the file chooser in the $IMOD_CALIB_DIR/Camera, if available,
    //otherwise open in the working directory
    String currentMtfDirectory = ltfMtfFile.getText();
    if (currentMtfDirectory.equals("")) {
      File calibrationDir = EtomoDirector.getInstance().getIMODCalibDirectory();
      File cameraDir = new File(calibrationDir.getAbsolutePath(), "Camera");
      if (cameraDir.exists()) {
        currentMtfDirectory = cameraDir.getAbsolutePath();
      }
      else {
        currentMtfDirectory = applicationManager.getPropertyUserDir();
      }
    }
    JFileChooser chooser = new JFileChooser(new File(currentMtfDirectory));
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
    String[] manPagelabel = {"Newst", "Tilt", "3dmod"};
    String[] manPage = {"newst.html", "tilt.html", "3dmod.html"};
    String[] logFileLabel = {"Newst", "Tilt"};
    String[] logFile = new String[2];
    logFile[0] = "newst" + axisID.getExtension() + ".log";
    logFile[1] = "tilt" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "TOMOGRAM GENERATION", ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, applicationManager);
  }

  public void startingAndEndingZKeyReleased(KeyEvent event) {
    enableUseFilter();
  }

  protected void enableUseFilter() {
    if (!enableFiltering) {
      btnUseFilter.setEnabled(false);
      return;
    }
    String startingAndEndingZ = ltfStartingAndEndingZ.getText();
    if (startingAndEndingZ.length() == 0 || startingAndEndingZ.matches("\\s+")) {
      btnFilter.setSelected(false);
      btnUseFilter.setEnabled(true);
    }
    else {
      btnUseFilter.setEnabled(false);
    }
  }
  
  public void enableUseZFactors(boolean enable) {
    cbUseZFactors.setEnabled(enable);
  }
  
  public void enableUseLocalAlignment(boolean enable) {
    cbBoxUseLocalAlignment.setEnabled(enable);
  }
  
  protected void updateFiducialess() {
    ltfRotation.setEnabled(cbFiducialess.isSelected());
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
        errorMessage[1] = "A filename for the trial tomogram must be entered in the Trial"
            + " tomogram filename edit box.";
        applicationManager.getMainPanel().openMessageDialog(errorMessage,
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
    else if (command.equals(cbFiducialess.getActionCommand())) {
      updateFiducialess();
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

  private class MtfFileActionListener implements ActionListener {
    TomogramGenerationDialog adaptee;

    MtfFileActionListener(TomogramGenerationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnMtfFileAction(event);
    }
  }

  class StartingAndEndingZKeyListener implements KeyListener {
    TomogramGenerationDialog adaptee;

    public StartingAndEndingZKeyListener(TomogramGenerationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void keyReleased(KeyEvent event) {
      adaptee.startingAndEndingZKeyReleased(event);
    }

    public void keyPressed(KeyEvent event) {
    }

    public void keyTyped(KeyEvent event) {
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

    text = "Make aligned stack with linear instead of cubic interpolation to "
        + "reduce noise.";
    cbBoxUseLinearInterpolation.setToolTipText(tooltipFormatter.setText(text)
        .format());
    text = "Generate the complete aligned stack for input into the tilt process."
        + "  This runs the newst.com script.";
    btnNewst.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Open the complete aligned stack in 3dmod";
    btn3dmodFull.setToolTipText(tooltipFormatter.setText(text).format());
    if (autodoc != null) {
      text = TooltipFormatter.getText(autodoc, "StartingAndEndingZ");
      if (text != null) {
        ltfStartingAndEndingZ.setToolTipText(tooltipFormatter.setText(text)
            .format());
      }
      text = TooltipFormatter.getText(autodoc, "LowPassRadiusSigma");
      if (text != null) {
        ltfLowPassRadiusSigma.setToolTipText(tooltipFormatter.setText(text)
            .format());
      }
      text = TooltipFormatter.getText(autodoc, "MtfFile");
      if (text != null) {
        ltfMtfFile.setToolTipText(tooltipFormatter.setText(text).format());
      }
      text = TooltipFormatter.getText(autodoc, "MaximumInverse");
      if (text != null) {
        ltfMaximumInverse.setToolTipText(tooltipFormatter.setText(text)
            .format());
      }
      text = TooltipFormatter.getText(autodoc, "InverseRolloffRadiusSigma");
      if (text != null) {
        ltfInverseRolloffRadiusSigma.setToolTipText(tooltipFormatter.setText(
            text).format());
      }
    }
    text = "Run mtffilter on the full aligned stack.";
    btnFilter.setToolTipText(tooltipFormatter.setText(text).format());
    text = "View the results of running mtffilter on the full aligned stack.";
    btnViewFilter.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Use the results of running mtffilter as the new full aligned stack.";
    btnUseFilter.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Thickness, in pixels, along the z-axis of the reconstructed volume.";
    ltfTomoThickness.setToolTipText(tooltipFormatter.setText(text).format());
    text = "The first slice in the Y dimension to include in the reconstructed "
        + " volume.  Slices are numbered from 0, a last slice must also "
        + "be specified.";
    ltfSliceStart.setToolTipText(tooltipFormatter.setText(text).format());
    text = "The last slice in the Y dimension to include in the reconstructed "
        + " volume.  Slices are numbered from 0, a first slice must also "
        + "be specified.";
    ltfSliceStop.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Step between slices in the Y dimension.  A first and last slice must "
        + "also be entered. Default is 1.";
    ltfSliceIncr.setToolTipText(tooltipFormatter.setText(text).format());
    text = "This entry specifies the width of the output image; the default is the "
        + "width of the input image.";
    ltfTomoWidth.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Amount to shift the reconstructed slices in X before output.  A "
        + "positive offset will shift the slice to the right, and the "
        + "output will contain the left part of the whole potentially "
        + "reconstructable area.";
    ltfXOffset.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Amount to shift the reconstructed slices in Z before output.  A "
        + "positive offset will shift the slice upward.  Do not use this option"
        + " if you have fiducials and the tomogram is part of a dual-axis "
        + "series.";
    ltfZOffset.setToolTipText(tooltipFormatter.setText(text).format());
    text = "This line allows one to rotate the reconstruction around the X axis, so "
        + "that a section that appears to be tilted around the X axis can be "
        + "made flat to fit into a smaller volume.  The ANGLE should be the "
        + "tilt of the section relative to the X-Y plane in an unrotated "
        + "reconstruction.  For example, if the reconstruction extends 500 "
        + "slices, and the section is 5 pixels below the middle in the first "
        + "slice and 5 pixels above the middle in the last slice, ANGLE should"
        + " be 1.1 (the arc sine of 10/500).";
    ltfXAxisTilt.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Offset in degrees to apply to the tilt angles; a positive offset will "
        + "rotate the reconstructed slices counterclockwise.  Do not use this "
        + "option if you have fiducials and the tomogram is part of a dual-axis"
        + " series.";
    ltfTiltAngleOffset.setToolTipText(tooltipFormatter.setText(text).format());
    text = "The spatial frequency at which to switch from the R-weighted radial "
        + "filter to a Gaussian falloff.  Frequency is in cycles/pixel and "
        + "ranges from 0-0.5.  Both a cutoff and a falloff must be entered.";
    ltfRadialMax.setToolTipText(tooltipFormatter.setText(text).format());
    text = "The sigma value of a Gaussian which determines how fast the radial "
        + "filter falls off at spatial frequencies above the cutoff frequency."
        + "  Frequency is in cycles/pixel and ranges from 0-0.5.  Both a "
        + "cutoff and a falloff must be entered ";
    ltfRadialFallOff.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Amount to add to reconstructed density values before multiplying by"
        + " the scale factor and outputting the values.";
    ltfDensityOffset.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Amount to multiply reconstructed density values by, after adding the "
        + "offset value.";
    ltfDensityScale.setToolTipText(tooltipFormatter.setText(text).format());
    text = "This parameter allows one to generate a reconstruction using the "
        + "logarithm of the densities in the input file, with the value "
        + "specified added before taking the logarithm.  If no parameter is "
        + "specified the logarithm of the input data is not taken.";
    ltfLogOffset.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Select this checkbox to use local alignments.  You must have "
        + "created the local alignments in the Fine Alignment step";
    cbBoxUseLocalAlignment.setToolTipText(tooltipFormatter.setText(text)
        .format());
    text = "Compute the tomogram from the full aligned stack.  This runs "
        + "the tilt.com script.";
    btnTilt.setToolTipText(tooltipFormatter.setText(text).format());
    text = "View the reconstructed volume in 3dmod.";
    btn3dmodTomogram.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Current name of trial tomogram, which will be generated, viewed, or"
        + " used by the buttons below.";
    lblTrialTomogramName
        .setToolTipText(tooltipFormatter.setText(text).format());
    cmboTrialTomogramName.setToolTipText(tooltipFormatter.setText(text)
        .format());
    text = "Compute a trial tomogram with the current parameters, using the "
        + "filename in the \" Trial tomogram filename \" box.";
    btnTrial.setToolTipText(tooltipFormatter.setText(text).format());
    text = "View the trial tomogram whose name is shown in \"Trial "
        + "tomogram filename\" box.";
    btn3dmodTrial.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Rename the trial tomogram whose name is shown in the \"Trial "
        + "tomogram filename\" box to be the final tomogram.";
    btnUseTrial.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Delete the pre-aligned and aligned stack for this axis.  Once the "
        + "tomogram is calculated these intermediate files are not used and can be "
        + "" + "deleted to free up disk space.";
    btnDeleteStacks.setToolTipText(tooltipFormatter.setText(text).format());
    text = "Use cross-correlation alignment only.";
    cbFiducialess.setToolTipText(tooltipFormatter.setText(text).format()); 
    text = "Rotation angle of tilt axis for generating aligned stack from "
        + "cross-correlation alignment only.";
    ltfRotation.setToolTipText(tooltipFormatter.setText(text).format()); 
    text = "Set the binning for the aligned image stack and tomogram.  With a "
        + "binned tomogram, all of the thickness, position, and size parameters"
        + " below are still entered in unbinned pixels.";
    spinBinning.setToolTipText(tooltipFormatter.setText(text).format()); 
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