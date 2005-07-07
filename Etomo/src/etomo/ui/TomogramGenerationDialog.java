
package etomo.ui;

import java.awt.Component;
import java.awt.Container;
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

import javax.swing.BorderFactory;
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
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstMTFFilterParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.TiltParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.storage.MtfFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.MetaData;
import etomo.type.ViewType;
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
 * Revision 3.47  2005/07/06 23:51:57  sueh
 * bug# 437 Rewrote the layout functions to include panel headers in a
 * standard way.  Rewrote setAdvanced().  Added individual setAdvanced
 * functions for each panel.  Added exapand().
 *
 * Revision 3.46  2005/07/01 21:26:01  sueh
 * bug# 619 Added check box cbParallelProcess to override normal
 * tomogram generation functionality.  Only displays when --newstuff is set.
 *
 * Revision 3.45  2005/06/13 23:38:35  sueh
 * bug# 583 Preventing tilt.com from being overwritten with a default
 * imageBinned after the .ali file is deleted.  DoneTomogramGeneration()
 * needs update and save tilt.com, but the result from getStackBinning will
 * be wrong if the .ali file has been deleted.  Move the responsibility for
 * getting the right imageBinned to TiltParam.  Modify getStackBinning() to
 * have an option to return a null value when it fails to calculate the stack
 * binning.  If TiltParam.setImageBinned() gets a null value and
 * imageBinned is not null, it won't override the current imageBinned value.
 *
 * Revision 3.44  2005/06/11 02:53:48  sueh
 * bug# 583, bug# 682, bug# 584  Storing screen binning for Tomo Pos and
 * Tomo Gen in MetaData separately (Tomo Pos default is 3).  Upgraded
 * tilt.com to have all unbinned parameters and a binning value.  No longer
 * managing full image size in tilt.com, except to upgrade the file.
 *
 * Revision 3.43  2005/06/01 21:27:51  sueh
 * bug# 667 Standardizing getMetaData function names.
 *
 * Revision 3.42  2005/04/25 21:41:37  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.  Move the interface for
 * popping up message dialogs to UIHarness.  It prevents headless
 * exceptions during a test execution.  It also allows logging of dialog
 * messages during a test.  It also centralizes the dialog interface and
 * allows the dialog functions to be synchronized to prevent dialogs popping
 * up in both windows at once.  All Frame functions will use UIHarness as a
 * public interface.
 *
 * Revision 3.41  2005/04/21 20:55:14  sueh
 * bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * that requires it.
 *
 * Revision 3.40  2005/04/16 02:05:14  sueh
 * bug# 615 Moved the adding of exit buttons to the base class.
 *
 * Revision 3.39  2005/03/29 19:54:50  sueh
 * bug# 623 Added getBinning().
 *
 * Revision 3.38  2005/03/11 01:38:28  sueh
 * bug# 533 Added get and setBlendParams to get and set
 * linearInterpolation in BlendmontParam.
 *
 * Revision 3.37  2005/03/09 22:33:23  sueh
 * bug# 533 Modify the context sensitive help so that it displays blendmont
 * man pages and log files instead of newst when the view type is montage.
 * Change "Newstack Parameters" to blendmont.
 *
 * Revision 3.36  2005/03/09 18:12:29  sueh
 * bug# 533 In the final alignment box disable linear interpolation and binning
 * when the view type is montage.
 *
 * Revision 3.35  2005/03/02 00:13:49  sueh
 * Corrected label.
 *
 * Revision 3.34  2005/02/19 00:31:21  sueh
 * bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * and localalignments.  Add them for A and B.
 *
 * Revision 3.33  2005/02/11 16:46:27  sueh
 * bug# 600 Getting tooltips using EtomoAutodoc instead of TooltipFormatter.
 *
 * Revision 3.32  2005/01/14 03:11:32  sueh
 * bug# 511 Added DialogType to super constructor.
 *
 * Revision 3.31  2005/01/12 18:35:32  sueh
 * bug# 505 Added ltfExtraExcludeList.
 *
 * Revision 3.30  2005/01/12 02:39:20  sueh
 * bug# 579 setting use local alignment correctly
 *
 * Revision 3.29  2005/01/12 00:48:10  sueh
 * bug# 579 Stop enabling/disabling Use local alignments checkbox when
 * fiducialess checkbox is selected/unselected.  This needs to initiated from
 * ProcessManager.postProcess().  Setting/getting Use local alignments
 * checkbox from metaData.
 *
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
      FiducialessParams, Expandable {
  public static final String rcsid = "$Id$";

  private JPanel pnlTilt = new JPanel();

  // Fiducialess parameters
  private JCheckBox cbFiducialess = new JCheckBox("Fiducialless alignment");
  private LabeledTextField ltfRotation = new LabeledTextField(
      "Tilt axis rotation: ");

  // Newst/Newstack objects
  private JCheckBox cbBoxUseLinearInterpolation = new JCheckBox(
      "Use linear interpolation");
  private LabeledSpinner spinBinning;

  //  Aligned stack buttons
  private MultiLineToggleButton btnNewst = new MultiLineToggleButton(
      "<html><b>Create Full<br>Aligned Stack</b>");
  private MultiLineButton btn3dmodFull = new MultiLineButton(
      "<html><b>View Full<br>Aligned Stack</b>");

  //  Tilt objects
  private SpacedTextField ltfXOffset = new SpacedTextField("X offset:");
  private SpacedTextField ltfZOffset = new SpacedTextField("Z offset: ");
  private SpacedTextField ltfSliceStart = new SpacedTextField("First slice: ");
  private SpacedTextField ltfSliceStop = new SpacedTextField("Last slice: ");
  private SpacedLabel lblInY = new SpacedLabel(" in Y");
  private SpacedTextField ltfSliceIncr = new SpacedTextField(
      "Slice step in Y: ");
  private SpacedTextField ltfTomoWidth = new SpacedTextField(
      "Tomogram width in X: ");
  private SpacedTextField ltfTomoThickness = new SpacedTextField(
      "Tomogram thickness in Z: ");
  private SpacedTextField ltfXAxisTilt = new SpacedTextField("X axis tilt: ");
  private SpacedTextField ltfTiltAngleOffset = new SpacedTextField(
      "Tilt angle offset: ");
  private SpacedTextField ltfRadialMax = new SpacedTextField(
      "Radial filter cutoff: ");
  private SpacedTextField ltfRadialFallOff = new SpacedTextField("Falloff: ");
  private SpacedTextField ltfDensityOffset = new SpacedTextField("Offset: ");
  private SpacedTextField ltfDensityScale = new SpacedTextField(
      "Output density scaling factor: ");
  private SpacedTextField ltfLogOffset = new SpacedTextField("Log offset: ");
  private JCheckBox cbBoxUseLocalAlignment = new JCheckBox(
      "Use local alignments");

  //  Trial tomogram objects

  private JLabel lblTrialTomogramName = new JLabel("Trial tomogram filename: ");
  private JComboBox cmboTrialTomogramName = new JComboBox();
  private Vector trialTomogramList = new Vector();
  private MultiLineButton btnTrial = new MultiLineButton(
      "<html><b>Generate Trial Tomogram</b>");
  private MultiLineButton btn3dmodTrial = new MultiLineButton(
      "<html><b>View Trial in 3dmod</b>");
  private MultiLineToggleButton btnUseTrial = new MultiLineToggleButton(
      "<html><b>Use Current Trial Tomogram</b>");

  // MTF Filter objects
  private LabeledTextField ltfLowPassRadiusSigma = new LabeledTextField(
      "Low pass (cutoff,sigma): ");
  private ImageIcon iconFolder = new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif"));
  private LabeledTextField ltfMtfFile = new LabeledTextField("MTF file: ");
  private JButton btnMtfFile = new JButton(iconFolder);
  private LabeledTextField ltfMaximumInverse = new LabeledTextField(
      "Maximum Inverse: ");
  private LabeledTextField ltfInverseRolloffRadiusSigma = new LabeledTextField(
      "Rolloff (radius,sigma): ");
  private MultiLineToggleButton btnFilter = new MultiLineToggleButton("Filter");
  private MultiLineButton btnViewFilter = new MultiLineButton(
      "View Filtered Stack");
  private MultiLineToggleButton btnUseFilter = new MultiLineToggleButton(
      "Use Filtered Stack");
  private SpacedTextField ltfStartingAndEndingZ = new SpacedTextField(
      "Starting and ending views: ");
  boolean enableFiltering = false;

  //  Tomogram generation buttons
  private MultiLineToggleButton btnTilt = new MultiLineToggleButton(
      "<html><b>Generate Tomogram</b>");
  private MultiLineButton btn3dmodTomogram = new MultiLineButton(
      "<html><b>View Tomogram In 3dmod</b>");
  private MultiLineToggleButton btnDeleteStacks = new MultiLineToggleButton(
      "<html><b>Delete Aligned Image Stack</b>");
  private JCheckBox cbUseZFactors = new JCheckBox("Use Z factors");
  private SpacedTextField ltfExtraExcludeList = new SpacedTextField(
  "Extra views to exclude: ");
  private JCheckBox cbParallelProcess = new JCheckBox("Parallel Processing");
  //headers should not go into garbage collection
  private PanelHeader newstHeader;
  private PanelHeader tiltHeader;
  private PanelHeader trialHeader;
  private PanelHeader filterHeader;
  //panels that are changed in setAdvanced()
  private SpacedPanel inverseParamsPanel;
  private JPanel trialPanel;

  
  //backward compatibility functionality - if the metadata binning is missing
  //get binning from newst
  private boolean getBinningFromNewst = true;

  public TomogramGenerationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID, DialogType.TOMOGRAM_GENERATION);
    fixRootPanel(rootSize);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");
    
    // Layout the main panel (and sub panels) and add it to the root panel
    pnlTilt.setBorder(new BeveledBorder("Tomogram Generation").getBorder());
    pnlTilt.setLayout(new BoxLayout(pnlTilt, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlTilt, layoutNewstPanel());
    UIUtilities.addWithYSpace(pnlTilt, layoutFilterPanel());
    UIUtilities.addWithYSpace(pnlTilt, layoutTiltPanel());
    UIUtilities.alignComponentsX(pnlTilt, Component.CENTER_ALIGNMENT);

    rootPanel.add(pnlTilt);
    addExitButtons();

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
    cbParallelProcess.addActionListener(tomogramGenerationListener);

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
    if (getBinningFromNewst) {
      spinBinning.setValue(newstParam.getBinByFactor());
    }
  }
  
  public void setParameters(ConstMetaData metaData) {
    ConstEtomoNumber binning = metaData.getTomoGenBinning(axisID);
    if (!binning.isNull()) {
      getBinningFromNewst = false;
      spinBinning.setValue(binning);
    }
  }
  
  public void getParameters(MetaData metaData) {
    metaData.setTomoGenBinning(axisID, ((Integer) spinBinning.getValue()).intValue());
  }
  
  public void setBlendParams(BlendmontParam blendmontParam) {
    cbBoxUseLinearInterpolation.setSelected(blendmontParam.isLinearInterpolation());
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
    cbBoxUseLocalAlignment.setSelected(metaData.getUseLocalAlignments(axisID));
    cbUseZFactors.setSelected(metaData.getUseZFactors(axisID).is());
    ltfExtraExcludeList.setText(tiltParam.getExcludeList2());
  }
  
  public int getBinning() {
    return ((Integer) spinBinning.getValue()).intValue();
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

  public void getBlendParams(BlendmontParam blendmontParam) {
    blendmontParam.setLinearInterpolation(cbBoxUseLinearInterpolation.isSelected());
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  public void getTiltParams(TiltParam tiltParam) throws NumberFormatException,
      InvalidParameterException {
    String badParameter = "";
    MetaData metaData = applicationManager.getMetaData();
    try {
      badParameter = "IMAGEBINNED";
      tiltParam.setImageBinned();
      //Do not manage full image size.  It is coming from copytomocoms.
      // Set the appropriate FULLIMAGE line
      //badParameter = "FULLIMAGE";
      //tiltParam.setFullImageX(fullImageSize.width);
      //tiltParam.setFullImageY(fullImageSize.height);

      if (ltfTomoWidth.getText().matches("\\S+")) {
        badParameter = ltfTomoWidth.getLabel();
        tiltParam.setWidth(Integer.parseInt(ltfTomoWidth.getText()));
      }
      else {
        tiltParam.resetWidth();
      }

      //set Z offset
      if (ltfZOffset.getText().matches("\\S+")) {
        badParameter = ltfZOffset.getLabel();
        tiltParam.setZOffset(Float.parseFloat(ltfZOffset.getText()));
      }
      else {
        tiltParam.resetZOffset();
      }
      
      //set X offset
      if (ltfXOffset.getText().matches("\\S+")) {
        badParameter = ltfXOffset.getLabel();
        tiltParam.setXOffset(Float.parseFloat(ltfXOffset.getText()));
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
        tiltParam.setIdxSliceStart(Integer.parseInt(ltfSliceStart.getText()));
        badParameter = ltfSliceStop.getLabel();
        tiltParam.setIdxSliceStop(Integer.parseInt(ltfSliceStop.getText()));
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
        tiltParam.setThickness(Integer.parseInt(ltfTomoThickness.getText()));
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

      if (cbBoxUseLocalAlignment.isSelected() && cbBoxUseLocalAlignment.isEnabled()) {
        tiltParam.setLocalAlignFile(applicationManager.getMetaData()
            .getDatasetName()
            + axisID.getExtension() + "local.xf");
      }
      else {
        tiltParam.setLocalAlignFile("");
      }
      metaData.setUseLocalAlignments(axisID, cbBoxUseLocalAlignment.isSelected());
      tiltParam.setFiducialess(cbFiducialess.isSelected());
      tiltParam.setUseZFactors(cbUseZFactors.isSelected() && cbUseZFactors.isEnabled());
      metaData.setUseZFactors(axisID, cbUseZFactors.isSelected());
      tiltParam.setExcludeList2(ltfExtraExcludeList.getText());
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
  
  
  public void expand(ExpandButton button) {
    if (tiltHeader.equalsAdvancedBasic(button)) {
      updateAdvancedTilt(button.isExpanded());
    }
    else if (filterHeader.equalsAdvancedBasic(button)) {
      updateAdvancedFilter(button.isExpanded());
    }
    else {
      return;
    }
    applicationManager.packMainWindow(axisID);
  }
  
  private void updateAdvancedFilter(boolean advanced) {
    ltfStartingAndEndingZ.setVisible(advanced);
    //ltfLowPassRadiusSigma
    inverseParamsPanel.setVisible(advanced);
    //btnFilter
    //btnViewFilter
    //btnUseFilter
  }

  private void updateAdvancedTilt(boolean advanced) {
    ltfLogOffset.setVisible(advanced);
    ltfDensityOffset.setVisible(advanced);
    ltfDensityScale.setVisible(advanced);
    ltfTomoWidth.setVisible(advanced);
    //ltfTomoThickness
    ltfSliceStart.setVisible(advanced);
    ltfSliceStop.setVisible(advanced);
    lblInY.setVisible(advanced);
    ltfSliceIncr.setVisible(advanced);
    ltfXOffset.setVisible(advanced);
    ltfZOffset.setVisible(advanced);
    //ltfXAxisTilt
    ltfTiltAngleOffset.setVisible(advanced);
    //ltfRadialMax
    //ltfRadialFallOff
    ltfExtraExcludeList.setVisible(advanced);
    //cbBoxUseLocalAlignment
    //cbUseZFactors
    //cbParallelProcess
    trialPanel.setVisible(advanced);
    //btnTilt
    //btn3dmodTomogram
    //btnDeleteStacks
  }
  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    filterHeader.setAdvanced(isAdvanced);
    tiltHeader.setAdvanced(isAdvanced);

    applicationManager.packMainWindow(axisID);
  }

  /**
   * Layout the newstack panel
   */
  private JPanel layoutNewstPanel() {//TODO
    //panels
    JPanel newstPanel = new JPanel();
    newstPanel.setLayout(new BoxLayout(newstPanel, BoxLayout.Y_AXIS));
    newstPanel.setBorder(BorderFactory.createEtchedBorder());
    SpacedPanel newstBodyPanel = new SpacedPanel();
    newstBodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel buttonPanel = new SpacedPanel();
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      newstHeader = new PanelHeader(axisID, "Blendmont", newstBodyPanel);
    }
    else {
      newstHeader = new PanelHeader(axisID, "Newstack", newstBodyPanel);
    }
    //initialization
    SpinnerModel integerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinBinning = new LabeledSpinner("Aligned image stack binning ", integerModel);
    //buttonPanel
    buttonPanel.add(btnNewst);
    buttonPanel.add(btn3dmodFull);
    buttonPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //newstBodyPanel
    newstBodyPanel.add(cbBoxUseLinearInterpolation);
    newstBodyPanel.add(spinBinning);
    newstBodyPanel.add(cbFiducialess);
    newstBodyPanel.add(ltfRotation);
    newstBodyPanel.add(buttonPanel);
    newstBodyPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //newstPanel
    newstPanel.add(newstHeader.getContainer());
    newstPanel.add(newstBodyPanel.getContainer());
    UIUtilities.alignComponentsX(newstPanel, Component.LEFT_ALIGNMENT);
    //configure
    newstHeader.setOpen(true);

    return newstPanel;
  }

  /**
   * Layout the MTF filter panel
   *
   */
  private JPanel layoutFilterPanel() {
    //panels
    JPanel filterPanel = new JPanel();
    filterPanel.setLayout(new BoxLayout(filterPanel, BoxLayout.Y_AXIS));
    filterPanel.setBorder(BorderFactory.createEtchedBorder());
    JPanel filterBodyPanel = new JPanel();
    filterBodyPanel.setLayout(new BoxLayout(filterBodyPanel, BoxLayout.Y_AXIS));
    inverseParamsPanel = new SpacedPanel(true);
    inverseParamsPanel.setBoxLayout(BoxLayout.Y_AXIS);
    inverseParamsPanel.setBorder(new EtchedBorder("Inverse Filtering Parameters: ").getBorder());
    SpacedPanel mtfFilePanel = new SpacedPanel();
    mtfFilePanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel inversePanel = new SpacedPanel();
    inversePanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel buttonPanel = new SpacedPanel(true);
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    filterHeader = new PanelHeader(axisID, "2D Filtering (optional)", filterBodyPanel, this);
    //buttonPanel
    buttonPanel.add(btnFilter);
    buttonPanel.add(btnViewFilter);
    buttonPanel.add(btnUseFilter);
    //inversePanel
    inversePanel.add(ltfMaximumInverse);
    inversePanel.add(ltfInverseRolloffRadiusSigma);
    //mtfFilePanel
    mtfFilePanel.add(ltfMtfFile);
    mtfFilePanel.add(btnMtfFile);
    //inverseParamsPanel
    inverseParamsPanel.add(mtfFilePanel);
    inverseParamsPanel.add(inversePanel);
    //filterBodyPanel
    filterBodyPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    filterBodyPanel.add(ltfStartingAndEndingZ.getContainer());
    filterBodyPanel.add(ltfLowPassRadiusSigma.getContainer());
    filterBodyPanel.add(inverseParamsPanel.getContainer());
    filterBodyPanel.add(buttonPanel.getContainer());
    //filterPanel
    filterPanel.add(filterHeader.getContainer());
    filterPanel.add(filterBodyPanel);
    //configure
    filterHeader.setOpen(true);
    ButtonHelper.setStandardSize(btnFilter);
    ButtonHelper.setStandardSize(btnViewFilter);
    ButtonHelper.setStandardSize(btnUseFilter);
    return filterPanel;
  }

  /**
   * Layout the tilt panel
   */
  private Container layoutTiltPanel() {
    //panels
    JPanel tiltPanel = new JPanel();
    tiltPanel.setLayout(new BoxLayout(tiltPanel, BoxLayout.Y_AXIS));
    tiltPanel.setBorder(BorderFactory.createEtchedBorder());
    JPanel tiltBodyPanel = new JPanel();
    tiltBodyPanel.setLayout(new BoxLayout(tiltBodyPanel, BoxLayout.Y_AXIS));
    JPanel densityPanel = new JPanel();
    densityPanel.setLayout(new BoxLayout(densityPanel, BoxLayout.X_AXIS));
    JPanel slicesInYPanel = new JPanel();
    slicesInYPanel.setLayout(new BoxLayout(slicesInYPanel, BoxLayout.X_AXIS));
    JPanel offsetPanel = new JPanel();
    offsetPanel.setLayout(new BoxLayout(offsetPanel, BoxLayout.X_AXIS));
    JPanel radialPanel = new JPanel();
    radialPanel.setLayout(new BoxLayout(radialPanel, BoxLayout.X_AXIS));
    JPanel checkBoxPanel = new JPanel();
    checkBoxPanel.setLayout(new BoxLayout(checkBoxPanel, BoxLayout.X_AXIS));
    JPanel westCheckBoxPanel = new JPanel();
    westCheckBoxPanel.setLayout(new BoxLayout(westCheckBoxPanel, BoxLayout.Y_AXIS));
    SpacedPanel trialPanel = new SpacedPanel();
    trialPanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel buttonPanel = new SpacedPanel(true);
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    tiltHeader = new PanelHeader(axisID, "Tilt", tiltBodyPanel, this);
    //buttonPanel
    buttonPanel.add(btnTilt);
    buttonPanel.add(btn3dmodTomogram);
    buttonPanel.add(btnDeleteStacks);
    buttonPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //trialPanel
    trialPanel.add(layoutTrialPanel());
    trialPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //westCheckBoxPanel
    westCheckBoxPanel.add(cbBoxUseLocalAlignment);
    westCheckBoxPanel.add(cbUseZFactors);
    UIUtilities.alignComponentsX(westCheckBoxPanel, Component.LEFT_ALIGNMENT);
    //checkBoxPanel
    checkBoxPanel.add(westCheckBoxPanel);
    checkBoxPanel.add(Box.createHorizontalStrut(125));
    checkBoxPanel.add(cbParallelProcess);
    UIUtilities.alignComponentsX(checkBoxPanel, Component.LEFT_ALIGNMENT);
    //radialPanel
    radialPanel.add(ltfRadialMax.getContainer());
    radialPanel.add(ltfRadialFallOff.getContainer());
    UIUtilities.alignComponentsX(radialPanel, Component.LEFT_ALIGNMENT);
    //offsetPanel
    offsetPanel.add(ltfXOffset.getContainer());
    offsetPanel.add(ltfZOffset.getContainer());
    UIUtilities.alignComponentsX(offsetPanel, Component.LEFT_ALIGNMENT);
    //slicesInYPanel
    slicesInYPanel.add(ltfSliceStart.getContainer());
    slicesInYPanel.add(ltfSliceStop.getContainer());
    slicesInYPanel.add(lblInY.getContainer());
    UIUtilities.alignComponentsX(slicesInYPanel, Component.LEFT_ALIGNMENT);
    //densityPanel
    densityPanel.add(ltfDensityScale.getContainer());
    densityPanel.add(ltfDensityOffset.getContainer());
    UIUtilities.alignComponentsX(densityPanel, Component.LEFT_ALIGNMENT);
    //tiltBodyPanel
    tiltBodyPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    tiltBodyPanel.add(ltfLogOffset.getContainer());
    tiltBodyPanel.add(densityPanel);
    tiltBodyPanel.add(ltfTomoWidth.getContainer());
    tiltBodyPanel.add(ltfTomoThickness.getContainer());
    tiltBodyPanel.add(slicesInYPanel);
    tiltBodyPanel.add(ltfSliceIncr.getContainer());
    tiltBodyPanel.add(offsetPanel);
    tiltBodyPanel.add(ltfXAxisTilt.getContainer());
    tiltBodyPanel.add(ltfTiltAngleOffset.getContainer());
    tiltBodyPanel.add(radialPanel);
    tiltBodyPanel.add(ltfExtraExcludeList.getContainer());
    tiltBodyPanel.add(checkBoxPanel);
    tiltBodyPanel.add(trialPanel.getContainer());
    tiltBodyPanel.add(buttonPanel.getContainer());
    UIUtilities.alignComponentsX(tiltBodyPanel, Component.LEFT_ALIGNMENT);
    //tiltPanel
    tiltPanel.add(tiltHeader.getContainer());
    tiltPanel.add(tiltBodyPanel);
    UIUtilities.alignComponentsX(tiltPanel, Component.LEFT_ALIGNMENT);
    //configure
    tiltHeader.setOpen(true);
    ButtonHelper.setStandardSize(btnTilt);
    ButtonHelper.setStandardSize(btn3dmodTomogram);
    ButtonHelper.setStandardSize(btnDeleteStacks);
    return tiltPanel;
  }

  /**
   * Layout the trial tomogram panel
   */
  private JPanel layoutTrialPanel() {
    //panels
    trialPanel = new JPanel();
    trialPanel.setLayout(new BoxLayout(trialPanel, BoxLayout.Y_AXIS));
    trialPanel.setBorder(BorderFactory.createEtchedBorder());
    SpacedPanel trialBodyPanel = new SpacedPanel();
    trialBodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel northPanel = new SpacedPanel();
    northPanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel buttonPanel = new SpacedPanel();
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    trialHeader = new PanelHeader(axisID, "Trial Tilt", trialBodyPanel);
    //buttonPanel
    buttonPanel.add(btnTrial);
    buttonPanel.add(btn3dmodTrial);
    buttonPanel.add(btnUseTrial);
    //northPanel
    northPanel.add(lblTrialTomogramName);
    northPanel.add(cmboTrialTomogramName);
    //trialBodyPanel
    trialBodyPanel.addRigidArea();
    trialBodyPanel.add(northPanel);
    trialBodyPanel.add(buttonPanel);
    //trialPanel
    trialPanel.add(trialHeader.getContainer());
    trialPanel.add(trialBodyPanel.getContainer());
    //configure
    trialHeader.setOpen(true);
    cmboTrialTomogramName.setEditable(true);
    return trialPanel;
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
    String alignManpageLabel;
    String alignManpage;
    String alignLogfileLabel;
    String alignLogfile;
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      alignManpageLabel = "Blendmont";
      alignManpage = "blendmont";
      alignLogfileLabel = "Blend";
      alignLogfile = "blend";
    }
    else {
      alignManpageLabel = "Newstack";
      alignManpage = "newstack";
      alignLogfileLabel = "Newst";
      alignLogfile = "newst";
    }
    String[] manPagelabel = {alignManpageLabel, "Tilt", "3dmod"};
    String[] manPage = {alignManpage + ".html", "tilt.html", "3dmod.html"};
    String[] logFileLabel = {alignLogfileLabel, "Tilt"};
    String[] logFile = new String[2];
    logFile[0] = alignLogfile + axisID.getExtension() + ".log";
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
        UIHarness.INSTANCE.openMessageDialog(errorMessage,
            "Tilt Parameter Syntax Error", axisID);
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
      if (cbParallelProcess.isSelected()) {
        applicationManager.parallelProcess(this.dialogType, axisID);
      }
      else {
        applicationManager.tilt(axisID);
      }
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
      autodoc = Autodoc.get(Autodoc.MTF_FILTER, axisID);
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
      text = EtomoAutodoc.getTooltip(autodoc, "StartingAndEndingZ");
      if (text != null) {
        ltfStartingAndEndingZ.setToolTipText(tooltipFormatter.setText(text)
            .format());
      }
      text = EtomoAutodoc.getTooltip(autodoc, "LowPassRadiusSigma");
      if (text != null) {
        ltfLowPassRadiusSigma.setToolTipText(tooltipFormatter.setText(text)
            .format());
      }
      text = EtomoAutodoc.getTooltip(autodoc, "MtfFile");
      if (text != null) {
        ltfMtfFile.setToolTipText(tooltipFormatter.setText(text).format());
      }
      text = EtomoAutodoc.getTooltip(autodoc, "MaximumInverse");
      if (text != null) {
        ltfMaximumInverse.setToolTipText(tooltipFormatter.setText(text)
            .format());
      }
      text = EtomoAutodoc.getTooltip(autodoc, "InverseRolloffRadiusSigma");
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
}