package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.TiltParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
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
 * <p> Revision 2.10  2003/09/08 22:51:25  rickg
 * <p> Added commit test volume action
 * <p>
 * <p> Revision 2.9  2003/09/08 05:47:09  rickg
 * <p> Added trial tilt
 * <p> Output for a single axis tomogram is changed to
 * <p> dataset_full.rec
 * <p>
 * <p> Revision 2.8  2003/06/25 22:14:57  rickg
 * <p> Constructed a panel for the tilt parameters
 * <p>
 * <p> Revision 2.7  2003/06/23 23:26:59  rickg
 * <p> Added advanced options/parameters
 * <p>
 * <p> Revision 2.6  2003/05/23 22:14:11  rickg
 * <p> Removed any extensions from log file labels in context menu
 * <p>
 * <p> Revision 2.5  2003/05/23 21:26:55  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.4  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.3  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.2  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.1  2003/01/24 21:04:18  rickg
 * <p> AxisID bug fix from single buttonAction function
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.6.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.6  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.5  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>t
 * <p> Revision 1.3  2002/10/17 22:40:22  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TomogramGenerationDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel pnlTilt = new JPanel();
  private JPanel pnlNewstParams = new JPanel();
  private BeveledBorder border = new BeveledBorder("Tomogram Generation");
  
	private JCheckBox chkBoxUseLinearInterpolation =
		new JCheckBox("Use linear interpolation");

  private JToggleButton btnNewst =
    new JToggleButton("<html><b>Create full<br>aligned stack</b>");

  private JPanel pnlTiltParams = new JPanel();

  private LabeledTextField ltfXOffset = new LabeledTextField("X offset: ");
  private LabeledTextField ltfZOffset = new LabeledTextField("Z offset: ");
  private LabeledTextField ltfSliceStart =
    new LabeledTextField("First slice: ");
  private LabeledTextField ltfSliceStop = new LabeledTextField("Last slice: ");
  private LabeledTextField ltfSliceStep = new LabeledTextField("Slice step: ");

  private LabeledTextField ltfTomoWidth =
    new LabeledTextField("Tomogram width: ");
  private LabeledTextField ltfTomoThickness =
    new LabeledTextField("Tomogram thickness: ");

  private LabeledTextField ltfXAxisTilt = new LabeledTextField("X axis tilt: ");

  private LabeledTextField ltfTiltAngleOffset =
    new LabeledTextField("Tilt angle offset: ");
  private LabeledTextField ltfTiltAxisOffset =
    new LabeledTextField("Tilt axis offset: ");

  private LabeledTextField ltfRadialMax = new LabeledTextField("Radial max: ");
  private LabeledTextField ltfRadialFallOff =
    new LabeledTextField("Radial falloff: ");
  private LabeledTextField ltfDensityOffset =
    new LabeledTextField("Density offset (FLEVL): ");
  private LabeledTextField ltfDensityScale =
    new LabeledTextField("Density scale (SCALE): ");
  private LabeledTextField ltfLogOffset = new LabeledTextField("Log offset: ");
	
  private JCheckBox chkBoxUseLocalAlignment =
    new JCheckBox("Use local alignments");

  private JPanel pnlTrial = new JPanel();
  private JPanel pnlTrialTomogramName = new JPanel();
  private JComboBox cmboTrialTomogramName = new JComboBox();
  private Vector trialTomogramList = new Vector();

  private JPanel pnlTrialButtons = new JPanel();
  private JButton btnTrial =
    new JButton("<html><b>Generate trial tomogram</b>");
  private JButton btn3dmodTrial =
    new JButton("<html><b>View trial tomogram in 3dmod</b>");
  private JToggleButton btnCommit =
    new JToggleButton("<html><b>Commit current trial tomogram</b>");

  private JToggleButton btnTilt =
    new JToggleButton("<html><b>Generate<br>tomogram</b>");
  private JButton btn3dmod =
    new JButton("<html><b>View tomogram<br>in 3dmod</b>");

  public TomogramGenerationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    buttonExecute.setText("Done");
    
		chkBoxUseLinearInterpolation.setAlignmentX(Component.CENTER_ALIGNMENT);		
    btnNewst.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnNewst.setPreferredSize(FixedDim.button2Line);
    btnNewst.setMaximumSize(FixedDim.button2Line);
    btnNewst.addActionListener(new TomogramGenerationActionListener(this));

    pnlTrial.setLayout(new BoxLayout(pnlTrial, BoxLayout.Y_AXIS));
    pnlTrial.setBorder(new EtchedBorder("Trial mode").getBorder());
    cmboTrialTomogramName.setEditable(true);
    pnlTrialTomogramName.setLayout(
      new BoxLayout(pnlTrialTomogramName, BoxLayout.X_AXIS));
    pnlTrialTomogramName.add(new JLabel("Trial tomogram filename: "));
    pnlTrialTomogramName.add(cmboTrialTomogramName);

    pnlTrialButtons.setLayout(new BoxLayout(pnlTrialButtons, BoxLayout.X_AXIS));
    pnlTrialButtons.add(btnTrial);
    pnlTrialButtons.add(btn3dmodTrial);
    pnlTrialButtons.add(btnCommit);

    pnlTrial.add(pnlTrialTomogramName);
    pnlTrial.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTrial.add(pnlTrialButtons);

    btnTrial.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTrial.setPreferredSize(FixedDim.button2Line);
    btnTrial.setMaximumSize(FixedDim.button2Line);
    btnTrial.addActionListener(new TomogramGenerationActionListener(this));

    btn3dmodTrial.setAlignmentX(Component.CENTER_ALIGNMENT);
    btn3dmodTrial.setPreferredSize(FixedDim.button2Line);
    btn3dmodTrial.setMaximumSize(FixedDim.button2Line);
    btn3dmodTrial.addActionListener(new TomogramGenerationActionListener(this));

    btnCommit.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnCommit.setPreferredSize(FixedDim.button2Line);
    btnCommit.setMaximumSize(FixedDim.button2Line);
    btnCommit.addActionListener(new TomogramGenerationActionListener(this));

    btnTilt.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTilt.setPreferredSize(FixedDim.button2Line);
    btnTilt.setMaximumSize(FixedDim.button2Line);
    btnTilt.addActionListener(new TomogramGenerationActionListener(this));

    btn3dmod.setAlignmentX(Component.CENTER_ALIGNMENT);
    btn3dmod.setPreferredSize(FixedDim.button2Line);
    btn3dmod.setMaximumSize(FixedDim.button2Line);
    btn3dmod.addActionListener(new TomogramGenerationActionListener(this));

    chkBoxUseLocalAlignment.setAlignmentX(Component.CENTER_ALIGNMENT);

    layoutTiltPanel();
    layoutNewstPanel();
    
	pnlTilt.setBorder(border.getBorder());
	pnlNewstParams.setLayout(new BoxLayout(pnlNewstParams, BoxLayout.Y_AXIS));
	pnlTilt.setLayout(new BoxLayout(pnlTilt, BoxLayout.Y_AXIS));
	pnlTiltParams.setLayout(new BoxLayout(pnlTiltParams, BoxLayout.Y_AXIS));
    
	pnlTilt.add(pnlNewstParams);
	pnlTilt.add(Box.createRigidArea(FixedDim.x0_y5));
	pnlTilt.add(btnNewst);
	pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
	pnlTilt.add(pnlTiltParams);
	pnlTilt.add(btnTilt);
	pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
	pnlTilt.add(btn3dmod);
	pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));

	rootPanel.add(pnlTilt);
	rootPanel.add(Box.createVerticalGlue());
	rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
	rootPanel.add(panelExitButtons);
	rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }
//MARK Bug236 new interface: void setNewstParams(ConstNewstParam newstParam)
  public void setNewstParams(ConstNewstParam newstParam) {
	 chkBoxUseLinearInterpolation.setSelected(newstParam.isUseLinearInterpolation());
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
    if (tiltParam.hasShift()) {
      ltfXOffset.setText(tiltParam.getXOffset());
      ltfZOffset.setText(tiltParam.getZOffset());
    }
    if (tiltParam.hasSlice()) {
      ltfSliceStart.setText(tiltParam.getIdxSliceStart());
      ltfSliceStop.setText(tiltParam.getIdxSliceStop());
      ltfSliceStep.setText(tiltParam.getIdxSliceIncr());
    }
    if (tiltParam.hasXAxisTilt()) {
      ltfXAxisTilt.setText(tiltParam.getXAxisTilt());
    }
    if (tiltParam.hasAngleOffsets()) {
      ltfTiltAngleOffset.setText(tiltParam.getTiltAngleOffset());
      ltfTiltAxisOffset.setText(tiltParam.getTiltAxisOffset());
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
    chkBoxUseLocalAlignment.setSelected(tiltParam.getUseLocalAlignFile());
  }

//MARK Bug236 new interface: void getNewstParams(NewstParam newstParam
  public void getNewstParams(NewstParam newstParam) {
    newstParam.setUseLinearInterpolation(chkBoxUseLinearInterpolation.isSelected());
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  public void getTiltParams(TiltParam tiltParam) throws NumberFormatException {
    String badParameter = "";
    try {

      if (ltfTomoWidth.getText().matches("\\S+")) {
        badParameter = ltfTomoWidth.getLabel();
        tiltParam.setWidth(Integer.parseInt(ltfTomoWidth.getText()));
      }
      else {
        tiltParam.useWidth(false);
      }

      //  TODO: Error checking to be sure that all parameters are supplied
      if (ltfXOffset.getText().matches("\\S+")
        || ltfZOffset.getText().matches("\\S+")) {
        badParameter = ltfXOffset.getLabel();
        tiltParam.setXOffset(Double.parseDouble(ltfXOffset.getText()));
        badParameter = ltfZOffset.getLabel();
        tiltParam.setZOffset(Double.parseDouble(ltfZOffset.getText()));
      }
      else {
        tiltParam.useShift(false);
      }

      //  TODO: Error checking to be sure that all parameters are supplied
      if (ltfSliceStart.getText().matches("\\S+")
        || ltfSliceStop.getText().matches("\\S+")
        || ltfSliceStep.getText().matches("\\S+")) {
        badParameter = ltfSliceStart.getLabel();
        tiltParam.setIdxSliceStart(Integer.parseInt(ltfSliceStart.getText()));
        badParameter = ltfSliceStop.getLabel();
        tiltParam.setIdxSliceStop(Integer.parseInt(ltfSliceStop.getText()));
        badParameter = ltfSliceStep.getLabel();
        tiltParam.setIdxSliceIncr(Integer.parseInt(ltfSliceStep.getText()));
      }
      else {
        tiltParam.useSlice(false);
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

      //    TODO: Error checking to be sure that all parameters are supplied
      if (ltfTiltAngleOffset.getText().matches("\\S+")
        || ltfTiltAxisOffset.getText().matches("\\S+")) {
        badParameter = ltfTiltAngleOffset.getLabel();
        tiltParam.setTiltAngleOffset(
          Double.parseDouble(ltfTiltAngleOffset.getText()));
        badParameter = ltfTiltAxisOffset.getLabel();
        tiltParam.setTiltAxisOffset(
          Double.parseDouble(ltfTiltAxisOffset.getText()));
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

      if (chkBoxUseLocalAlignment.isSelected()) {
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
			pnlNewstParams.add(chkBoxUseLinearInterpolation);
		}
	}

  private void layoutTiltPanel() {
  	pnlTiltParams.removeAll();
    if (isAdvanced) {

      pnlTiltParams.add(ltfTomoThickness.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfTomoWidth.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStart.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStop.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStep.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfXOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfZOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfXAxisTilt.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfTiltAngleOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfTiltAxisOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfRadialMax.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfRadialFallOff.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfDensityOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfDensityScale.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfLogOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(chkBoxUseLocalAlignment);
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
      pnlTiltParams.add(chkBoxUseLocalAlignment);
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));
    }
  }

  /**
   * Right mouse button context menu
   */

  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "newst", "tilt", "3dmod" };
    String[] manPage = { "newst.html", "tilt.html", "3dmod.html" };

    String[] logFileLabel;
    String[] logFile;
    logFileLabel = new String[2];
    logFileLabel[0] = "newst";
    logFileLabel[1] = "tilt";
    logFile = new String[2];
    logFile[0] = "newst" + axisID.getExtension() + ".log";
    logFile[1] = "tilt" + axisID.getExtension() + ".log";

    ContextPopup contextPopup =
      new ContextPopup(
        rootPanel,
        mouseEvent,
        "Final Runs",
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
    else if (command.equals(btnCommit.getActionCommand())) {
      applicationManager.commitTestVolume(axisID);
    }
    else if (command.equals(btnTilt.getActionCommand())) {
      applicationManager.tilt(axisID);
    }
    else if (command.equals(btn3dmod.getActionCommand())) {
      applicationManager.imodFullVolume(axisID);
    }
  }

  private class TomogramGenerationActionListener implements ActionListener {

    TomogramGenerationDialog adaptee;

    TomogramGenerationActionListener(TomogramGenerationDialog adaptee) {
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
      "Generate the complete aligned stack for input into the tilt process."
        + "  This runs the newst.com script.";
    btnNewst.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This parameter specifies the thickness in pixels (along the z-axis) of"
        + " the reconstructed volume.";
    ltfTomoThickness.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "These parameters allow a limited part of the map to be reconstructed and"
        + "are useful for test purposes. A slab from column specified by First "
        + "slice to the column Last slice (that is, along the medium axis, "
        + "perpendicular to the tilt axis) of the volume is reconstructed, at "
        + "intervals of Slice step.  Slices are numbered from 0.";
    ltfSliceStart.setToolTipText(tooltipFormatter.setText(text).format());
    ltfSliceStop.setToolTipText(tooltipFormatter.setText(text).format());
    ltfSliceStep.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This entry specifies the width of the output image; the default is the "
        + "width of the input image.";
    ltfTomoWidth.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This parameter allows one to shift the reconstructed slice in X ."
        + "  If XOFFSET is positive, the slice will be shifted to the right, "
        + "and the output will contain the left part of the whole potentially "
        + "reconstructable area";
    ltfXOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This parameter allows one to shift the reconstructed slice Z before "
        + "it is output.  If ZOFFSET is positive, the slice is shifted upward.";
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
      "This parameters allows an offset (in degrees) to be applied to all "
        + "tilt angles.  A positive value rotates reconstructed sections "
        + "anticlockwise.  If you specify this parameter you must also specify "
        + "the tilt axis offset.";
    ltfTiltAngleOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This parameter indicates that the tilt axis is offset in the projection "
        + "images, cutting the X-axis at  NX/2. + DELXX instead ofNX/2.  If you"
        + " specify this parameter you must also specify the tilt angle offset.";

    ltfTiltAxisOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "These parameters control the radial weighting function.  The radial "
        + "weighting function is linear away from the origin for a distance of "
        + "Radial max in reciprocal space followed by a gaussian fall-off of "
        + "s.d.  Radial falloff.  The distances may be specified either as "
        + "pixels (values greater than 1) or as frequencies (cycles/pixel) in "
        + "Fourier space  (values < 1).  Both parameters must be specified if "
        + "either is specified.";
    ltfRadialFallOff.setToolTipText(tooltipFormatter.setText(text).format());
    ltfRadialMax.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This line allows a linear change of density in the reconstructed image "
        + "according to the formula ARRAY(I)=(ARRAY(I)+FLEVL)*SCALE.  After "
        + "the reconstruction is complete, the program will output the scale "
        + "values that would make the data range from 10 to 245.  Both "
        + "parameters must be specified if either is specified.";
    ltfDensityScale.setToolTipText(tooltipFormatter.setText(text).format());
    ltfDensityOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "This parameter allows one to generate a reconstruction using the "
        + "logarithm of the densities in the input file, with the value "
        + "specified added before taking the logarithm.  If no parameter is "
        + "specified the logarithm of the input data is not taken.";
    ltfLogOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Select this checkbox to use local alignments.  You must have "
        + "created the local alignments in the Fine Alignment step";
    chkBoxUseLocalAlignment.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Compute the tomogram from the full aligned stack.  This runs"
        + "the tilt.com script.";
    btnTilt.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the reconstructed volume in 3dmod.";
    btn3dmod.setToolTipText(tooltipFormatter.setText(text).format());
    
    text = "Use linear interpolation rather than the default, cubic interpolation, " +
              "when transforming images.  Linear interpolation is more " +
              "suitable when images are very noisy, but cubic interpolation " +
              "will preserve fine detail better when noise is not an issue. " +
              "Images are transformed when the -xform, -expand, or -rotate " +
              "option is entered.";
    chkBoxUseLinearInterpolation.setToolTipText(tooltipFormatter.setText(text).format());
  }
}
