package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JToggleButton;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.TiltParam;
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

  JPanel pnlTilt = new JPanel();
  BeveledBorder border = new BeveledBorder("Tomogram Generation");

  JToggleButton buttonNewst =
    new JToggleButton("<html><b>Create full<br>aligned stack</b>");

  JPanel pnlTiltParams = new JPanel();
  LabeledTextField ltfTomoWidth = new LabeledTextField("Tomogram width: ");
  LabeledTextField ltfXOffset = new LabeledTextField("X Shift: ");

  LabeledTextField ltfSliceStart = new LabeledTextField("First slice: ");
  LabeledTextField ltfSliceStop = new LabeledTextField("Last slice: ");
  LabeledTextField ltfSliceStep = new LabeledTextField("Slice step: ");

  LabeledTextField ltfTomoThickness =
    new LabeledTextField("Tomogram thickness: ");
  LabeledTextField ltfZOffset = new LabeledTextField("Z Shift: ");

  LabeledTextField ltfXAxisTilt = new LabeledTextField("X axis tilt: ");
  LabeledTextField ltfTiltAngleOffset =
    new LabeledTextField("Tilt angle offset: ");
  LabeledTextField ltfTiltAxisOffset =
    new LabeledTextField("Tilt axis offset: ");

  LabeledTextField ltfRadialMax = new LabeledTextField("Radial max: ");
  LabeledTextField ltfRadialFallOff = new LabeledTextField("Radial falloff: ");
  LabeledTextField ltfLogOffset = new LabeledTextField("Log offset: ");

  JCheckBox chkBoxUseLocalAlignment = new JCheckBox("Use local alignments");

  JToggleButton buttonTilt =
    new JToggleButton("<html><b>Generate<br>tomogram</b>");
  JToggleButton buttonImod =
    new JToggleButton("<html><b>View tomogram<br>in 3dmod</b>");

  public TomogramGenerationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    buttonExecute.setText("Done");

    buttonNewst.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonNewst.setPreferredSize(FixedDim.button2Line);
    buttonNewst.setMaximumSize(FixedDim.button2Line);
    buttonNewst.addActionListener(new TomogramGenerationActionListener(this));

    buttonTilt.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonTilt.setPreferredSize(FixedDim.button2Line);
    buttonTilt.setMaximumSize(FixedDim.button2Line);
    buttonTilt.addActionListener(new TomogramGenerationActionListener(this));

    buttonImod.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonImod.setPreferredSize(FixedDim.button2Line);
    buttonImod.setMaximumSize(FixedDim.button2Line);
    buttonImod.addActionListener(new TomogramGenerationActionListener(this));

    chkBoxUseLocalAlignment.setAlignmentX(Component.CENTER_ALIGNMENT);

    layoutTiltPanel();

    pnlTilt.setBorder(border.getBorder());
    pnlTilt.setLayout(new BoxLayout(pnlTilt, BoxLayout.Y_AXIS));
    pnlTiltParams.setLayout(new BoxLayout(pnlTiltParams, BoxLayout.Y_AXIS));

    pnlTilt.add(buttonNewst);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTilt.add(pnlTiltParams);
    pnlTilt.add(buttonTilt);
    pnlTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTilt.add(buttonImod);
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
    if (tiltParam.hasLogOffset()) {
      ltfLogOffset.setText(tiltParam.getLogShift());
    }
    chkBoxUseLocalAlignment.setSelected(tiltParam.getUseLocalAlignFile());
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
      else{
        tiltParam.useXAxisTilt(false);
      }

      if (ltfTiltAngleOffset.getText().matches("\\S+")
        || ltfTiltAxisOffset.getText().matches("\\S+")) {
        badParameter = ltfTiltAngleOffset.getLabel();
        tiltParam.setTiltAngleOffset(
          Double.parseDouble(ltfTiltAngleOffset.getText()));
        badParameter = ltfTiltAxisOffset.getLabel();
        tiltParam.setTiltAxisOffset(
          Double.parseDouble(ltfTiltAxisOffset.getText()));
      }
      else{
        tiltParam.useAngleOffsets(false);
      }

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
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    layoutTiltPanel();
    applicationManager.packMainWindow();
  }

  private void layoutTiltPanel() {
    pnlTiltParams.removeAll();
    if (isAdvanced) {

      pnlTiltParams.add(ltfTomoWidth.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfXOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStart.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStop.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfSliceStep.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(ltfTomoThickness.getContainer());
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

      pnlTiltParams.add(ltfLogOffset.getContainer());
      pnlTiltParams.add(Box.createRigidArea(FixedDim.x0_y5));

      pnlTiltParams.add(chkBoxUseLocalAlignment);
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

  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(buttonNewst.getActionCommand())) {
      applicationManager.newst(axisID);
    }
    else if (command.equals(buttonTilt.getActionCommand())) {
      applicationManager.tilt(axisID);
    }

    else if (command.equals(buttonImod.getActionCommand())) {
      applicationManager.imodTomogram(axisID);
    }
  }

  //  Action function overides for buttons
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
}

class TomogramGenerationActionListener implements ActionListener {

  TomogramGenerationDialog adaptee;

  TomogramGenerationActionListener(TomogramGenerationDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonAction(event);
  }
}
