package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.comscript.ConstTilt;
import etomo.comscript.TiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.TiltalignParam;

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
public class TomogramPositioningDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel panelPosition = new JPanel();
  private BeveledBorder border = new BeveledBorder("Tomogram Positioning");

  private LabeledTextField ltfSampleTomoThickness =
    new LabeledTextField("Sample tomogram thickness: ");
  private JToggleButton buttonSample =
    new JToggleButton("<html><b>Create sample tomograms</b>");

  private JToggleButton buttonCreateBoundary =
    new JToggleButton("<html><b>Create boundary models</b>");

  private JToggleButton buttonTomopitch =
    new JToggleButton("<html><b>Execute tomopitch</b>");

  private LabeledTextField ltfTiltAngleOffset =
    new LabeledTextField("Tilt angle offset: ");
  private LabeledTextField ltfTiltAxisZShift =
    new LabeledTextField("Tilt axis z shift: ");
  private LabeledTextField ltfTiltAxisXShift =
    new LabeledTextField("Tilt axis x shift: ");
  private JToggleButton buttonAlign =
    new JToggleButton("<html><b>Create final alignment</b>");

  public TomogramPositioningDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    buttonExecute.setText("Done");

    ltfSampleTomoThickness.setTextPreferredSize(new Dimension(50, 20));
    buttonSample.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonSample.setPreferredSize(FixedDim.button2Line);
    buttonSample.setMaximumSize(FixedDim.button2Line);

    buttonCreateBoundary.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonCreateBoundary.setPreferredSize(FixedDim.button2Line);
    buttonCreateBoundary.setMaximumSize(FixedDim.button2Line);

    buttonTomopitch.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonTomopitch.setPreferredSize(FixedDim.button2Line);
    buttonTomopitch.setMaximumSize(FixedDim.button2Line);

    buttonAlign.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonAlign.setPreferredSize(FixedDim.button2Line);
    buttonAlign.setMaximumSize(FixedDim.button2Line);

    // Bind the buttons to the action listener
    TomogramPositioningActionListener tomogramPositioningActionListener =
      new TomogramPositioningActionListener(this);
    buttonSample.addActionListener(tomogramPositioningActionListener);
    buttonCreateBoundary.addActionListener(tomogramPositioningActionListener);
    buttonTomopitch.addActionListener(tomogramPositioningActionListener);
    buttonAlign.addActionListener(tomogramPositioningActionListener);

    //  Create the primary panels
    panelPosition.setBorder(border.getBorder());
    panelPosition.setLayout(new BoxLayout(panelPosition, BoxLayout.Y_AXIS));

    panelPosition.add(ltfSampleTomoThickness.getContainer());
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(buttonSample);
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(buttonCreateBoundary);
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(buttonTomopitch);
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(ltfTiltAngleOffset.getContainer());
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(ltfTiltAxisZShift.getContainer());
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(ltfTiltAxisXShift.getContainer());
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(buttonAlign);

    //  Create dialog content pane
    rootPanel.add(panelPosition);
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

  //  Set the tilt.com parameters that are editable in this dialog
  public void setTiltParams(ConstTilt tiltParam) {
    ltfSampleTomoThickness.setText(tiltParam.getThickness());
  }

  // Get the tilt.com parameters that 
  public void getTiltParams(TiltParam tiltParam) throws NumberFormatException {
    try {
      tiltParam.setThickness(
        Integer.parseInt(ltfSampleTomoThickness.getText()));
    }
    catch (NumberFormatException except) {
      String message = "Axis " + axisID.getExtension() + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  public void setAlignParams(ConstTiltalignParam tiltalignParam) {
    ltfTiltAngleOffset.setText(tiltalignParam.getTiltAngleOffset());
    ltfTiltAxisZShift.setText(tiltalignParam.getTiltAxisZShift());
    ltfTiltAxisXShift.setText(tiltalignParam.getTiltAxisXShift());
  }

  public void getAlignParams(TiltalignParam tiltalignParam)
    throws NumberFormatException {
    try {
      tiltalignParam.setTiltAngleOffset(ltfTiltAngleOffset.getText());
      tiltalignParam.setTiltAxisZShift(ltfTiltAxisZShift.getText());
      tiltalignParam.setTiltAxisXShift(ltfTiltAxisXShift.getText());
    }
    catch (NumberFormatException except) {
      throw new NumberFormatException(except.getMessage());
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "newst", "imod", "tomopitch", "tilt" };
    String[] manPage =
      { "newst.html", "imod.html", "tomopitch.html", "tilt.html" };

    String[] logFileLabel;
    String[] logFile;
    logFileLabel = new String[2];
    logFileLabel[0] = "sample" + axisID.getExtension();
    logFileLabel[1] = "tomopitch" + axisID.getExtension();
    logFile = new String[2];
    logFile[0] = "sample" + axisID.getExtension() + ".log";
    logFile[1] = "tomopitch" + axisID.getExtension() + ".log";

    ContextPopup contextPopup =
      new ContextPopup(
        rootPanel,
        mouseEvent,
        "GENERATING THE TOMOGRAM",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  //  Button action handler methods

  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(buttonSample.getActionCommand())) {
      applicationManager.createSample(axisID);
    }

    else if (command.equals(buttonCreateBoundary.getActionCommand())) {
      applicationManager.imodSample(axisID);
    }

    else if (command.equals(buttonTomopitch.getActionCommand())) {
      applicationManager.tomopitch(axisID);
    }

    else if (command.equals(buttonAlign.getActionCommand())) {
      applicationManager.finalAlign(axisID);
    }

  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneTomogramPositioningDialog(axisID);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneTomogramPositioningDialog(axisID);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneTomogramPositioningDialog(axisID);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  void updateAdvanced() {
    ltfTiltAxisXShift.setVisible(isAdvanced);
    applicationManager.packMainWindow();
  }
}

//
//  Action listener adapters
//
class TomogramPositioningActionListener implements ActionListener {

  TomogramPositioningDialog adaptee;

  TomogramPositioningActionListener(TomogramPositioningDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonAction(event);
  }
}
