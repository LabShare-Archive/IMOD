package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.TiltParam;
import etomo.comscript.TiltalignParam;
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
public class TomogramPositioningDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel panelPosition = new JPanel();
  private BeveledBorder border = new BeveledBorder("Tomogram Positioning");

  private LabeledTextField ltfSampleTomoThickness =
    new LabeledTextField("Sample tomogram thickness: ");
  private MultiLineToggleButton btnSample =
    new MultiLineToggleButton("<html><b>Create Sample Tomograms</b>");

  private MultiLineToggleButton btnCreateBoundary =
    new MultiLineToggleButton("<html><b>Create Boundary Models</b>");

  private MultiLineToggleButton btnTomopitch =
    new MultiLineToggleButton("<html><b>Compute Z Shift & Pitch Angles</b>");

  private LabeledTextField ltfTiltAngleOffset =
    new LabeledTextField("Total angle offset: ");
  private LabeledTextField ltfTiltAxisZShift =
    new LabeledTextField("Total Z shift: ");
  private MultiLineToggleButton btnAlign =
    new MultiLineToggleButton("<html><b>Create Final Alignment</b>");

  public TomogramPositioningDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");

    ltfSampleTomoThickness.setTextPreferredSize(new Dimension(50, 20));
    btnSample.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnSample.setPreferredSize(FixedDim.button2Line);
    btnSample.setMaximumSize(FixedDim.button2Line);

    btnCreateBoundary.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnCreateBoundary.setPreferredSize(FixedDim.button2Line);
    btnCreateBoundary.setMaximumSize(FixedDim.button2Line);

    btnTomopitch.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTomopitch.setPreferredSize(FixedDim.button2Line);
    btnTomopitch.setMaximumSize(FixedDim.button2Line);

    btnAlign.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnAlign.setPreferredSize(FixedDim.button2Line);
    btnAlign.setMaximumSize(FixedDim.button2Line);

    // Bind the buttons to the action listener
    TomogramPositioningActionListener tomogramPositioningActionListener =
      new TomogramPositioningActionListener(this);
    btnSample.addActionListener(tomogramPositioningActionListener);
    btnCreateBoundary.addActionListener(tomogramPositioningActionListener);
    btnTomopitch.addActionListener(tomogramPositioningActionListener);
    btnAlign.addActionListener(tomogramPositioningActionListener);

    //  Create the primary panels
    panelPosition.setBorder(border.getBorder());
    panelPosition.setLayout(new BoxLayout(panelPosition, BoxLayout.Y_AXIS));

    panelPosition.add(ltfSampleTomoThickness.getContainer());
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(btnSample);
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(btnCreateBoundary);
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(btnTomopitch);
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(ltfTiltAngleOffset.getContainer());
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(ltfTiltAxisZShift.getContainer());
    panelPosition.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPosition.add(btnAlign);

    //  Create dialog content pane
    rootPanel.add(panelPosition);
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

  //  Set the tilt.com parameters that are editable in this dialog
  public void setTiltParams(ConstTiltParam tiltParam) {
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
  }

  public void getAlignParams(TiltalignParam tiltalignParam)
    throws NumberFormatException {
    try {
      tiltalignParam.setTiltAngleOffset(ltfTiltAngleOffset.getText());
      tiltalignParam.setTiltAxisZShift(ltfTiltAxisZShift.getText());
    }
    catch (NumberFormatException except) {
      throw new NumberFormatException(except.getMessage());
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Tomopitch", "Newst", "3dmod", "Tilt" };
    String[] manPage =
      { "tomopitch.html", "newst.html", "3dmod.html", "tilt.html" };

    String[] logFileLabel = { "Tomopitch", "Sample" };
    String[] logFile = new String[2];
    logFile[0] = "tomopitch" + axisID.getExtension() + ".log";
    logFile[1] = "sample" + axisID.getExtension() + ".log";

    ContextPopup contextPopup =
      new ContextPopup(
        rootPanel,
        mouseEvent,
        "TOMOGRAM POSITIONING",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  //  Button action handler methods

  private void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals(btnSample.getActionCommand())) {
      applicationManager.createSample(axisID);
    }

    else if (command.equals(btnCreateBoundary.getActionCommand())) {
      applicationManager.imodSample(axisID);
    }

    else if (command.equals(btnTomopitch.getActionCommand())) {
      applicationManager.tomopitch(axisID);
    }

    else if (command.equals(btnAlign.getActionCommand())) {
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

  private void updateAdvanced() {
    applicationManager.packMainWindow();
  }

  //
  //	Action listener adapters
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

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    text =
      "Thickness of sample slices.  Make this much larger than expected section"
        + " thickness to see borders of section.";
    ltfSampleTomoThickness.setToolTipText(
      tooltipFormatter.setText(text).format());
    text =
      "Build 3 sample tomograms for finding location and angles of section.";
    btnSample.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Open samples in 3dmod to make a model with lines along top and bottom "
        + "edges of the section in each sample.";
    btnCreateBoundary.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Run tomopitch.  You need to examine the log file to determine the"
        + "Z shift, additional angle offset, and X-axis tilt.";
    btnTomopitch.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Add the additional offset from tomopitch to the amount already "
        + "shown here to get the total offset.";
    ltfTiltAngleOffset.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Add the additional shift from tomopitch to the amount shown here to get "
        + "the total shift.";
    ltfTiltAxisZShift.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Run tiltalign with these final offset parameters.";
    btnAlign.setToolTipText(tooltipFormatter.setText(text).format());
  }
}
