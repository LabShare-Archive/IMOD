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
 * <p>
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

  JPanel panelTilt = new JPanel();
  BeveledBorder border = new BeveledBorder("Tomogram Generation");

  JToggleButton buttonNewst =
    new JToggleButton("<html><b>Create full<br>aligned stack</b>");

  LabeledTextField ltfTomoThickness =
    new LabeledTextField("Tomogram thickness: ");

  LabeledTextField ltfXAxisTilt = new LabeledTextField("X Axis Tilt: ");

  JCheckBox chkBoxUseLocalAlignment = new JCheckBox("Use local alignments");

  JToggleButton buttonTilt =
    new JToggleButton("<html><b>Generate<br>tomogram</b>");
  JToggleButton buttonImod =
    new JToggleButton("<html><b>View tomogram<br>in imod</b>");

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

    panelTilt.setBorder(border.getBorder());
    panelTilt.setLayout(new BoxLayout(panelTilt, BoxLayout.Y_AXIS));

    panelTilt.add(buttonNewst);
    panelTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    panelTilt.add(ltfTomoThickness.getContainer());
    panelTilt.add(ltfXAxisTilt.getContainer());
    panelTilt.add(chkBoxUseLocalAlignment);
    panelTilt.add(buttonTilt);
    panelTilt.add(Box.createRigidArea(FixedDim.x0_y10));
    panelTilt.add(buttonImod);
    panelTilt.add(Box.createRigidArea(FixedDim.x0_y10));

    rootPanel.add(panelTilt);
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
    ltfTomoThickness.setText(tiltParam.getThickness());
    ltfXAxisTilt.setText(tiltParam.getXAxisTilt());
    chkBoxUseLocalAlignment.setSelected(tiltParam.getUseLocalAlignFile());
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  public void getTiltParams(TiltParam tiltParam) throws NumberFormatException {

    try {
      tiltParam.setThickness(Integer.parseInt(ltfTomoThickness.getText()));
      tiltParam.setXAxisTilt(Double.parseDouble(ltfXAxisTilt.getText()));

      if (chkBoxUseLocalAlignment.isSelected()) {
        tiltParam.setLocalAlignFile(
          applicationManager.getFilesetName()
            + axisID.getExtension()
            + "local.xf");
      }
      else {
        tiltParam.setLocalAlignFile("");
      }
    }
    catch (NumberFormatException except) {
      String message =
        "Axis: " + axisID.getExtension() + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    applicationManager.packMainWindow();
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "newst", "tilt", "imod" };
    String[] manPage = { "newst.html", "tilt.html", "imod.html" };

    String[] logFileLabel;
    String[] logFile;
    logFileLabel = new String[2];
    logFileLabel[0] = "newst" + axisID.getExtension();
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
