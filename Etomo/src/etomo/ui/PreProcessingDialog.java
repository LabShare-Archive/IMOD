package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.*;
import etomo.comscript.ConstCCDEraserParam;
import etomo.comscript.CCDEraserParam;

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
 * <p> Revision 1.4  2002/11/19 02:40:37  rickg
 * <p> Label spelling correction
 * <p>
 * <p> Revision 1.3  2002/10/17 22:40:02  rickg
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
public class PreProcessingDialog extends ProcessDialog {
  public static final String rcsid =
    "$Id$";

  private JLabel textDM2MRC =
    new JLabel("No digital micrograph files detected:  ");
  private JPanel panelConvertDM2MRC = new JPanel();
  private JCheckBox chkBoxUniqueHeaders =
    new JCheckBox("Digital Micrograph files have unique headers");
  private BeveledBorder borderDM2MRC =
    new BeveledBorder("Digital Micrograph Conversion");

  CCDEraserPanel panelCCDEraser = new CCDEraserPanel();
  private JButton buttonCreateModel =
    new JButton("<html><b>Create replacement model</b>");
  private JButton buttonErase = new JButton("<html><b>Erase pixels</b>");

  public PreProcessingDialog(ApplicationManager appManager, AxisID axisID) {
    super(appManager, axisID);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    //  Build the digital micrograph panel
    panelConvertDM2MRC.setLayout(
      new BoxLayout(panelConvertDM2MRC, BoxLayout.Y_AXIS));
    panelConvertDM2MRC.setBorder(borderDM2MRC.getBorder());
    panelConvertDM2MRC.add(textDM2MRC);
    panelConvertDM2MRC.add(Box.createRigidArea(FixedDim.x20_y0));
    panelConvertDM2MRC.add(chkBoxUniqueHeaders);
    // applicationManager.isDigitalMicrographData();
    disableDM2MRC();

    //  Build the base panel
    buttonExecute.setText("Done");
    panelConvertDM2MRC.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(panelConvertDM2MRC);

    rootPanel.add(panelCCDEraser.getContainer());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    buttonCreateModel.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonCreateModel.setPreferredSize(FixedDim.button2Line);
    buttonCreateModel.setMaximumSize(FixedDim.button2Line);
    rootPanel.add(buttonCreateModel);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    buttonErase.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonErase.setPreferredSize(FixedDim.button2Line);
    buttonErase.setMaximumSize(FixedDim.button2Line);

    rootPanel.add(buttonErase);

    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the button action adapters to their listeners
    buttonCreateModel.addActionListener(new PreProcessingActionListener(this));
    buttonErase.addActionListener(new PreProcessingActionListener(this));

    setToolTipText();

    //  Set the default advanced state for the window, this also executes
    updateAdvanced();
  }

  /**
   * Set the parameters for the specified CCD eraser panel
   */
  public void setCCDEraserParams(ConstCCDEraserParam ccdEraserParams) {
    panelCCDEraser.setParameters(ccdEraserParams);
  }

  /**
   * Get the input parameters from the dialog box.
   */
  public void getCCDEraserParams(CCDEraserParam ccdEraserParams) {
    panelCCDEraser.getParameters(ccdEraserParams);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    panelCCDEraser.setAdvanced(isAdvanced);
    applicationManager.packMainWindow();
  }

  private void disableDM2MRC() {
    chkBoxUniqueHeaders.setEnabled(false);
    chkBoxUniqueHeaders.setSelected(false);
  }

  //  Button action method
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(buttonCreateModel.getActionCommand())) {
      applicationManager.imodErase(axisID);
    }
    else if (command.equals(buttonErase.getActionCommand())) {
      applicationManager.eraser(axisID);
    }
  }

  void buttonErasePixelsB(ActionEvent event) {
    applicationManager.eraser(AxisID.SECOND);
  }

  //
  //  Action function overides for buttons
  //
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.donePreProcDialog(axisID);
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.donePreProcDialog(axisID);
  }

  public void buttonExecuteAction(ActionEvent event) {
    exitState = DialogExitState.EXECUTE;
    applicationManager.donePreProcDialog(axisID);
  }

  private void setToolTipText() {
    String line1, line2, line3, line4, line5, line6, line7;
    line1 = "<html>This button will open 3dmod with the current CCD erase<br>";
    line2 = "model.  This will allow you to specify additional pixels and<br>";
    line3 = "regions to be replaced with interpolated values.";
    buttonCreateModel.setToolTipText(line1 + line2 + line3);

    line1 = "<html>This button will execute the specified erase command<br>";
    line2 = "applying the erase model created in the previous step.";
    buttonErase.setToolTipText(line1 + line2);
  }
}

//  Action listener
class PreProcessingActionListener implements ActionListener {

  PreProcessingDialog adaptee;

  PreProcessingActionListener(PreProcessingDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonAction(event);
  }
}
