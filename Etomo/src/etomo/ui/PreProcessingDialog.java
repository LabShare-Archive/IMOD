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

  ApplicationManager applicationManager;

  JPanel contentPane;

  JLabel textDM2MRC = new JLabel("No digital micrgraph files detected:  ");
  JPanel panelConvertDM2MRC = new JPanel();
  JCheckBox chkBoxUniqueHeaders =
    new JCheckBox("Digital Micrograph files have unique headers");
  BeveledBorder borderDM2MRC =
    new BeveledBorder("Digital Micrograph Conversion");

  JPanel panelCCDEraser = new JPanel();
  BeveledBorder borderCCDEraser = new BeveledBorder("CCD Eraser");
  CCDEraserPanel panelCCDEraserA = new CCDEraserPanel("Axis: A");
  CCDEraserPanel panelCCDEraserB = new CCDEraserPanel("Axis: B");

  public PreProcessingDialog(ApplicationManager appManager) {
    applicationManager = appManager;
    contentPane = (JPanel) this.getContentPane();
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    setTitle("eTomo Pre-processing: " + applicationManager.getFilesetName());

    //
    //  Build the digital micrograph panel
    //
    panelConvertDM2MRC.setLayout(new FlowLayout());
    panelConvertDM2MRC.setBorder(borderDM2MRC.getBorder());
    panelConvertDM2MRC.add(textDM2MRC);
    panelConvertDM2MRC.add(Box.createRigidArea(FixedDim.x20_y0));
    panelConvertDM2MRC.add(chkBoxUniqueHeaders);
    // applicationManager.isDigitalMicrographData();
    disableDM2MRC();

    //
    //  Build the CCD eraser panel
    //
    panelCCDEraser.setLayout(new BoxLayout(panelCCDEraser, BoxLayout.X_AXIS));
    panelCCDEraser.setBorder(borderCCDEraser.getBorder());
    panelCCDEraser.add(Box.createRigidArea(FixedDim.x5_y0));
    panelCCDEraser.add(panelCCDEraserA.getPanel());
    panelCCDEraser.add(Box.createRigidArea(FixedDim.x10_y0));
    panelCCDEraser.add(panelCCDEraserB.getPanel());
    panelCCDEraser.add(Box.createRigidArea(FixedDim.x5_y0));

    //
    //  Build the base panel
    //
    buttonExecute.setText("Done");
    contentPane.add(panelConvertDM2MRC);
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(panelCCDEraser);
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    //
    //  Bind the button action adapters to their listeners
    //
    panelCCDEraserA.setButtonCreateModelActionListener(
      new PreProcDialogCreateModelAActionAdapter(this));

    panelCCDEraserB.setButtonCreateModelActionListener(
      new PreProcDialogCreateModelBActionAdapter(this));

    panelCCDEraserA.setButtonErasePixelsActionListener(
      new PreProcDialogErasePixelsAActionAdapter(this));

    panelCCDEraserB.setButtonErasePixelsActionListener(
      new PreProcDialogErasePixelsBActionAdapter(this));

    //
    //  Set the default advanced state for the window
    //  FIXME: this needs to be defined by the options and
    //  the last state it was opened in
    //
    panelCCDEraserA.setAdvanced(false);
    panelCCDEraserB.setAdvanced(false);

    //
    // Calcute the necessary window size
    //
    pack();
  }

  /**
   * Set the parameters for the specified CCD eraser panel
   */
  public void setCCDEraserParams(
    ConstCCDEraserParam ccdEraserParams,
    AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      panelCCDEraserB.setParameters(ccdEraserParams);
    }
    else {
      panelCCDEraserA.setParameters(ccdEraserParams);
    }
  }

  /**
   * Get the input parameters from the dialog box.
   */
  public void getCCDEraserParams(
    CCDEraserParam ccdEraserParams,
    AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      panelCCDEraserB.getParameters(ccdEraserParams);
    }
    else {
      panelCCDEraserA.getParameters(ccdEraserParams);
    }
  }
  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    if (isAdvanced) {
      panelCCDEraserA.setAdvanced(true);
      panelCCDEraserB.setAdvanced(true);
    }
    else {
      panelCCDEraserA.setAdvanced(false);
      panelCCDEraserB.setAdvanced(false);
    }
    pack();
  }

  public void setEnabledB(boolean state) {
    panelCCDEraserB.setVisible(state);
  }

  private void disableDM2MRC() {
    chkBoxUniqueHeaders.setEnabled(false);
    chkBoxUniqueHeaders.setSelected(false);
  }

  //
  //  Button action methods
  //
  void buttonCreateModelA(ActionEvent event) {
    if (applicationManager.isDualAxis()) {
      applicationManager.imodErase(AxisID.FIRST);
    }
    else {
      applicationManager.imodErase(AxisID.ONLY);
    }
  }

  void buttonCreateModelB(ActionEvent event) {
    applicationManager.imodErase(AxisID.SECOND);
  }

  void buttonErasePixelsA(ActionEvent event) {
    if (applicationManager.isDualAxis()) {
      applicationManager.eraser(AxisID.FIRST);
    }
    else {
      applicationManager.eraser(AxisID.ONLY);
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
    applicationManager.donePreProcDialog();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.donePreProcDialog();
  }

  public void buttonExecuteAction(ActionEvent event) {
    exitState = DialogExitState.EXECUTE;
    applicationManager.donePreProcDialog();
  }
}

//
//  Action adapters
//
class PreProcDialogCreateModelAActionAdapter implements ActionListener {

  PreProcessingDialog adaptee;

  PreProcDialogCreateModelAActionAdapter(PreProcessingDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCreateModelA(event);
  }
}

class PreProcDialogCreateModelBActionAdapter implements ActionListener {

  PreProcessingDialog adaptee;

  PreProcDialogCreateModelBActionAdapter(PreProcessingDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonCreateModelB(event);
  }
}

class PreProcDialogErasePixelsAActionAdapter implements ActionListener {

  PreProcessingDialog adaptee;

  PreProcDialogErasePixelsAActionAdapter(PreProcessingDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonErasePixelsA(event);
  }
}

class PreProcDialogErasePixelsBActionAdapter implements ActionListener {

  PreProcessingDialog adaptee;

  PreProcDialogErasePixelsBActionAdapter(PreProcessingDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonErasePixelsB(event);
  }
}
