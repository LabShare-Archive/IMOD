package etomo.ui;

import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.*;

import etomo.type.TiltAngleType;
import etomo.type.TiltAngleSpec;

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
 * <p> Revision 1.2.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.2  2003/01/06 20:17:40  rickg
 * <p> Changed edit boxed to LabeledTextFields
 * <p> Added an action listener and enable funcsions to
 * <p> enable the specify dialogs only when selected
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TiltAngleDialogPanel {
  public static final String rcsid =
    "$Id$";

  private JPanel panelTiltAngleSource = new JPanel();

  private JRadioButton rbTiltAngleExtract =
    new JRadioButton("Extract tilt angles from data");

  private JPanel panelTiltAngleSpecify = new JPanel();
  private JRadioButton rbTiltAngleSpecify =
    new JRadioButton("Specify range and step (degrees)");
  private LabeledTextField ltfAngleMin = new LabeledTextField("Min:");
  private LabeledTextField ltfAngleMax = new LabeledTextField("Max:");
  private LabeledTextField ltfAngleStep = new LabeledTextField("Step:");

  private JRadioButton rbTiltAngleFile =
    new JRadioButton("Tilt angles in existing rawtilt file");

  private ButtonGroup bgTiltAngleSource = new ButtonGroup();

  TiltAngleDialogPanel() {

    panelTiltAngleSpecify.setAlignmentX(Component.LEFT_ALIGNMENT);
    panelTiltAngleSpecify.setLayout(
      new BoxLayout(panelTiltAngleSpecify, BoxLayout.X_AXIS));
    panelTiltAngleSpecify.add(ltfAngleMin.getContainer());
    panelTiltAngleSpecify.add(Box.createRigidArea(FixedDim.x10_y0));
    panelTiltAngleSpecify.add(ltfAngleMax.getContainer());
    panelTiltAngleSpecify.add(Box.createRigidArea(FixedDim.x10_y0));
    panelTiltAngleSpecify.add(ltfAngleStep.getContainer());
    panelTiltAngleSpecify.add(Box.createHorizontalGlue());

    panelTiltAngleSource.setLayout(
      new BoxLayout(panelTiltAngleSource, BoxLayout.Y_AXIS));
    panelTiltAngleSource.add(rbTiltAngleExtract);
    panelTiltAngleSource.add(rbTiltAngleSpecify);
    panelTiltAngleSource.add(panelTiltAngleSpecify);

    panelTiltAngleSource.add(rbTiltAngleFile);
    panelTiltAngleSource.add(Box.createHorizontalGlue());

    //
    //  Build button group
    //
    bgTiltAngleSource.add(rbTiltAngleExtract);
    bgTiltAngleSource.add(rbTiltAngleSpecify);
    bgTiltAngleSource.add(rbTiltAngleFile);

    TiltAngleDialogListener tiltAlignRadioButtonListener =
      new TiltAngleDialogListener(this);
    rbTiltAngleExtract.addActionListener(tiltAlignRadioButtonListener);
    rbTiltAngleFile.addActionListener(tiltAlignRadioButtonListener);
    rbTiltAngleSpecify.addActionListener(tiltAlignRadioButtonListener);
  }

  JPanel getPanel() {
    return panelTiltAngleSource;
  }

  void setFields(TiltAngleSpec tiltAngleSpec) {
    if (tiltAngleSpec.getType() == TiltAngleType.EXTRACT) {
      rbTiltAngleExtract.setSelected(true);
      enableTiltAngleSpecifyFields(false);
    }
    if (tiltAngleSpec.getType() == TiltAngleType.RANGE) {
      rbTiltAngleSpecify.setSelected(true);
      enableTiltAngleSpecifyFields(true);
    }
    if (tiltAngleSpec.getType() == TiltAngleType.FILE) {
      rbTiltAngleFile.setSelected(true);
      enableTiltAngleSpecifyFields(false);
    }

    ltfAngleMin.setText(String.valueOf(tiltAngleSpec.getRangeMin()));
    ltfAngleMax.setText(String.valueOf(tiltAngleSpec.getRangeMax()));
    ltfAngleStep.setText(String.valueOf(tiltAngleSpec.getRangeStep()));
  }

  void getFields(TiltAngleSpec tiltAngleSpec) {
    if (rbTiltAngleExtract.isSelected()) {
      tiltAngleSpec.setType(TiltAngleType.EXTRACT);
    }
    if (rbTiltAngleSpecify.isSelected()) {
      tiltAngleSpec.setType(TiltAngleType.RANGE);
    }
    if (rbTiltAngleFile.isSelected()) {
      tiltAngleSpec.setType(TiltAngleType.FILE);
    }
    tiltAngleSpec.setRangeMin(Double.parseDouble(ltfAngleMin.getText()));
    tiltAngleSpec.setRangeMax(Double.parseDouble(ltfAngleMax.getText()));
    tiltAngleSpec.setRangeStep(Double.parseDouble(ltfAngleStep.getText()));
  }

  //
  //  Walk through all of the objects applying the appropriate state
  //
  void setEnabled(boolean enable) {
    panelTiltAngleSource.setEnabled(enable);
    panelTiltAngleSpecify.setEnabled(enable);
    rbTiltAngleExtract.setEnabled(enable);
    rbTiltAngleFile.setEnabled(enable);
    rbTiltAngleSpecify.setEnabled(enable);
    enableTiltAngleSpecifyFields(rbTiltAngleSpecify.isSelected() & enable);
  }

  void setToolTipText(String text) {
    panelTiltAngleSource.setToolTipText(text);
    rbTiltAngleExtract.setToolTipText(text);
    rbTiltAngleSpecify.setToolTipText(text);
    rbTiltAngleFile.setToolTipText(text);
    ltfAngleMax.setToolTipText(text);
    ltfAngleMin.setToolTipText(text);
    ltfAngleStep.setToolTipText(text);
  }

  //  Set the state of the text fields depending upon the radio button state
  void setRadioButtonState(ActionEvent event) {
    enableTiltAngleSpecifyFields(
      event.getActionCommand().equals(rbTiltAngleSpecify.getText()));
  }

  void enableTiltAngleSpecifyFields(boolean enable) {
    ltfAngleMax.setEnabled(enable);
    ltfAngleMin.setEnabled(enable);
    ltfAngleStep.setEnabled(enable);
  }
}

class TiltAngleDialogListener implements ActionListener {

  TiltAngleDialogPanel adaptee;

  TiltAngleDialogListener(TiltAngleDialogPanel adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.setRadioButtonState(event);
  }
}
