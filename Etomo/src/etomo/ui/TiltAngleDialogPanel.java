package etomo.ui;

import java.awt.*;
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
 * <p> $Log$ </p>
 */
public class TiltAngleDialogPanel {
  public static final String rcsid = "$Id$";

  private JPanel panelTiltAngleSource = new JPanel();

  private JRadioButton rbTiltAngleExtract =
    new JRadioButton("Extract tilt angles from data");

  private JPanel panelTiltAngleSpecify = new JPanel();
  private JRadioButton rbTiltAngleSpecify =
    new JRadioButton("Specify range and step (degrees)");
  private JLabel labelAngleMin = new JLabel("Min:");
  private JTextField textFieldAngleMin = new JTextField();
  private JLabel labelAngleMax = new JLabel("Max:");
  private JTextField textFieldAngleMax = new JTextField();
  private JLabel labelAngleStep = new JLabel("Step:");
  private JTextField textFieldAngleStep = new JTextField();

  private JRadioButton rbTiltAngleFile =
    new JRadioButton("Tilt angles in existing rawtilt file");

  private ButtonGroup bgTiltAngleSource = new ButtonGroup();

  TiltAngleDialogPanel() {
    Dimension dimAngleTextPref = new Dimension(60, 20);
    Dimension dimAngleTextMax = new Dimension(70, 20);

    textFieldAngleMin.setPreferredSize(dimAngleTextPref);
    textFieldAngleMin.setMaximumSize(dimAngleTextMax);
    textFieldAngleMax.setPreferredSize(dimAngleTextPref);
    textFieldAngleMax.setMaximumSize(dimAngleTextMax);
    textFieldAngleStep.setPreferredSize(dimAngleTextPref);
    textFieldAngleStep.setMaximumSize(dimAngleTextMax);

    panelTiltAngleSpecify.setAlignmentX(0.0f);
    panelTiltAngleSpecify.setLayout(
      new BoxLayout(panelTiltAngleSpecify, BoxLayout.X_AXIS));
    panelTiltAngleSpecify.add(labelAngleMin);
    panelTiltAngleSpecify.add(Box.createRigidArea(FixedDim.x5_y0));
    panelTiltAngleSpecify.add(textFieldAngleMin);
    panelTiltAngleSpecify.add(Box.createRigidArea(FixedDim.x10_y0));
    panelTiltAngleSpecify.add(labelAngleMax);
    panelTiltAngleSpecify.add(Box.createRigidArea(FixedDim.x5_y0));
    panelTiltAngleSpecify.add(textFieldAngleMax);
    panelTiltAngleSpecify.add(Box.createRigidArea(FixedDim.x10_y0));
    panelTiltAngleSpecify.add(labelAngleStep);
    panelTiltAngleSpecify.add(Box.createRigidArea(FixedDim.x5_y0));
    panelTiltAngleSpecify.add(textFieldAngleStep);
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
  }

  JPanel getPanel() {
    return  panelTiltAngleSource;
  }

  void setFields(TiltAngleSpec tiltAngleSpec) {
    if(tiltAngleSpec.getType() == TiltAngleType.EXTRACT){
      rbTiltAngleExtract.setSelected(true);
    }
    if(tiltAngleSpec.getType() == TiltAngleType.RANGE){
      rbTiltAngleSpecify.setSelected(true);
    }
    if(tiltAngleSpec.getType() == TiltAngleType.FILE){
      rbTiltAngleFile.setSelected(true);
    }

    textFieldAngleMin.setText(String.valueOf(tiltAngleSpec.getRangeMin()));
    textFieldAngleMax.setText(String.valueOf(tiltAngleSpec.getRangeMax()));
    textFieldAngleStep.setText(String.valueOf(tiltAngleSpec.getRangeStep()));
  }

  void getFields(TiltAngleSpec tiltAngleSpec) {
    if(rbTiltAngleExtract.isSelected()) {
      tiltAngleSpec.setType(TiltAngleType.EXTRACT);
    }
    if(rbTiltAngleSpecify.isSelected()) {
      tiltAngleSpec.setType(TiltAngleType.RANGE);
    }
    if(rbTiltAngleFile.isSelected()) {
      tiltAngleSpec.setType(TiltAngleType.FILE);
    }
    tiltAngleSpec.setRangeMin(Double.parseDouble(textFieldAngleMin.getText()));
    tiltAngleSpec.setRangeMax(Double.parseDouble(textFieldAngleMax.getText()));
    tiltAngleSpec.setRangeStep(
      Double.parseDouble(textFieldAngleStep.getText()));
  }

  //
  //  Walk through all of the objects applying the appropriate state
  //
  void setEnabled(boolean enable) {
    rbTiltAngleExtract.setEnabled(enable);
    rbTiltAngleFile.setEnabled(enable);
    rbTiltAngleSpecify.setEnabled(enable);
    textFieldAngleMax.setEnabled(enable);
    labelAngleMax.setEnabled(enable);
    textFieldAngleMin.setEnabled(enable);
    labelAngleMin.setEnabled(enable);
    textFieldAngleStep.setEnabled(enable);
    labelAngleStep.setEnabled(enable);
  }


  void setToolTipText(String text) {
    panelTiltAngleSource.setToolTipText(text);
  }
}
