package etomo.ui;

import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ui.TooltipFormatter;

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
 * <p> Revision 3.6  2007/08/08 15:04:25  sueh
 * <p> bug# 834 Using UserConfiguration to initialize fields.
 * <p>
 * <p> Revision 3.5  2007/03/07 21:15:28  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.
 * <p>
 * <p> Revision 3.4  2007/02/09 00:53:54  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.3  2006/01/03 23:58:37  sueh
 * <p> bug# 675 Converted JRadioButton's toRadioButton.
 * <p>
 * <p> Revision 3.2  2004/08/03 18:51:27  sueh
 * <p> bug# 519 removing rangeMax
 * <p>
 * <p> Revision 3.1  2003/11/10 18:51:43  sueh
 * <p> bug332 getErrorMessage(): New function validate panel and
 * <p> returns (rather then displaying) error message.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/10/09 20:27:43  sueh
 * <p> bug264
 * <p> UI Changes
 * <p>
 * <p> Revision 2.1  2003/05/07 23:38:36  rickg
 * <p> Added tooltips to object as well as panel
 * <p>
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
final class TiltAnglePanel {
  public static final String rcsid = "$Id$";

  static final String EXISTING_RAWTILT_FILE = "Tilt angles in existing rawtlt file";

  private final JPanel pnlSource = new JPanel();
  private final RadioButton rbExtract = new RadioButton(
      "Extract tilt angles from data");
  private final JPanel pnlAngle = new JPanel();
  private final RadioButton rbSpecify = new RadioButton(
      "Specify the starting angle and step (degrees)");
  private final LabeledTextField ltfMin = new LabeledTextField(
      "Starting angle:");
  private final LabeledTextField ltfStep = new LabeledTextField("Increment:");
  private final RadioButton rbFile = new RadioButton(EXISTING_RAWTILT_FILE);

  private final TiltAnglePanelExpert expert;

  TiltAnglePanel(final TiltAnglePanelExpert expert) {
    this.expert = expert;
    pnlAngle.setAlignmentX(Component.LEFT_ALIGNMENT);
    pnlAngle.setLayout(new BoxLayout(pnlAngle, BoxLayout.X_AXIS));
    pnlAngle.add(ltfMin.getContainer());
    pnlAngle.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlAngle.add(ltfStep.getContainer());
    pnlAngle.add(Box.createHorizontalGlue());

    pnlSource.setLayout(new BoxLayout(pnlSource, BoxLayout.Y_AXIS));
    pnlSource.add(rbExtract.getComponent());
    pnlSource.add(rbSpecify.getComponent());
    pnlSource.add(pnlAngle);

    pnlSource.add(rbFile.getComponent());
    pnlSource.add(Box.createHorizontalGlue());
    pnlSource.setAlignmentX(Component.CENTER_ALIGNMENT);

    //
    //  Build button group
    //
    ButtonGroup bgSource = new ButtonGroup();
    bgSource.add(rbExtract.getAbstractButton());
    bgSource.add(rbSpecify.getAbstractButton());
    bgSource.add(rbFile.getAbstractButton());

    TiltAngleDialogListener tiltAlignRadioButtonListener = new TiltAngleDialogListener(
        expert);
    rbExtract.addActionListener(tiltAlignRadioButtonListener);
    rbFile.addActionListener(tiltAlignRadioButtonListener);
    rbSpecify.addActionListener(tiltAlignRadioButtonListener);
  }

  Component getComponent() {
    return pnlSource;
  }

  void setFile(final boolean input) {
    rbFile.setSelected(input);
  }

  void setExtract(final boolean input) {
    rbExtract.setSelected(input);
  }

  void setSpecify(final boolean input) {
    rbSpecify.setSelected(input);
  }

  void setMin(final double input) {
    ltfMin.setText(input);
  }

  void setStep(final double input) {
    ltfStep.setText(input);
  }

  void setMinEnabled(final boolean enable) {
    ltfMin.setEnabled(enable);
  }

  void setStepEnabled(final boolean enable) {
    ltfStep.setEnabled(enable);
  }

  boolean isExtractSelected() {
    return rbExtract.isSelected();
  }

  boolean isSpecifySelected() {
    return rbSpecify.isSelected();
  }

  boolean isFileSelected() {
    return rbFile.isSelected();
  }

  String getMin() {
    return ltfMin.getText();
  }

  boolean isMinEmpty() {
    return ltfMin.isEmpty();
  }

  String getStep() {
    return ltfStep.getText();
  }

  boolean isStepEmpty() {
    return ltfStep.isEmpty();
  }

  void setSourceEnabled(final boolean enable) {
    pnlSource.setEnabled(enable);
  }

  void setAngleEnabled(final boolean enable) {
    pnlAngle.setEnabled(enable);
  }

  void setExtractEnabled(final boolean enable) {
    rbExtract.setEnabled(enable);
  }

  void setFileEnabled(final boolean enable) {
    rbFile.setEnabled(enable);
  }

  void setSpecifyEnabled(final boolean enable) {
    rbSpecify.setEnabled(enable);
  }

  void setSourceTooltip(final String tooltip) {
    pnlSource.setToolTipText(TooltipFormatter.INSTANCE.format(tooltip));
  }

  void setExtractTooltip(final String tooltip) {
    rbExtract.setToolTipText(tooltip);
  }

  void setSpecifyTooltip(final String tooltip) {
    rbSpecify.setToolTipText(tooltip);
  }

  void setMinTooltip(final String tooltip) {
    ltfMin.setToolTipText(tooltip);
  }

  void setStepTooltip(final String tooltip) {
    ltfStep.setToolTipText(tooltip);
  }

  void setFileTooltip(final String tooltip) {
    rbFile.setToolTipText(tooltip);
  }

  String getSpecify() {
    return rbSpecify.getText();
  }
}

final class TiltAngleDialogListener implements ActionListener {

  private final TiltAnglePanelExpert adaptee;

  TiltAngleDialogListener(final TiltAnglePanelExpert adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(final ActionEvent event) {
    adaptee.setRadioButtonState(event);
  }
}
