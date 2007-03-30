package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.type.ConstEtomoNumber;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2007/03/07 21:13:18  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.  Added radioValue - a way to assign an
 * <p> integer value to each radio button in a group.
 * <p>
 * <p> Revision 1.1  2007/03/03 01:05:57  sueh
 * <p> bug# 973 Class combining a RadioButton and JTextField.  Turns off the
 * <p> JTextField when the radio button is not selected.
 * <p> </p>
 */
final class RadioTextField implements RadioButtonParent {
  public static final String rcsid = "$Id$";

  private final JPanel rootPanel = new JPanel();
  private final RadioButton radioButton;
  private final TextField textField;

  /**
   * Constructs local instance, adds listener, and returns.
   * @param label
   * @param group
   * @return
   */
  static RadioTextField getInstance(final String label, final ButtonGroup group) {
    RadioTextField radioTextField = new RadioTextField(label, group);
    radioTextField.addListeners();
    return radioTextField;
  }

  /**
   * Constructs local instance, adds listener, and returns.
   * @param label
   * @param group
   * @param radioValue
   * @return
   */
  static RadioTextField getInstance(final String label,
      final ButtonGroup group, int radioValue) {
    RadioTextField radioTextField = new RadioTextField(label, group, radioValue);
    radioTextField.addListeners();
    return radioTextField;
  }

  private RadioTextField(final String label, final ButtonGroup group) {
    radioButton = new RadioButton(label);
    textField=new TextField(label);
    init(group);
  }

  /**
   * Do not use constructor directly.  Internal listener added in getInstance.
   * @param label
   * @param group
   */
  private RadioTextField(final String label, final ButtonGroup group,
      int radioValue) {
    radioButton = new RadioButton(label, radioValue);
    textField=new TextField(label);
    init(group);
  }

  private void init(final ButtonGroup group) {
    radioButton.setModel(new RadioButton.RadioButtonModel(this));
    group.add(radioButton.getAbstractButton());
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.X_AXIS));
    rootPanel.add(radioButton.getComponent());
    rootPanel.add(textField.getComponent());
    setTextFieldEnabled();
  }

  void setTextPreferredWidth(final double minWidth) {
    Dimension prefSize = textField.getPreferredSize();
    prefSize.setSize(minWidth, prefSize.getHeight());
    textField.setSize(prefSize);
  }

  Container getContainer() {
    return rootPanel;
  }

  void setText(final String text) {
    textField.setText(text);
  }

  void setText(final ConstEtomoNumber text) {
    textField.setText(text.toString());
  }

  String getLabel() {
    return radioButton.getText();
  }

  String getText() {
    String text = textField.getText();
    if (text == null || text.matches("\\s*")) {
      return "";
    }
    return text;
  }

  ConstEtomoNumber getRadioValue() {
    return radioButton.getRadioValue();
  }

  boolean isSelected() {
    return radioButton.isSelected();
  }

  void setEnabled(final boolean enable) {
    radioButton.setEnabled(enable);
    setTextFieldEnabled();
  }

  public void msgSelected() {
    setTextFieldEnabled();
  }

  void setSelected(boolean selected) {
    radioButton.setSelected(selected);
  }

  void setToolTipText(final String text) {
    radioButton.setToolTipText(text);
    textField.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }
  
  void setRadioButtonToolTipText(final String text) {
    radioButton.setToolTipText(text);
  }
  
  void setTextFieldToolTipText(final String text) {
    textField.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  void addActionListener(ActionListener actionListener) {
    radioButton.addActionListener(actionListener);
  }

  String getActionCommand() {
    return radioButton.getActionCommand();
  }

  /**
   * @return null if instance is in a valid state
   */
  String validate() {
    if (!radioButton.getName().equals(textField.getName())) {
      return "Fields should have the same name";
    }
    if (!radioButton.isEnabled() && textField.isEnabled()) {
      return "Fields should enable and disable together";
    }
    if (!radioButton.isSelected() && textField.isEnabled()) {
      return "Text field should be disabled when radio button is not selected";
    }
    if (radioButton.isEnabled() && radioButton.isSelected()
        && !textField.isEnabled()) {
      return "text field should be enabled when radio button is selected";
    }
    return null;
  }

  private void setTextFieldEnabled() {
    textField.setEnabled(radioButton.isEnabled() && radioButton.isSelected());
  }

  private void addListeners() {
    radioButton.addActionListener(new RTFActionListener(this));
  }

  private void action(final ActionEvent actionEvent) {
    if (actionEvent.getActionCommand().equals(radioButton.getActionCommand())) {
      setTextFieldEnabled();
    }
  }

  private static final class RTFActionListener implements ActionListener {
    private final RadioTextField radioTextField;

    private RTFActionListener(final RadioTextField radioTextField) {
      this.radioTextField = radioTextField;
    }

    public void actionPerformed(final ActionEvent actionEvent) {
      radioTextField.action(actionEvent);
    }
  }
}
