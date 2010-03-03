package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.type.ConstEtomoNumber;
import etomo.type.EnumeratedType;

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
 * <p> Revision 1.9  2009/11/20 17:31:49  sueh
 * <p> bug# 1282 Changed validate to handle the name prefixes.
 * <p>
 * <p> Revision 1.8  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.7  2009/02/19 01:45:50  sueh
 * <p> bug# 1178 In setToolTipText stop formatting tooltip twice.
 * <p>
 * <p> Revision 1.6  2009/02/19 01:44:19  sueh
 * <p> bug# 1178 In setToolTipText stop formatting tooltip twice.
 * <p>
 * <p> Revision 1.5  2008/11/20 01:46:57  sueh
 * <p> bug# 1147 Commented out code that was not in use.
 * <p>
 * <p> Revision 1.4  2007/04/13 20:38:46  sueh
 * <p> bug# 964Added EnumeratedType, which is the interface for enumeration types.
 * <p>
 * <p> Revision 1.3  2007/03/30 23:52:28  sueh
 * <p> bug# 964 Switched from JTextField to etomo.ui.TextField, which names itself.
 * <p>
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
final class RadioTextField implements RadioButtonInterface {
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
  /*static RadioTextField getInstance(final String label,
   final ButtonGroup group, EnumeratedType enumeratedType) {
   RadioTextField radioTextField = new RadioTextField(label, group,
   enumeratedType);
   radioTextField.addListeners();
   return radioTextField;
   }*/

  private RadioTextField(final String label, final ButtonGroup group) {
    radioButton = new RadioButton(label);
    textField = new TextField(label);
    init(group);
  }

  /**
   * Do not use constructor directly.  Internal listener added in getInstance.
   * @param label
   * @param group
   */
  /* private RadioTextField(final String label, final ButtonGroup group,
   EnumeratedType enumeratedType) {
   radioButton = new RadioButton(label, enumeratedType);
   textField = new TextField(label);
   init(group);
   }*/

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
    textField.setTextPreferredSize(prefSize);
  }

  Container getContainer() {
    return rootPanel;
  }

  void setText(final float value) {
    textField.setText(String.valueOf(value));
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

  public EnumeratedType getEnumeratedType() {
    return radioButton.getEnumeratedType();
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
    textField.setToolTipText(text);
  }

  void setRadioButtonToolTipText(final String text) {
    radioButton.setToolTipText(text);
  }

  void setTextFieldToolTipText(final String text) {
    textField.setToolTipText(text);
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
    if (!radioButton.getName().endsWith(textField.getName().substring(2))) {
      return "Fields should have the same name, except for the prefix";
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
