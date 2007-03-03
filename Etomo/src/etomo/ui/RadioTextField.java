package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JTextField;

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
* <p> $Log$ </p>
*/
final class RadioTextField implements RadioButtonParent {
  public static  final String  rcsid =  "$Id$";
  
  private final JTextField textField = new JTextField();
  private final JPanel rootPanel = new JPanel();
  private final RadioButton radioButton;

  /**
   * Do not use constructor directly.  Internal listener added in getInstance.
   * @param label
   * @param group
   */
  private RadioTextField(final String label, final ButtonGroup group) {
    radioButton = new RadioButton(label);
    radioButton.setModel(new RadioButton.RadioButtonModel(this));
    textField.setName(radioButton.getName());
    group.add(radioButton);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.X_AXIS));
    rootPanel.add(radioButton);
    rootPanel.add(textField);
    setTextFieldEnabled();
  }

  /**
   * Constructs local instance, adds listener, and returns.
   * @param label
   * @param group
   * @return
   */
  static RadioTextField getInstance(final String label,
      final ButtonGroup group) {
    RadioTextField radioTextField = new RadioTextField(label, group);
    radioTextField.addListeners();
    return radioTextField;
  }

  Container getContainer() {
    return rootPanel;
  }

  void setText(final String text) {
    textField.setText(text);
  }

  String getLabel() {
    return radioButton.getText();
  }

  String getText() {
    return textField.getText();
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
    if (!radioButton.isEnabled()&&textField.isEnabled()) {
      return "Fields should enable and disable together";
    }
    if (!radioButton.isSelected()&&textField.isEnabled()) {
      return "Text field should be disabled when radio button is not selected";
    }
    if (radioButton.isEnabled()&&radioButton.isSelected()&&!textField.isEnabled()) {
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
