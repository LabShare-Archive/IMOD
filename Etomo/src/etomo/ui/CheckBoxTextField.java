package etomo.ui;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * <p>Description: CheckBoxTextField combines a JCheckBox with it's label and an
 * editable text field.  The included action listener enables/disables the text
 * field to match the state of the check box.</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2003/04/09 23:36:12  rickg
 * <p> Changes to allow for subclassing
 * <p>
 * <p> Revision 1.1  2003/04/09 23:14:48  rickg
 * <p> Moved out of FinalCombinePanel
 * <p> </p>
 */
class CheckBoxTextField extends JPanel {
  private JCheckBox checkBox;
  private JTextField textField;

  /**
   * Default constructor
   *
   */
  public CheckBoxTextField() {
    checkBox = new JCheckBox();
    textField = new JTextField();
    this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
    this.add(checkBox);
    this.add(textField);

    //  set the size of the text field
    double height = checkBox.getPreferredSize().getHeight();
    Dimension dim = textField.getPreferredSize();
    dim.setSize(50 * height, height);
    textField.setMaximumSize(dim);
    textField.setEnabled(checkBox.isSelected());
    checkBox.addActionListener(new CheckBoxActionListener(this));
  }
  
  /**
   * Label string constructor
   * @param label
   */
  public CheckBoxTextField(String label) {
    this();
    checkBox.setText(label);
  }

  String getCheckBoxLabel() {
    return checkBox.getText();
  }

  void setCheckBoxSelected(boolean state) {
    checkBox.setSelected(state);
    textField.setEnabled(state);
  }

  boolean isCheckBoxSelected() {
    return checkBox.isSelected();
  }

  void setTextField(String string) {
    textField.setText(string);
  }

  String getTextField() {
    return textField.getText();
  }

  //  Action event handler for the check box
  void manageCheckBoxState(ActionEvent event) {
    textField.setEnabled(checkBox.isSelected());
  }

  //  ActionListener class for the check box
  class CheckBoxActionListener implements ActionListener {
    CheckBoxTextField listenee;

    CheckBoxActionListener(CheckBoxTextField checkBoxTextField) {
      listenee = checkBoxTextField;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.manageCheckBoxState(event);
    }
  }

}