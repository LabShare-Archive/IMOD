package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JTextField;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description: Check box and text field.  Text field is enabled only when
 * check box is checked.</p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.1  2010/03/03 05:02:42  sueh
 * <p> bug# 1311 A checkbox which enables/disables a text field.
 * <p> </p>
 */
final class CheckTextField {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox checkBox;
  private final JTextField textField = new JTextField();
  private final String label;

  private CheckTextField(final String label) {
    this.label = label;
    checkBox = new CheckBox();
    setLabel(label);
  }

  static CheckTextField getInstance(final String label) {
    CheckTextField instance = new CheckTextField(label);
    instance.createPanel();
    instance.updateDisplay();
    instance.addListeners();
    return instance;
  }
  
  void setLabel(final String label) {
    checkBox.setText(label);
    String name = Utilities.convertLabelToName(label);
    textField.setName(UITestFieldType.TEXT_FIELD.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(textField.getName() + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  private void createPanel() {
    //root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.add(checkBox);
    pnlRoot.add(textField);
  }

  void setToolTipText(final String text) {
    String tooltip = TooltipFormatter.INSTANCE.format(text);
    pnlRoot.setToolTipText(tooltip);
    textField.setToolTipText(tooltip);
    checkBox.setToolTipText(text);
  }

  Component getRootComponent() {
    return pnlRoot;
  }

  private void addListeners() {
    checkBox.addActionListener(new CheckTextFieldActionListener(this));
  }

  public void addActionListener(ActionListener actionListener) {
    checkBox.addActionListener(actionListener);
  }

  void setText(final String input) {
    textField.setText(input);
  }

  String getLabel() {
    return label;
  }

  void setSelected(final boolean selected) {
    checkBox.setSelected(selected);
    updateDisplay();
  }

  boolean isSelected() {
    return checkBox.isSelected();
  }

  String getText() {
    return textField.getText();
  }

  void setTextPreferredWidth(int width) {
    Dimension size = textField.getSize();
    size.width = width;
    textField.setPreferredSize(size);
  }

  void setTextFieldVisible(boolean visible) {
    textField.setVisible(visible);
  }

  private void updateDisplay() {
    textField.setEnabled(checkBox.isSelected());
  }

  private void action() {
    updateDisplay();
  }

  private static final class CheckTextFieldActionListener implements
      ActionListener {
    private final CheckTextField checkTextField;

    private CheckTextFieldActionListener(final CheckTextField checkTextField) {
      this.checkTextField = checkTextField;
    }

    public void actionPerformed(final ActionEvent actionEvent) {
      checkTextField.action();
    }
  }
}
