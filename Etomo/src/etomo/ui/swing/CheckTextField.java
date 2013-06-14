package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentListener;
import javax.swing.text.Document;

import etomo.EtomoDirector;
import etomo.logic.FieldValidator;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.EtomoNumber;
import etomo.type.UITestFieldType;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.ui.UIComponent;
import etomo.util.Utilities;

/**
 * <p>Description: A self-naming check box and text field.  The text field is enabled only
 * when the check box is checked.  Implements StateChangeSource with its state equal to
 * whether it has changed since it was checkpointed.</p>
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
 * <p> Revision 1.6  2011/05/03 03:13:51  sueh
 * <p> bug# 1416 Checkpointing from a parameter instead of isSelected().  Removed the reporter (phased out).
 * <p>
 * <p> Revision 1.5  2011/04/25 23:36:03  sueh
 * <p> bug# 1416 Implemented StateChangeActionSource.  Added equals(Object) and equals(Document) so
 * <p> StateChangedReporter can find instances of this class.  Changed isChanged to isDifferentFromCheckpoint.
 * <p> Added getState.
 * <p>
 * <p> Revision 1.4  2011/04/04 17:17:27  sueh
 * <p> bug# 1416 Added savedValue, checkpoint, isChanged.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:05:48  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 04:58:39  sueh
 * <p> bug# 1416 Added setEnabled.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.2  2010/03/05 04:02:32  sueh
 * <p> bug# 1319 Added setLabel.
 * <p>
 * <p> Revision 1.1  2010/03/03 05:02:42  sueh
 * <p> bug# 1311 A checkbox which enables/disables a text field.
 * <p> </p>
 */
final class CheckTextField implements UIComponent, SwingComponent {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox checkBox;
  private final JTextField textField = new JTextField();
  private final String label;
  private final EtomoNumber.Type numericType;
  private final FieldType fieldType;

  private String checkpointValue = null;
  private EtomoNumber nCheckpointValue = null;

  private CheckTextField(final FieldType fieldType, final String label,
      final EtomoNumber.Type numericType) {
    this.label = label;
    this.numericType = numericType;
    this.fieldType = fieldType;
    checkBox = new CheckBox();
    setLabel(label);
  }

  static CheckTextField getInstance(final FieldType fieldType, final String label) {
    CheckTextField instance = new CheckTextField(fieldType, label, null);
    instance.createPanel();
    instance.updateDisplay();
    instance.addListeners();
    return instance;
  }

  static CheckTextField getNumericInstance(final FieldType fieldType,
      final String tfLabel, final EtomoNumber.Type numericType) {
    CheckTextField instance = new CheckTextField(fieldType, tfLabel, numericType);
    instance.createPanel();
    instance.updateDisplay();
    instance.addListeners();
    return instance;
  }

  public boolean equals(final Object object) {
    return object == checkBox || object == textField;
  }

  public boolean equals(final Document document) {
    return textField.getDocument() == document;
  }

  void setLabel(final String label) {
    checkBox.setText(label);
    String name = Utilities.convertLabelToName(label);
    textField.setName(UITestFieldType.TEXT_FIELD.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(textField.getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
  }

  /**
   * Checkpoints checkbox from checkboxValue.  Saves textValue as the text field
   * checkpoint.
   */
  void checkpoint(final boolean checkboxValue, final String textValue) {
    checkBox.checkpoint(checkboxValue);
    checkpointValue = textValue;
    if (numericType != null) {
      if (nCheckpointValue == null) {
        nCheckpointValue = new EtomoNumber(numericType);
      }
      nCheckpointValue.set(checkpointValue);
    }
  }

  /**
   * First checks the checkbox and returns true if the checkbox is different from its
   * checkpoint.  Then it returns false if the checkbox is not selected, since the value
   * of the text field is not in use.  Also, if the field is disabled then return false
   * because its value doesn't matter.  Then it returns true if the checkpoint has not
   * been done; the checkpoint value is from an outside value, so the current value must
   * be different from a non-existant checkpoint value.  After eliminating these
   * possibilities, it returns a boolean based on the difference between the text field
   * value and the checkpointed value.
   * @return
   */
  boolean isDifferentFromCheckpoint() {
    if (!checkBox.isEnabled() && !textField.isEnabled()
        || (!checkBox.isVisible() && !textField.isVisible())) {
      return false;
    }
    if (checkBox.isDifferentFromCheckpoint()) {
      return true;
    }
    if (!checkBox.isSelected() || !textField.isEnabled() || !textField.isVisible()) {
      return false;
    }
    if (checkpointValue == null) {
      return true;
    }
    if (numericType == null) {
      return !checkpointValue.equals(textField.getText());
    }
    return !nCheckpointValue.equals(textField.getText());
  }

  void setEnabled(final boolean enable) {
    checkBox.setEnabled(enable);
    textField.setEnabled(enable);
  }

  private void createPanel() {
    // root panel
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

  void setCheckBoxToolTipText(final String text) {
    checkBox.setToolTipText(text);
  }

  void setFieldToolTipText(final String text) {
    textField.setToolTipText(TooltipFormatter.INSTANCE.format(text));
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

  public void addDocumentListener(DocumentListener listener) {
    textField.getDocument().addDocumentListener(listener);
  }

  void setText(final String input) {
    textField.setText(input);
  }

  String getActionCommand() {
    return checkBox.getActionCommand();
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

  public SwingComponent getUIComponent() {
    return this;
  }

  public Component getComponent() {
    return pnlRoot;
  }

  String getText(final boolean doValidation) throws FieldValidationFailedException {
    String text = textField.getText();
    if (doValidation && textField.isEnabled()) {
      text = FieldValidator.validateText(text, fieldType, this, getQuotedLabel(), false);
    }
    return text;
  }

  /**
   * Get text without validation
   */
  String getText() {
    return textField.getText();
  }

  String getQuotedLabel() {
    return Utilities.quoteLabel(checkBox.getText());
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

  private static final class CheckTextFieldActionListener implements ActionListener {
    private final CheckTextField checkTextField;

    private CheckTextFieldActionListener(final CheckTextField checkTextField) {
      this.checkTextField = checkTextField;
    }

    public void actionPerformed(final ActionEvent actionEvent) {
      checkTextField.action();
    }
  }
}
