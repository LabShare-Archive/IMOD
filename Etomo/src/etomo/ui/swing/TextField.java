package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JLabel;
import javax.swing.JTextField;

import etomo.EtomoDirector;
import etomo.logic.FieldValidator;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.UITestFieldType;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.Utilities;

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
 */
final class TextField implements UIComponent {
  public static final String rcsid = "$Id$";

  private final JTextField textField = new JTextField();

  private final FieldType fieldType;
  private final String reference;

  TextField(final FieldType fieldType, final String reference) {
    this.fieldType = fieldType;
    this.reference = reference;
    setName(reference);
    // Set the maximum height of the text field box to twice the
    // font size since it is not set by default
    JLabel label = new JLabel(reference);
    Dimension maxSize = textField.getMaximumSize();
    if (label.getFont().getSize() > textField.getFont().getSize()) {
      maxSize.setSize(maxSize.getWidth(), 2 * label.getFont().getSize());
    }
    else {
      maxSize.setSize(maxSize.getWidth(), 2 * textField.getFont().getSize());
    }
    textField.setMaximumSize(maxSize);
  }

  void setToolTipText(String text) {
    textField.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  public Component getComponent() {
    return textField;
  }

  void setAlignmentX(float alignmentX) {
    textField.setAlignmentX(alignmentX);
  }

  void setEnabled(boolean enabled) {
    textField.setEnabled(enabled);
  }

  void setEditable(boolean editable) {
    textField.setEditable(editable);
  }

  void setText(String text) {
    textField.setText(text);
  }

  /**
   * Validates and returns text in text field.  Should never throw a
   * FieldValidationFailedException when doValidation is false.
   * @param doValidation
   * @return
   * @throws FieldValidationFailedException
   */
  String getText(final boolean doValidation) throws FieldValidationFailedException {
    String text = textField.getText();
    if (doValidation && textField.isEnabled() && fieldType.validationType.canValidate) {
      text = FieldValidator.validateText(text, fieldType, this, getQuotedReference());
    }
    return text;
  }
  
  /**
   * get text without validation
   * @return
   */
  String getText()   {
   return textField.getText();
  }

  private String getQuotedReference() {
    return Utilities.quoteLabel(reference);
  }

  Font getFont() {
    return textField.getFont();
  }

  Dimension getMaximumSize() {
    return textField.getMaximumSize();
  }

  void setMaximumSize(Dimension size) {
    textField.setMaximumSize(size);
  }

  void setTextPreferredWidth(final double minWidth) {
    Dimension prefSize = textField.getPreferredSize();
    prefSize.setSize(minWidth, prefSize.getHeight());
    textField.setPreferredSize(prefSize);
  }

  void setTextPreferredSize(Dimension size) {
    textField.setPreferredSize(size);
    textField.setMaximumSize(size);
  }

  void setSize(Dimension size) {
    textField.setSize(size);
  }

  Dimension getSize() {
    return textField.getSize();
  }

  Dimension getPreferredSize() {
    return textField.getPreferredSize();
  }

  String getName() {
    return textField.getName();
  }

  boolean isEnabled() {
    return textField.isEnabled();
  }

  boolean isEditable() {
    return textField.isEditable();
  }

  boolean isVisible() {
    return textField.isVisible();
  }

  private void setName(String reference) {
    String name = Utilities.convertLabelToName(reference);
    textField.setName(UITestFieldType.TEXT_FIELD.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.11  2010/03/03 05:08:06  sueh
 * <p> bug# 1311 Changed TextField.setSize to setPreferredSize.  Added getFont, getMaximumSize, setMaximumSize, and setSize.
 * <p>
 * <p> Revision 1.10  2009/11/20 17:37:52  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 1.9  2009/01/20 20:31:23  sueh
 * <p> bug# 1102 Changed UITestField to UITestFieldType.
 * <p>
 * <p> Revision 1.8  2008/05/30 22:36:39  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.7  2008/05/30 21:34:57  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.6  2008/02/19 00:47:46  sueh
 * <p> bug# 1078 Added setTextPreferredWidth.
 * <p>
 * <p> Revision 1.5  2007/12/26 22:35:07  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.4  2007/09/07 00:29:17  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.3  2007/03/30 23:54:19  sueh
 * <p> bug# 964 Wrapping JTextField instead of inheriting it.  Added automatic sizing.
 * <p>
 * <p> Revision 1.2  2007/02/09 00:53:33  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.1  2006/09/13 23:58:14  sueh
 * <p> bug# 924 Added TextField:  extends JTextField and automatically names itself.
 * <p> </p>
 */
