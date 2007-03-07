package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.JRadioButton;
import javax.swing.JToggleButton;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.UITestField;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class RadioButton {
  public static final String rcsid = "$Id$";

  private final JRadioButton radioButton;
  private final EtomoNumber radioValue = new EtomoNumber();

  RadioButton(final String text) {
    this(text, EtomoNumber.INTEGER_NULL_VALUE);
  }

  RadioButton(final String text, final int radioValue) {
    radioButton = new JRadioButton(text);
    setName(text);
    this.radioValue.set(radioValue);
  }

  void setText(final String text) {
    radioButton.setText(text);
    setName(text);
  }

  void setName(final String text) {
    String name = Utilities.convertLabelToName(text);
    radioButton.setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.RADIO_BUTTON.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  ConstEtomoNumber getRadioValue() {
    return radioValue;
  }

  boolean equalsRadioValue(final ConstEtomoNumber radioValue) {
    return this.radioValue.equals(radioValue);
  }

  void setToolTipText(final String text) {
    radioButton.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  void addActionListener(final ActionListener actionListener) {
    radioButton.addActionListener(actionListener);
  }

  void setSelected(final boolean selected) {
    radioButton.setSelected(selected);
  }

  boolean isSelected() {
    return radioButton.isSelected();
  }

  AbstractButton getAbstractButton() {
    return radioButton;
  }

  void setPreferredSize(final Dimension preferredSize) {
    radioButton.setPreferredSize(preferredSize);
  }

  String getText() {
    return radioButton.getText();
  }

  void setModel(final ButtonModel newModel) {
    radioButton.setModel(newModel);
  }

  String getName() {
    return radioButton.getName();
  }

  Component getComponent() {
    return radioButton;
  }

  void setEnabled(final boolean enable) {
    radioButton.setEnabled(enable);
  }

  String getActionCommand() {
    return radioButton.getActionCommand();
  }

  boolean isEnabled() {
    return radioButton.isEnabled();
  }

  void setAlignmentX(float alignmentX) {
    radioButton.setAlignmentX(alignmentX);
  }

  Object[] getSelectedObjects() {
    return radioButton.getSelectedObjects();
  }

  static final class RadioButtonModel extends JToggleButton.ToggleButtonModel {
    private final RadioButtonParent parent;

    RadioButtonModel(RadioButtonParent parent) {
      super();
      this.parent = parent;
    }

    public void setSelected(boolean selected) {
      super.setSelected(selected);
      parent.msgSelected();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.9  2007/03/03 01:03:49  sueh
 * <p> bug# 973 Added a RadioButtonModel for classes that use a radio button, and
 * <p> want to respond to the setSelected calls that automatically turn off other buttons
 * <p> in the group.
 * <p>
 * <p> Revision 1.8  2007/02/09 00:52:13  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.7  2006/05/16 21:36:30  sueh
 * <p> bug# 856 Changing the name whenever the label is changed so that its easy to
 * <p> see what the name is.
 * <p>
 * <p> Revision 1.6  2006/04/25 19:19:47  sueh
 * <p> bug# 787 Added UITestField, an enum style class which contains the
 * <p> fields found in uitestaxis.adoc files.
 * <p>
 * <p> Revision 1.5  2006/04/06 20:17:51  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.4  2006/01/12 17:37:28  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.3  2006/01/11 22:32:48  sueh
 * <p> bug# 675 fixed print names functionality
 * <p>
 * <p> Revision 1.2  2006/01/04 20:28:01  sueh
 * <p> bug# 675 For printing the name:  putting the type first and making the type
 * <p> as constant.
 * <p>
 * <p> Revision 1.1  2005/12/23 02:19:17  sueh
 * <p> bug# 675 A class to allow automatic naming of radio buttons.
 * <p> </p>
 */
