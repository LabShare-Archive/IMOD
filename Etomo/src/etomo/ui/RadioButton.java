package etomo.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionListener;

import javax.swing.AbstractButton;
import javax.swing.ButtonGroup;
import javax.swing.ButtonModel;
import javax.swing.JRadioButton;
import javax.swing.JToggleButton;
import javax.swing.border.Border;
import javax.swing.event.ChangeListener;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.EnumeratedType;
import etomo.type.EtomoAutodoc;
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
final class RadioButton implements RadioButtonInterface {
  public static final String rcsid = "$Id$";

   final JRadioButton radioButton;
  private final EnumeratedType enumeratedType;

  RadioButton(final String text) {
    this(text, null, null);
  }

  RadioButton(final String text, ButtonGroup group) {
    this(text, null, group);
  }
  
  RadioButton(ButtonGroup group) {
    this("", null, group);
  }

  RadioButton(final String text, final EnumeratedType enumeratedType) {
    this(text, enumeratedType, null);
  }

  RadioButton(final String text, final EnumeratedType enumeratedType,
      ButtonGroup group) {
    radioButton = new JRadioButton(text);
    radioButton.setModel(new RadioButtonModel(this));
    setName(text);
    this.enumeratedType = enumeratedType;
    if (group != null) {
      group.add(radioButton);
    }
    if (enumeratedType != null && enumeratedType.isDefault()) {
      radioButton.setSelected(true);
    }
  }

  void setText(final String text) {
    radioButton.setText(text);
    setName(text);
  }
  
   void setBorderPainted(boolean b) {
    radioButton.setBorderPainted(b);
  }
   
   public void setBorder(Border border) {
     radioButton.setBorder( border);
   }
   
   public void setForeground(Color fg) {
     radioButton.setForeground(fg);
   }
   
   public int getWidth() {
     return radioButton.getWidth();
   }
   
   public int getHeight() {
     return radioButton.getHeight();
   }
   
   public Border getBorder() {
     return radioButton.getBorder();
   }
   
  void setName(final String text) {
    String name = Utilities.convertLabelToName(text);
    radioButton.setName(name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestField.RADIO_BUTTON.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  boolean equals(final EnumeratedType enumeratedType) {
    return this.enumeratedType == enumeratedType;
  }

  /**
   * Sets a formated tooltip.  If enumeratedType is set, retrieves an enum
   * tooltip from the autodoc
   * @param section
   * @return the unformatted tooltip
   */
  String setToolTipText(ReadOnlySection section) {
    if (enumeratedType == null) {
      setToolTipText((String) null);
      return null;
    }
    String text = EtomoAutodoc.getTooltip(section, enumeratedType.toString());
    radioButton.setToolTipText(TooltipFormatter.INSTANCE.format(text));
    return text;
  }

  void setToolTipText(final String text) {
    radioButton.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  void addActionListener(final ActionListener actionListener) {
    radioButton.addActionListener(actionListener);
  }
  
   void addChangeListener(final ChangeListener listener) {
    radioButton.addChangeListener(listener);
  }

  void setSelected(final boolean selected) {
    radioButton.setSelected(selected);
  }

  public void msgSelected() {
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

  public EnumeratedType getEnumeratedType() {
    return enumeratedType;
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
    private final RadioButtonInterface radioButton;

    RadioButtonModel(RadioButtonInterface radioButton) {
      super();
      this.radioButton = radioButton;
    }

    public void setSelected(boolean selected) {
      super.setSelected(selected);
      radioButton.msgSelected();
    }

    EnumeratedType getEnumeratedType() {
      return radioButton.getEnumeratedType();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.16  2007/09/27 21:04:32  sueh
 * <p> bug# 1044 Expanded RadioButton so it can be used in RadioButtonCell.
 * <p>
 * <p> Revision 1.15  2007/09/07 00:28:23  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.14  2007/05/17 23:50:02  sueh
 * <p> bug# 964 In setToolTipText(), return the tooltip.
 * <p>
 * <p> Revision 1.13  2007/05/08 01:20:51  sueh
 * <p> bug# 964 Added setToolTipText(ReadOnlySection) to set an enum tooltip.
 * <p>
 * <p> Revision 1.12  2007/04/13 20:37:15  sueh
 * <p> bug# 964 Removed radioValue and added EnumeratedType, which is the
 * <p> interface for enumeration types.
 * <p>
 * <p> Revision 1.11  2007/03/20 00:45:40  sueh
 * <p> bug# 964 Added constructors which take ButtonGroup.
 * <p>
 * <p> Revision 1.10  2007/03/07 21:12:32  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.
 * <p>
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
