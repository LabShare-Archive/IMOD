package etomo.ui;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.border.Border;
import javax.swing.plaf.ColorUIResource;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.lang.String;

/**
* <p>Description: </p>
*
* <p>Copyright: Copyright © 2002 - 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $Author$
*
* @version $Revision$
*
* <p> $Log$
* <p> Revision 3.5  2005/08/10 20:44:42  sueh
* <p> bug# 711  Added getToggleButtonInstance(), isEnabled(), and
* <p> isSelected()
* <p>
* <p> Revision 3.4  2005/08/09 20:25:15  sueh
* <p> bug# 711  No longer inheriting JButton in MultiLineButton.  This allows
* <p> MultiLineButton to treate toggling as an attribute.  Then we can get rid of
* <p> MultiLineToggleButton.  Then we can have one Run3dmodButton which
* <p> can be toggle or non-toggle.
* <p>
* <p> Revision 3.3  2005/07/06 23:37:05  sueh
* <p> bug# 619 removed unused constructors
* <p>
* <p> Revision 3.2  2004/11/19 23:59:12  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 3.1.4.1  2004/09/17 21:37:56  sueh
* <p> bug# 520 getDefaultUIColor() was moved to UIUtilities
* <p>
* <p> Revision 3.1  2004/04/26 03:16:22  rickg
* <p> Set text margin to 2 all around
* <p>
* <p> Revision 3.0  2003/11/07 23:19:01  rickg
* <p> Version 1.0.0
* <p>
* <p> Revision 1.1  2003/10/21 02:32:40  sueh
* <p> Bug325 New class, behaves like JButton, except that it automatically makes button text multi-line.
* <p> </p>
*/
public class MultiLineButton {
  public static final String rcsid = "$$Id$$";

  public static final String ENABLED_TEXT_COLOR_PROPERTY =
    "Button.foreground";
  public static final String DISABLED_TEXT_COLOR_PROPERTY =
    "Button.disabledText";
  
  private final AbstractButton button;

  public MultiLineButton() {
    this(null);
  }

  public MultiLineButton(String text) {
    this(text, false);
  }
  
  protected MultiLineButton(String text, boolean toggleButton) {
    if (toggleButton) {
      button = new JToggleButton(format(text));
    }
    else {
      button = new JButton(format(text));
    }
    init();
  }
  
  static MultiLineButton getToggleButtonInstance(String text) {
    return new MultiLineButton(text, true);
  }
  
  static MultiLineButton getToggleButtonInstance() {
    return new MultiLineButton(null, true);
  }
  
  public void setEnabled(boolean isEnabled) {
    button.setEnabled(isEnabled);
    button.setForeground(
      isEnabled ? enabledTextColor : disabledTextColor);
  }
  
  public void setText(String text) {
    button.setText(format(text));
  }
  
  public static String format(String text) {
    if (text == null) {
      return null;
    }
    if (text.toLowerCase().startsWith("<html>")) {
      return text;
    }
    text = "<html><b>".concat(text).concat("</b>");
    return text;
  }

  public String toString() {
    return "[enabledTextColor=" + enabledTextColor + ",\n  disabledTextColor ="
        + disabledTextColor + ",\n  button=" + button + ",\n "
        + super.toString() + "]\n";
  }
  
  final void addActionListener(ActionListener actionListener) {
    button.addActionListener(actionListener);
  }
  
  final String getActionCommand() {
    return button.getActionCommand();
  }
  
  final Component getComponent() {
    return button;
  }
  
  final void setVisible(boolean visible) {
    button.setVisible(visible);
  }
  
  final void setBorder(Border border) {
    button.setBorder(border);
  }
  
  final void setToolTipText(String toolTip) {
    button.setToolTipText(toolTip);
  }
  
  final Dimension getPreferredSize() {
    return button.getPreferredSize();
  }
  
  final void setAlignmentX(float alignmentX) {
    button.setAlignmentX(alignmentX);
  }
  
  final void setAlignmentY(float alignmentY) {
    button.setAlignmentY(alignmentY);
  }

  final String getText() {
    return button.getText();
  }
  
  final void addMouseListener(MouseListener mouseListener) {
    button.addMouseListener(mouseListener);
  }
  
  final void removeActionListener(ActionListener actionListener) {
    button.removeActionListener(actionListener);
  }
  /**
   * Set the button sizes (preferred and maximum) of a button
   * @param container
   * @param size
   */
  final void setSize() {
    setSize(false);
  }
  
  final void setSize(boolean setMinimum) {
    Dimension size = UIParameters.getButtonDimension();
    button.setPreferredSize(size);
    button.setMaximumSize(size);
    if (setMinimum) {
      button.setMinimumSize(size);
    }
  }
  
  final void setSize(Dimension size) {
    button.setPreferredSize(size);
    button.setMaximumSize(size);
  }
  
  final void setSelected(boolean selected) {
    button.setSelected(selected);
  }
  
  final boolean isSelected() {
    return button.isSelected();
  }
  
  final boolean isEnabled() {
    return button.isEnabled();
  }
  
  final boolean isDisplayable() {
    return button.isDisplayable();
  }
  
  //private implementation

  private static ColorUIResource enabledTextColor = null;
  private static ColorUIResource disabledTextColor = null;

  //if changing this class to inheritable, make this method protected
  private void init() {
    button.setMargin(new Insets(2,2,2,2));
    enabledTextColor =
      getDefaultUIColor(ENABLED_TEXT_COLOR_PROPERTY);
    disabledTextColor =
      getDefaultUIColor(DISABLED_TEXT_COLOR_PROPERTY);
  }

  private ColorUIResource getDefaultUIColor(String property) {
    ColorUIResource color = UIUtilities.getDefaultUIColor(property);
    if (color == null) {
      color = createDefaultColor(property);
    }
    return color;
  }

  private static ColorUIResource createDefaultColor(String property) {
    System.err.println(
      "Warning: Cannot retrieve default UI property: " + property);
    if (property == ENABLED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(0, 0, 0);
    }
    else if (property == DISABLED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(153, 153, 153);
    }
    else {
      throw new IllegalArgumentException(property);
    }
  }
}
