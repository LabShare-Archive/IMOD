package etomo.ui;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JToggleButton;
import javax.swing.plaf.ColorUIResource;

import java.awt.Dimension;
import java.awt.Insets;
import java.lang.String;

/**
* <p>Description: MultiLineToggleButton is a JToggleButton which can
* contain a multi-line label.  It can be disabled correctly and will
* work with either html or plain text.
* 
* If the default color resources are missing, MultiLineToggleButton
* will pick colors.
* 
* Side effect: Any plain text used as a label will be wrapped in html tags
* and bolded.</p>
*
* <p>Copyright: Copyright © 2002, 2003</p>
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
* <p> Revision 3.3  2004/11/19 23:59:27  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 3.2.4.1  2004/09/17 21:38:10  sueh
* <p> bug# 520 getDefaultUIColor() was moved to UIUtilities
* <p>
* <p> Revision 3.2  2004/04/26 03:16:22  rickg
* <p> Set text margin to 2 all around
* <p>
* <p> Revision 3.1  2004/01/30 22:46:17  sueh
* <p> bug# 356 eliminated the "missing ui" error message
* <p>
* <p> Revision 3.0  2003/11/07 23:19:01  rickg
* <p> Version 1.0.0
* <p>
* <p> Revision 1.7  2003/10/21 02:33:38  sueh
* <p> Bug325 Pulled out common button functionality and placed it in ButtonHelper.
* <p>
* <p> Revision 1.6  2003/10/20 17:07:52  sueh
* <p> bug317 removing platform dependent methods
* <p>
* <p> Revision 1.5  2003/10/20 17:06:11  sueh
* <p> bug317
* <p>
* <p> Revision 1.4  2003/10/20 16:48:35  sueh
* <p> bug317 removing diagnostic prints
* <p>
* <p> Revision 1.3  2003/10/20 16:44:00  sueh
* <p> bug317 replacing assert with an unchecked exception
* <p>
* <p> Revision 1.2  2003/10/17 22:27:24  sueh
* <p> Bug317 selected, disabled text is now visible
* <p>
* <p> Revision 1.1  2003/10/17 01:56:50  sueh
* <p> Bug317 added new class - multiline button that disables correctly
* <p> </p>
*/

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
final public class MultiLineToggleButton extends JToggleButton {
  public static final String rcsid =
    "$$Id$$";

  public static final String ENABLED_UNSELECTED_TEXT_COLOR_PROPERTY =
    "ToggleButton.foreground";
  public static final String ENABLED_SELECTED_TEXT_COLOR_PROPERTY =
    "ToggleButton.foreground";
  public static final String DISABLED_UNSELECTED_TEXT_COLOR_PROPERTY =
    "ToggleButton.disabledText";
  public static final String DISABLED_SELECTED_TEXT_COLOR_PROPERTY =
    "ToggleButton.darkShadow"; //in the default is is the same color as 
                               //disabledSelectedText;

  public MultiLineToggleButton() {
    this(null, null, false);
  }

  public MultiLineToggleButton(Icon icon) {
    this(null, icon, false);
  }

  public MultiLineToggleButton(Icon icon, boolean selected) {
    this(null, icon, selected);
  }

  public MultiLineToggleButton(String text) {
    this(text, null, false);
  }

  public MultiLineToggleButton(String text, boolean selected) {
    this(text, null, selected);
  }

  public MultiLineToggleButton(Action a) {
    super(a);
    init();
  }

  public MultiLineToggleButton(String text, Icon icon) {
    this(text, icon, false);
  }

  public MultiLineToggleButton(String text, Icon icon, boolean selected) {
    super(ButtonHelper.format(text), icon, selected);
    init();
  }
  
  public void setSize(Dimension size) {
    setPreferredSize(size);
    setMaximumSize(size);
  }

  public void setEnabled(boolean isEnabled) {
    super.setEnabled(isEnabled);
    if (super.isSelected()) {
      super.setForeground(
        isEnabled ? enabledSelectedTextColor : disabledSelectedTextColor);
    }
    else {
      super.setForeground(
        isEnabled ? enabledUnselectedTextColor : disabledUnselectedTextColor);
    }
  }

  public void setText(String text) {
    super.setText(ButtonHelper.format(text));
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  //protected methods

  protected String paramString() {
    return ",enabledUnselectedTextColor="
      + enabledUnselectedTextColor
      + ", enabledSelectedTextColor ="
      + enabledSelectedTextColor
      + ", disabledUnselectedTextColor ="
      + disabledUnselectedTextColor
      + ", disabledSelectedTextColor ="
      + disabledSelectedTextColor
      + super.paramString();
  }

  protected void init(String text, Icon icon) {
    super.init(ButtonHelper.format(text), icon);
    setMargin(new Insets(2,2,2,2));
  }

  //private implementation

  private static ColorUIResource enabledUnselectedTextColor = null;
  private static ColorUIResource enabledSelectedTextColor = null;
  private static ColorUIResource disabledUnselectedTextColor = null;
  private static ColorUIResource disabledSelectedTextColor = null;

  //if changing this class to inheritable, make this method protected
  private void init() {
    setMargin(new Insets(2,2,2,2));
    enabledUnselectedTextColor =
      getDefaultUIColor(ENABLED_UNSELECTED_TEXT_COLOR_PROPERTY);
    enabledSelectedTextColor =
      getDefaultUIColor(ENABLED_SELECTED_TEXT_COLOR_PROPERTY);
    disabledUnselectedTextColor =
      getDefaultUIColor(DISABLED_UNSELECTED_TEXT_COLOR_PROPERTY);
    disabledSelectedTextColor =
      getDefaultUIColor(DISABLED_SELECTED_TEXT_COLOR_PROPERTY);
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
    if (property == ENABLED_UNSELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(0, 0, 0);
    }
    else if (property == ENABLED_SELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(0, 0, 0);
    }
    else if (property == DISABLED_UNSELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(153, 153, 153);
    }
    else if (property == DISABLED_SELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(102, 102, 102);
    }
    else {
      throw new IllegalArgumentException(property);
    }
  }

}
