package etomo.ui;

import javax.swing.JButton;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.plaf.ColorUIResource;
import java.lang.String;

/**
* <p>Description: </p>
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
* <p> Revision 1.1  2003/10/21 02:32:40  sueh
* <p> Bug325 New class, behaves like JButton, except that it automatically makes button text multi-line.
* <p> </p>
*/


/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class MultiLineButton extends JButton {
  public static final String rcsid = "$$Id$$";

  public static final String ENABLED_TEXT_COLOR_PROPERTY =
    "Button.foreground";
  public static final String DISABLED_TEXT_COLOR_PROPERTY =
    "Button.disabledText";

  public MultiLineButton() {
    this(null, null);
  }

  public MultiLineButton(Icon icon) {
    this(null, icon);
  }

  public MultiLineButton(String text) {
    this(text, null);
  }

  public MultiLineButton(Action a) {
    super(a);
    init();
  }

  public MultiLineButton(String text, Icon icon) {
    super(ButtonHelper.format(text), icon);
    init();
  }

  public void setEnabled(boolean isEnabled) {
    super.setEnabled(isEnabled);
    super.setForeground(
      isEnabled ? enabledTextColor : disabledTextColor);
  }
  
  public void setText(String text) {
    super.setText(ButtonHelper.format(text));
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  //protected methods

  protected String paramString() {
    return ",enabledTextColor="
      + enabledTextColor
      + ", disabledTextColor ="
      + disabledTextColor
      + super.paramString();
  }

  protected void init(String text, Icon icon) {
    super.init(ButtonHelper.format(text), icon);
  }

  //private implementation

  private static ColorUIResource enabledTextColor = null;
  private static ColorUIResource disabledTextColor = null;

  //if changing this class to inheritable, make this method protected
  private void init() {
    enabledTextColor =
      getDefaultUIColor(ENABLED_TEXT_COLOR_PROPERTY);
    disabledTextColor =
      getDefaultUIColor(DISABLED_TEXT_COLOR_PROPERTY);
  }

  private ColorUIResource getDefaultUIColor(String property) {
    ColorUIResource color = ButtonHelper.getDefaultUIColor(property);
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

