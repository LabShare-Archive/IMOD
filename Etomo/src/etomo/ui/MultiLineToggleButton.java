package etomo.ui;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JToggleButton;
import javax.swing.plaf.ColorUIResource;
import java.lang.String;

import etomo.ApplicationManager;

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
* <p> $Log$ </p>
*/

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class MultiLineToggleButton extends JToggleButton {
  public static final String rcsid = " $Id : $ ";
  
  public static ColorUIResource ENABLED_COLOR = getUIToggleButtonTextColor(true);
  public static ColorUIResource DISABLED_COLOR = getUIToggleButtonTextColor(false);
 
  public MultiLineToggleButton() {
    super();
  }
  public MultiLineToggleButton(Icon icon) {
    super(icon);
  }
  public MultiLineToggleButton(Icon icon, boolean selected) {
    super(icon, selected);
  }
  public MultiLineToggleButton(String text) {
    super(format(text));
  }
  public MultiLineToggleButton(String text, boolean selected) {
    super(format(text), selected);
  }
  public MultiLineToggleButton(Action a) {
    super(a);
  }
  public MultiLineToggleButton(String text, Icon icon) {
    super(format(text), icon);
  }
  public MultiLineToggleButton(String text, Icon icon, boolean selected) {
    super(format(text), icon, selected);
  }
  
  public void setEnabled(boolean isEnabled) {
    super.setEnabled(isEnabled);
    super.setForeground(isEnabled ? ENABLED_COLOR : DISABLED_COLOR);
  }
  
  public void setText(String text) {
    super.setText(format(text));
  }
  
  public static String format(String text) {
    if (text.substring(0,6).equalsIgnoreCase("<html>")) {
      return text;
    }
    text = "<html><b>".concat(text).concat("</b>");
    return text;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }
  
  public String paramString() {
    return ",ENABLED_COLOR=" + ENABLED_COLOR + ", DISABLED_COLOR =" + DISABLED_COLOR
      + super.paramString();
  }
  
  protected void init(String text, Icon icon) {
    super.init(format(text), icon);
  }

  private static ColorUIResource getUIToggleButtonTextColor(boolean isEnabled) {
    ColorUIResource color = new ColorUIResource(0, 0, 0);
    String resourceName = isEnabled ? "ToggleButton.foreground" : "ToggleButton.disabledText";
    color = (ColorUIResource) ApplicationManager.getDefaultUIResource(color, resourceName);
    if (color == null) {
      System.out.println("Warning: Cannot find default resource: "+ resourceName);
      return createDefaultColor(isEnabled);
    }
    return color;
  }
  
  private static ColorUIResource createDefaultColor(boolean isEnabled) {
    if (isEnabled) {
      return new ColorUIResource(0,0,0);   
    }
    else {
      return new ColorUIResource(153,153,153);
    }
  }

}

