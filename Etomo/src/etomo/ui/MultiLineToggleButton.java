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
* <p> $Log$
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
public class MultiLineToggleButton extends JToggleButton {
  public static final String rcsid = " $Id : $ ";
  
  public static final String ENABLED_UNSELECTED_TEXT_COLOR_PROPERTY = "ToggleButton.foreground";
  public static final String ENABLED_SELECTED_TEXT_COLOR_PROPERTY = "ToggleButton.foreground";
  public static final String DISABLED_UNSELECTED_TEXT_COLOR_PROPERTY = "ToggleButton.disabledText";
  public static final String DISABLED_SELECTED_TEXT_COLOR_PROPERTY =  "ToggleButton.disabledSelectedText";

  private static ColorUIResource enabledUnselectedTextColor = null;
  private static ColorUIResource enabledSelectedTextColor = null;
  private static ColorUIResource disabledUnselectedTextColor = null;
  private static ColorUIResource disabledSelectedTextColor = null;
  
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
    super(format(text), icon, selected);
    init();
  }
  
  public void setEnabled(boolean isEnabled) {
    super.setEnabled(isEnabled);
    if (super.isSelected()) {
      super.setForeground(isEnabled ? enabledSelectedTextColor : disabledSelectedTextColor);
      System.out.println("is selected");
    }
    else {
      super.setForeground(isEnabled ? enabledUnselectedTextColor : disabledUnselectedTextColor);
      System.out.println("is not selected");
    }
    System.out.println(paramString());
  }
  
  public void setText(String text) {
    super.setText(format(text));
  }
  

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }
  
  
  protected String paramString() {
    return ",enabledUnselectedTextColor=" + enabledUnselectedTextColor
      + ", enabledSelectedTextColor =" + enabledSelectedTextColor
      + ", disabledUnselectedTextColor =" + disabledUnselectedTextColor
      + ", disabledSelectedTextColor =" + disabledSelectedTextColor   
      + super.paramString();
  }
  
  protected void init(String text, Icon icon) {
    super.init(format(text), icon);
  }


  private static String format(String text) {
    if (text.substring(0,6).equalsIgnoreCase("<html>")) {
      return text;
    }
    text = "<html><b>".concat(text).concat("</b>");
    return text;
  }
  
  private static String format(String text0, String text1) {
    //assuming no html tags
    return "<html><b>" + text0 + "<br>" + text1 + "</b>";
  }

  private static String format(String text0, String text1, String text2) {
    //assuming no html tags
    return "<html><b>" + text0 + "<br>" + text1 + "<br>" + text2 + "</b>";
  }

  private void init() {
    enabledUnselectedTextColor = getDefaultUIToggleButtonTextColor(ENABLED_UNSELECTED_TEXT_COLOR_PROPERTY);
    enabledSelectedTextColor = getDefaultUIToggleButtonTextColor(ENABLED_SELECTED_TEXT_COLOR_PROPERTY);
    disabledUnselectedTextColor = getDefaultUIToggleButtonTextColor(DISABLED_UNSELECTED_TEXT_COLOR_PROPERTY);
    disabledSelectedTextColor = getDefaultUIToggleButtonTextColor(DISABLED_SELECTED_TEXT_COLOR_PROPERTY);
  }

  private static ColorUIResource getDefaultUIToggleButtonTextColor(String property) {
    ColorUIResource color = new ColorUIResource(0, 0, 0);
    color = (ColorUIResource) ApplicationManager.getDefaultUIResource(color, property);
    if (color == null) {
      return createDefaultColor(property);
    }
    return color;
  }
  
  private static ColorUIResource createDefaultColor(String property) {
    System.err.println("Warning: Cannot retrieve default UI property: " + property);
    if (property == ENABLED_UNSELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(0,0,0);
    }
    else if (property == ENABLED_SELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(0,0,0);
    }
    else if (property == DISABLED_UNSELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(153,153,153);
    }
    else if (property == DISABLED_SELECTED_TEXT_COLOR_PROPERTY) {
      return new ColorUIResource(102,102,102);
    }
    else {
      throw new IllegalArgumentException(property);
    }
  }
    
}

