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
* <p> $Log$ </p>
*/

package etomo.ui;

import java.lang.String;
import javax.swing.plaf.ColorUIResource;
import javax.swing.UIManager;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ButtonHelper {
  public static final String rcsid = "$$Id$$";

  public static String format(String text) {
    if (text.substring(0, 6).equalsIgnoreCase("<html>")) {
      return text;
    }
    text = "<html><b>".concat(text).concat("</b>");
    return text;
  }

  public static ColorUIResource getDefaultUIColor(String property) {
    ColorUIResource color = new ColorUIResource(0, 0, 0);
    color = (ColorUIResource) getDefaultUIResource(color, property);
    return color;
  }

  public static Object getDefaultUIResource(Object target, String name) {
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    if (target == null || name == null) {
      return null;
    }
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = UIManager.get(key);
      if (key.toString() == name && value.getClass() == target.getClass()) {
        return value;
      }
    }
    return null;
  }

  /**
   * Prints all DefaultUI resource entries for a type of resource, such as a
   * Button or a ToggleButton.
   *
   * @param type = Resource type.  Not case sensitive.  Package information is not required.
   */
  public static void printDefaultUIResource(String type) {
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    if (type == null) {
      printDefaultUIResource();
      return;
    }
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = UIManager.get(key);
      String keyString = key.toString();
      int index = keyString.indexOf('.');
      int prevIndex = 0;
      while (index >= 0) {
        if (keyString.substring(prevIndex, index).equalsIgnoreCase(type)) {
          System.out.println(key + "=" + value);
          break;
        }
        prevIndex = index + 1;

        index = keyString.indexOf('.', index + 1);
      }
    }
  }

  /**
   * Prints all DefaultUI resource entries.
   *
   */
  public static void printDefaultUIResource() {
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = UIManager.get(key);
      System.out.println(key + "=" + value);
    }
  }

  //private implementation

  //All public and protected methods are static, so constructor can be private
  public ButtonHelper() {
  }

}
