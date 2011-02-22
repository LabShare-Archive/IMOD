/**
* <p>Description: </p>
*
* <p>Copyright: Copyright ?? 2002, 2003</p>
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
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 3.6  2006/07/20 17:19:25  sueh
* <p> bug# 848 Made UIParameters a singleton.
* <p>
* <p> Revision 3.5  2005/07/06 23:31:48  sueh
* <p> bug# 619 added setstandardsize
* <p>
* <p> Revision 3.4  2005/05/18 22:36:28  sueh
* <p> bug# 662 In format(), handling a null text parameter.
* <p>
* <p> Revision 3.3  2004/11/19 23:48:32  sueh
* <p> *** empty log message ***
* <p>
* <p> Revision 3.2.2.1  2004/09/17 21:34:38  sueh
* <p> bug# 520 the get ui functions are needed by non-buttons, so move then
* <p> to UIUtilities.
* <p>
* <p> Revision 3.2  2004/08/19 02:46:26  sueh
* <p> was using == when I should have been using .equals()
* <p>
* <p> Revision 3.1  2004/03/15 20:31:48  rickg
* <p> Bug# 414 Fixed short string handling
* <p>
* <p> Revision 3.0  2003/11/07 23:19:01  rickg
* <p> Version 1.0.0
* <p>
* <p> Revision 1.2  2003/11/03 22:18:11  sueh
* <p> constructor should be private
* <p>
* <p> Revision 1.1  2003/10/21 02:31:42  sueh
* <p> Bug325 New class, holds common button functionality
* <p> </p>
*/

package etomo.ui.swing;

import java.awt.Dimension;
import java.lang.String;

import javax.swing.AbstractButton;
import javax.swing.UIManager;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ButtonHelper {
  public static final String rcsid = "$$Id$$";

  static final Dimension MULTI_LINE_BUTTON_DIM = UIParameters.INSTANCE
      .getButtonDimension();

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

  final static void setStandardSize(AbstractButton button) {
    button.setPreferredSize(ButtonHelper.MULTI_LINE_BUTTON_DIM);
    button.setMaximumSize(ButtonHelper.MULTI_LINE_BUTTON_DIM);
    button.setMinimumSize(ButtonHelper.MULTI_LINE_BUTTON_DIM);
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
  private ButtonHelper() {
  }

}
