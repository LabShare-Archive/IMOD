/**
 * <p>Description: UI utility static functions </p>
 * 
 * <p>Copyright: Copyright (c) 2002-2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.8  2005/12/23 02:25:53  sueh
 * <p> bug# 675 Added convertLabelToName, which converts a screen label to
 * <p> a name for the screen element.
 * <p>
 * <p> Revision 1.7  2005/08/09 21:13:49  sueh
 * <p> bug# 711 Moving button sizing from UIUtilities to the multi line button
 * <p> classes.
 * <p>
 * <p> Revision 1.6  2005/07/11 23:32:26  sueh
 * <p> bug# 619 Moved code to get the screen size to UIUtilities so it can be
 * <p> used in ProcessorTable.
 * <p>
 * <p> Revision 1.5  2005/04/22 00:21:06  sueh
 * <p> bug# 615 Fix bug in getDefaultUIResource().  The function assumed that
 * <p> the parameter target was the same type as value, which is retrieved by
 * <p> the UIManager.  Fixed it so that the function also accepts a child class.
 * <p>
 * <p> Revision 1.4  2004/11/20 00:07:51  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.3.2.2  2004/09/22 22:19:09  sueh
 * <p> bug# 520 Add an option to set button minimum size to setButtonSize().
 * <p>
 * <p> Revision 1.3.2.1  2004/09/17 21:49:45  sueh
 * <p> bug# 520 Move getDefaultUIColor() and getDefaultUIResource() to this
 * <p> class from ButtonHelper because they are used in non-buttons.
 * <p>
 * <p> Revision 1.3  2004/07/21 00:20:47  sueh
 * <p> bug# 474 prevented JCheckBox from being set to button size
 * <p>
 * <p> Revision 1.2  2004/06/17 21:49:21  sueh
 * <p> bug# 474 added setButtonSize(AbstractButton...)
 * <p>
 * <p> Revision 1.1  2004/04/07 21:02:30  rickg
 * <p> Initial revision
 * <p> </p>
 */

package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;

import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.UIManager;
import javax.swing.plaf.ColorUIResource;

public class UIUtilities {
  public static final String rcsid = "$Id$";

  private static final int estimatedMenuHeight = 60;

  private static Dimension screenSize = null;

  private UIUtilities() {
  }

  /**
   * Add a component to a container followed by the default value of x space
   * @param panel
   * @param component
   */
  public static void addWithXSpace(Container panel, Component component) {
    panel.add(component);
    panel.add(Box.createRigidArea(FixedDim.x5_y0));
  }

  /**
   * Add a component to a container followed by the default value of y space
   * @param panel
   * @param component
   */
  public static void addWithYSpace(Container panel, Component component) {
    panel.add(component);
    panel.add(Box.createRigidArea(FixedDim.x0_y5));
  }

  /**
   * Add a component to a container followed by the specified value of space
   * @param panel
   * @param component
   * @param dim
   */
  public static void addWithSpace(Container panel, Component component,
      Dimension dim) {
    panel.add(component);
    panel.add(Box.createRigidArea(dim));
  }

  /**
   * Align the components in a continer to the same aligment
   * @param container
   * @param alignment
   */
  public static void alignComponentsX(Container container, float alignment) {
    Component[] children = container.getComponents();
    for (int i = 0; i < children.length; i++) {
      if (children[i] instanceof JComponent) {
        JComponent jcomp = (JComponent) children[i];
        jcomp.setAlignmentX(alignment);
      }
    }
  }

  /**
   * Set the button sizes (preferred and maximum) of all buttons in a container
   * to the same size.
   * @param container
   * @param size
   */
  public static void setButtonSizeAll(Container container, Dimension size) {
    Component[] children = container.getComponents();
    for (int i = 0; i < children.length; i++) {
      if (children[i] instanceof AbstractButton
          && !(children[i] instanceof JCheckBox)) {
        AbstractButton btn = (AbstractButton) children[i];
        btn.setPreferredSize(size);
        btn.setMaximumSize(size);
      }
    }
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
      if (key.toString().equals(name) && target.getClass().isInstance(value)) {
        return value;
      }
    }
    return null;
  }

  public static Dimension getScreenSize() {
    if (screenSize == null) {
      Toolkit toolkit = Toolkit.getDefaultToolkit();
      screenSize = toolkit.getScreenSize();
      screenSize.height -= estimatedMenuHeight;
    }
    return screenSize;
  }

  static final String convertLabelToName(String label) {
    if (label == null) {
      return null;
    }
    String name = label;
    //System.out.println("step 0: " + name);
    int index = name.indexOf(':');
    if (index != -1) {
      name = name.substring(0, index);
      //System.out.println("step 1: " + name);
    }
    int startIndex = name.indexOf('(');
    while (startIndex != -1) {
      int endIndex = name.indexOf(')');
      if (endIndex != -1) {
        name = name.substring(0, startIndex) + name.substring(endIndex + 1);
        //System.out.println("step 2: " + name);
      }
      startIndex = name.indexOf('(');
    }
    startIndex = name.indexOf('<');
    while (startIndex != -1) {
      int endIndex = name.indexOf('>');
      if (endIndex != -1) {
        name = name.substring(0, startIndex) + name.substring(endIndex + 1);
        //System.out.println("step 3: " + name);
      }
      startIndex = name.indexOf('<');
    }
    name = name.trim().toLowerCase().replace(' ', '-');
    //System.out.println("step 4: " + name);
    index = name.indexOf("--");
    while (index != -1) {
      name = name.substring(0, index) + name.substring(index + 1);
      //System.out.println("step 5: " + name);
      index = name.indexOf("--");
    }
    return name;
  }
}