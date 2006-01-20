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
 * <p> Revision 1.10  2006/01/11 23:20:24  sueh
 * <p> bug# 675 Changed convertLabelToName() so that it can use
 * <p> PrimativeTokenizer.  This means that all whitespace is treated correctly.
 * <p>
 * <p> Revision 1.9  2006/01/04 00:09:29  sueh
 * <p> bug# 675 Fix convertLabelToName.
 * <p>
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
import java.io.IOException;

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
    String name = label.trim().toLowerCase();
    //strip unnecessary characters
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(name);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    try {
      tokenizer.initialize();
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      return label;
    }
    boolean ignoreParen = false;
    boolean ignoreBracket = false;
    while (token != null && !token.is(Token.EOF) && !token.is(Token.EOL)) {
      if (token.equals(Token.SYMBOL, '(')) {
        //ignore everything in parenthesis
        ignoreParen = true;
      }
      else if (token.equals(Token.SYMBOL, ')')) {
        ignoreParen = false;
      }
      if (token.equals(Token.SYMBOL, '<')) {
        //replace everything in angle brackets with one space
        ignoreBracket = true;
        buffer.append(' ');
      }
      else if (token.equals(Token.SYMBOL, '>')) {
        ignoreBracket = false;
      }
      else if (token.equals(Token.SYMBOL, ':')) {
        //ignore everything after ":"
        break;
      }
      else if (!ignoreParen && !ignoreBracket) {
        buffer.append(token.getValue());
      }
      try {
        token = tokenizer.next();
      }
      catch (IOException e) {
        e.printStackTrace();
        break;
      }
    }
    //convert whitespace to "-"
    name = buffer.toString().trim();
    tokenizer = new PrimativeTokenizer(name);
    buffer = new StringBuffer();
    try {
      tokenizer.initialize();
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      return name;
    }
    while (token != null && !token.is(Token.EOF) && !token.is(Token.EOL)) {
      if (token.is(Token.WHITESPACE)) {
        buffer.append('-');
      }
      else {
        buffer.append(token.getValue());
      }
      try {
        token = tokenizer.next();
      }
      catch (IOException e) {
        e.printStackTrace();
        break;
      }
    }
    return buffer.toString();
  }
}