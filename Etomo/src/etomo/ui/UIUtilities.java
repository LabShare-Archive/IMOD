/**
 * <p>Description: UI utility static functions </p>
 * 
 * <p>Copyright: Copyright (c) 2002-2004</p>
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
 * <p> Revision 1.1  2004/04/07 21:02:30  rickg
 * <p> Initial revision
 * <p> </p>
 */

package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;

import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.JComponent;

public class UIUtilities {
  public static final String rcsid = "$Id$";

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
      if (children[i] instanceof AbstractButton) {
        AbstractButton btn = (AbstractButton) children[i];
        btn.setPreferredSize(size);
        btn.setMaximumSize(size);
      }
    }
  }
  
  /**
   * Set the button sizes (preferred and maximum) of a button
   * @param container
   * @param size
   */
  public static void setButtonSize(AbstractButton button, Dimension size) {
    button.setPreferredSize(size);
    button.setMaximumSize(size);
  }

}