package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;

import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.border.Border;
/**
* <p>Description: A JPanel-like object that places rigid areas between 
* components.  It can optionally create rigid areas outside the components on
* the layout axis.  If rigid areas are needed outside the components on both
* axes, use DoubleSpacedPanel.  It is most useful for creating panels which
* would normally need rigid areas.  It should work like a JPanel.</p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.1.2.5  2004/11/11 01:43:14  sueh
* <p> bug# 520 Removed unused functions add(Component).
* <p>
* <p> Revision 1.1.2.4  2004/11/08 22:31:33  sueh
* <p> bug# 520 Added an add() function that can handle a Component.  Not
* <p> spacing this component since it be a spacer.
* <p>
* <p> Revision 1.1.2.3  2004/10/13 23:16:04  sueh
* <p> bug# 520 Added a way to set and reset component alignments.  Added
* <p> specialized add() functions for generic etomo ui objects.  Added a way to
* <p> prevent spacing being added at the top of a panel and after a component.
* <p>
* <p> Revision 1.1.2.2  2004/09/23 23:52:23  sueh
* <p> bug# 520 Added a class description.
* <p>
* <p> Revision 1.1.2.1  2004/09/23 23:41:40  sueh
* <p> bug# 520 Panel which automatically places rigid areas between
* <p> components.  Can choose whether to place rigid areas before and after
* <p> placing all components.
* <p> </p>
*/
public class SpacedPanel {
  public static  final String  rcsid =  "$Id$";
  
  Dimension spacing;
  boolean outerSpacing = false;
  FormattedPanel panel;
  int numComponents = 0;
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "\n,spacing=" + spacing + ",\nouterSpacing="
        + outerSpacing + ",\npanel=" + panel
        + ",\nnumComponents=" + numComponents;
  } 
  
  SpacedPanel(Dimension spacing) {
    this(spacing, false);
  }
  
  SpacedPanel(Dimension spacing, boolean outerSpacing) {
    this(spacing, outerSpacing, true);
  }
  
  SpacedPanel(Dimension spacing, boolean outerSpacing, boolean spaceBefore) {
    this.spacing = spacing;
    this.outerSpacing = outerSpacing;
    panel = new FormattedPanel();
    if (outerSpacing && spaceBefore) {
      panel.add(Box.createRigidArea(spacing));
    }
  }
  
  Component add(JComponent comp) {
    return add(comp, true);
  }
  
  Component add(JComponent comp, boolean spaceAfter) {
    numComponents++;
    addSpacingBefore();
    Component component = panel.add(comp);
    addSpacingAfter(spaceAfter);
    return component;
  }
  
  Component add(SpacedPanel spacedPanel) {
    numComponents++;
    addSpacingBefore();
    Component component = panel.add(spacedPanel);
    addSpacingAfter(true);
    return component;
  }
  
  Component add(DoubleSpacedPanel doubleSpacedPanel) {
    numComponents++;
    addSpacingBefore();
    Component component = panel.add(doubleSpacedPanel);
    addSpacingAfter(true);
    return component;
  }
  
  Component add(LabeledTextField field) {
    numComponents++;
    addSpacingBefore();
    Component component = panel.add(field);
    addSpacingAfter(true);
    return component;
  }
  
  Component add(LabeledSpinner spinner) {
    numComponents++;
    addSpacingBefore();
    Component component = panel.add(spinner);
    addSpacingAfter(true);
    return component;
  }
  
  Component addMultiLineButton(AbstractButton button) {
    numComponents++;
    addSpacingBefore();
    Component component = panel.addMultiLineButton(button);
    addSpacingAfter(true);
    return component;
  }
  
  void addRigidArea() {
    panel.add(Box.createRigidArea(spacing));
  }
  
  private void addSpacingBefore() {
    if ((outerSpacing && numComponents == 1) || (!outerSpacing && numComponents > 1)) {
      panel.add(Box.createRigidArea(spacing));
    }
  }
  
  private void addSpacingAfter(boolean spaceAfter) {
    if (spaceAfter && outerSpacing) {
      panel.add(Box.createRigidArea(spacing));
    }
  }
  
  void removeAll() {
    numComponents = 0;
    panel.removeAll();
  }
  
  void setLayout(LayoutManager mgr) {
    panel.setLayout(mgr);
  }
  
  void setBorder(Border border) {
    panel.setBorder(border);
  }
  
  void setComponentAlignmentX(float alignmentX) {
    panel.setComponentAlignmentX(alignmentX);
  }
  
  public void resetComponentAlignmentX() {
    panel.resetComponentAlignmentX();
  }
  
  public Container getContainer() {
    return panel.getContainer();
  }
}
