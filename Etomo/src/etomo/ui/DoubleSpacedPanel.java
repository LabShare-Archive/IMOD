package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;

import javax.swing.AbstractButton;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.border.Border;

/**
* <p>Description: A box layout JPanel-like object that can either use X_AXIS or 
* Y_AXIS.  It places rigid areas outside the components on both the x axis and 
* the y axis.  It also places rigid areas between components.  It can be created
* with or without a border.  It is most useful for bordered panels, and some
* panels containing bordered panels.  It should work like a JPanel, except that
* layout and border are set during construction.</p>
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
* <p> Revision 1.1.2.5  2004/11/11 01:38:11  sueh
* <p> bug# 520 Removed unused functions add(Component) and addStrut.
* <p>
* <p> Revision 1.1.2.4  2004/11/08 22:24:13  sueh
* <p> bug# 520 Added addStrut() to add a strut in either direction.
* <p>
* <p> Revision 1.1.2.3  2004/10/13 23:01:48  sueh
* <p> bug# 520 Added a way to set and reset component alignments.  Added
* <p> specialized add() functions for generic etomo ui objects.  Added a way to
* <p> prevent spacing being added at the top of a panel and after a component.
* <p>
* <p> Revision 1.1.2.2  2004/09/23 23:52:12  sueh
* <p> bug# 520 Added a class description.
* <p>
* <p> Revision 1.1.2.1  2004/09/23 23:36:12  sueh
* <p> bug# 520 A panel which has rigid areas on both the X axis and Y axis.
* <p> Useful with borders and panels containing panels with borders.
* <p> </p>
*/
public class DoubleSpacedPanel {
  public static final String rcsid = "$Id$";

  private SpacedPanel innerPanel;
  private SpacedPanel outerPanel;
  private boolean xAxisLayout;

  public DoubleSpacedPanel(boolean xAxisLayout, Dimension xPixels,
      Dimension yPixels) {
    this(xAxisLayout, xPixels, yPixels, null);
  }
  
  public DoubleSpacedPanel(boolean xAxisLayout, Dimension xPixels,
      Dimension yPixels, Border border) {
    this(xAxisLayout, xPixels, yPixels, border, true);
  }
  
  public DoubleSpacedPanel(boolean xAxisLayout, Dimension xPixels,
      Dimension yPixels, Border border, boolean spaceTop) {
    this.xAxisLayout = xAxisLayout;
    if (xAxisLayout) {
      intialize(border, BoxLayout.X_AXIS, xPixels, true, BoxLayout.Y_AXIS, yPixels, spaceTop);
    }
    else {
      intialize(border, BoxLayout.Y_AXIS, yPixels, spaceTop, BoxLayout.X_AXIS, xPixels, true);
    }
  }
  
  private void intialize(Border border, int innerAxis, Dimension innerPixels,
      boolean innerSpaceBefore, int outerAxis, Dimension outerPixels, boolean outerSpaceBefore) {
    outerPanel = new SpacedPanel(outerPixels, true, outerSpaceBefore);
    innerPanel = new SpacedPanel(innerPixels, true, innerSpaceBefore);
    outerPanel.setLayout(new BoxLayout(outerPanel.getContainer(), outerAxis));
    if (border != null) {
      outerPanel.setBorder(border);
    }
    innerPanel.setLayout(new BoxLayout(innerPanel.getContainer(), innerAxis));
    outerPanel.add(innerPanel);
  }
  
  public void setComponentAlignmentX(float alignmentX) {
    innerPanel.setComponentAlignmentX(alignmentX);
  }
  
  public void resetComponentAlignmentX() {
    innerPanel.resetComponentAlignmentX();
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "\n,innerPanel=" + innerPanel + ",\nouterPanel=" + outerPanel;
  }
  
  public Component add(JComponent comp) {
    return innerPanel.add(comp);
  }
  
  public Component add(JComponent comp, boolean spaceAfter) {
    return innerPanel.add(comp, spaceAfter);
  }

  public Component add(DoubleSpacedPanel doubleSpacedPanel) {
    return innerPanel.add(doubleSpacedPanel);
  }

  public Component add(SpacedPanel spacedPanel) {
    return innerPanel.add(spacedPanel);
  }
  
  public Component add(LabeledTextField field) {
    return innerPanel.add(field);
  }
  
  public Component add(LabeledSpinner spinner) {
    return innerPanel.add(spinner);
  }
  
  public Component addMultiLineButton(AbstractButton button) {
    return innerPanel.addMultiLineButton(button);
  }
  
  public void removeAll() {
    innerPanel.removeAll();
  }

  public Container getContainer() {
    return outerPanel.getContainer();
  }
}