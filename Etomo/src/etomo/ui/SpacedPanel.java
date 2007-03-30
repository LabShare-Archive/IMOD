package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.MouseListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
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
* <p> Revision 1.15  2007/03/07 21:14:36  sueh
* <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
* <p> because it is getting more complicated.
* <p>
* <p> Revision 1.14  2007/02/05 23:44:33  sueh
* <p> bug# 962 Fixing alignment setting.
* <p>
* <p> Revision 1.13  2006/07/20 17:22:20  sueh
* <p> bug# 848 Made UIParameters a singleton.
* <p>
* <p> Revision 1.12  2006/03/27 21:07:33  sueh
* <p> bug# 836 Added addMouseListener().
* <p>
* <p> Revision 1.11  2006/01/03 23:54:45  sueh
* <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted JRadioButton's
* <p> toRadioButton.
* <p>
* <p> Revision 1.10  2005/09/29 19:11:14  sueh
* <p> add comment
* <p>
* <p> Revision 1.9  2005/08/10 20:47:49  sueh
* <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
* <p> of MultiLineButton.
* <p>
* <p> Revision 1.8  2005/08/09 21:09:55  sueh
* <p> bug# 711 Moving button sizing from UIUtilities to the multi line button
* <p> classes.
* <p>
* <p> Revision 1.7  2005/07/11 23:23:46  sueh
* <p> bug# 619 Added add(container).
* <p>
* <p> Revision 1.6  2005/07/06 23:49:19  sueh
* <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
* <p> their functionality in SpacedPanel.  Simplified the construction of
* <p> SpacedPanel.
* <p>
* <p> Revision 1.5  2005/07/01 21:23:27  sueh
* <p> bug# 619 Added getRootPanel() to return the JPanel in panel.
* <p>
* <p> Revision 1.4  2004/12/30 19:32:42  sueh
* <p> bug# 567 Added setAlignmentX() to set alignment for the panel.
* <p>
* <p> Revision 1.3  2004/12/01 03:48:46  sueh
* <p> bug# 557 Added add option to space with glue instead of rigid areas.
* <p> Added an option to automatically use 5 pixels when spacing with rigid
* <p> areas.
* <p>
* <p> Revision 1.2  2004/11/20 00:04:29  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
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
final class SpacedPanel {
  public static  final String  rcsid =  "$Id$";
  
  private static final Dimension MULTI_LINE_BUTTON_DIM = UIParameters.INSTANCE.getButtonDimension();
  
  private JPanel panel;
  private JPanel innerPanel = null;
  private JPanel outerPanel = null;
  private boolean componentAdded = false;
  private boolean layoutSet = false;
  private boolean previousComponentWasSpaced = false;
  private int axis;
  private Float componentAlignmentX = null;
  private StringBuffer xDescription = new StringBuffer();
  private StringBuffer yDescription = new StringBuffer();
  
  SpacedPanel() {
    this(false);
  }
  
  /**
   * 
   * @param yAxisPadding - add a rigid area at the top of the panel
   */
  SpacedPanel(boolean yAxisPadding) {
    //panels
    outerPanel = new JPanel();
    outerPanel.setLayout(new BoxLayout(outerPanel, BoxLayout.Y_AXIS));
    innerPanel = new JPanel();
    innerPanel.setLayout(new BoxLayout(innerPanel, BoxLayout.X_AXIS));
    panel = new JPanel();
    //innerPanel
    innerPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    xDescription.append("first FixedDim.x5_y0,");
    innerPanel.add(panel);
    innerPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    xDescription.append("last FixedDim.x5_y0,");
    //outerPanel
    if (yAxisPadding) {
      outerPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    }
    outerPanel.add(innerPanel);
    outerPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    yDescription.append("last FixedDim.x0_y5,");
  }
  
  public String toString() {
    return ",axis="+axis+"\n,xDescription="+xDescription+"\n,yDescription="+yDescription;
  }
  
  final void setBoxLayout(int axis) {
    this.axis = axis;
    panel.setLayout(new BoxLayout(panel, axis));
    layoutSet = true;
  }
  
  private final void addSpacing() {
    if (!componentAdded) {
      componentAdded = true;
      return;
    }
    if (!layoutSet) {
      return;
    }
    if (axis == BoxLayout.X_AXIS) {
      panel.add(Box.createRigidArea(FixedDim.x5_y0));
      xDescription.append("FixedDim.x5_y0,");
    }
    else if (axis == BoxLayout.Y_AXIS && !previousComponentWasSpaced) {
      panel.add(Box.createRigidArea(FixedDim.x0_y5));
      yDescription.append("FixedDim.x0_y5,");
    }
    if (previousComponentWasSpaced) {
      previousComponentWasSpaced = false;
    }
  }
  
  final void add(Container container) {
    addSpacing();
    panel.add(container);
    xDescription.append("Container,");
    yDescription.append("Container,");
  }
  
  final void add(SpacedPanel spacedPanel) {
    addSpacing();
    panel.add(spacedPanel.getContainer());
    if (componentAlignmentX != null) {
      spacedPanel.setComponentAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("SpacedPanel,");
    yDescription.append("SpacedPanel,");
    previousComponentWasSpaced = true;
    
  }
  
  final void add(JScrollPane jScrollPane) {
    addSpacing();
    panel.add(jScrollPane);
    if (componentAlignmentX != null) {
      jScrollPane.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("JScrollPane,");
    yDescription.append("JScrollPane,");
  }
  
  final void add(JPanel jPanel) {
    addSpacing();
    panel.add(jPanel);
    if (componentAlignmentX != null) {
      jPanel.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("JPanel,");
    yDescription.append("JPanel,");
  }
  
  final void add(JComboBox jComboBox) {
    addSpacing();
    panel.add(jComboBox);
    if (componentAlignmentX != null) {
      jComboBox.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("JComboBox,");
    yDescription.append("JComboBox,");
  }

  final void add(LabeledTextField labeledTextField) {
    addSpacing();
    panel.add(labeledTextField.getContainer());
    if (componentAlignmentX != null) {
      labeledTextField.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("LabeledTextField,");
    yDescription.append("LabeledTextField,");
  }
  
  final void add(TextField textField) {
    addSpacing();
    panel.add(textField.getComponent());
    if (componentAlignmentX != null) {
      textField.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("LabeledTextField,");
    yDescription.append("LabeledTextField,");
  }
  
  final void add(JLabel jLabel) {
    addSpacing();
    panel.add(jLabel);
    if (componentAlignmentX != null) {
      jLabel.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("JLabel,");
    yDescription.append("JLabel,");
  }
  
  final void add(RadioButton radioButton) {
    addSpacing();
    panel.add(radioButton.getComponent());
    if (componentAlignmentX != null) {
      radioButton.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("RadioButton,");
    yDescription.append("RadioButton,");
  }
  
  final void add(CheckBox checkBox) {
    addSpacing();
    panel.add(checkBox);
    if (componentAlignmentX != null) {
      checkBox.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("CheckBox,");
    yDescription.append("CheckBox,");
  }
  
  final void add(SpacedTextField spacedTextField) {
    addSpacing();
    panel.add(spacedTextField.getContainer());
    if (componentAlignmentX != null) {
      spacedTextField.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("SpacedTextField,");
    yDescription.append("SpacedTextField,");
    previousComponentWasSpaced = true;
  }
  
  final void add(PanelHeader panelHeader) {
    addSpacing();
    panel.add(panelHeader.getContainer());
    xDescription.append("PanelHeader,");
    yDescription.append("PanelHeader,");
  }
  
  final void add(LabeledSpinner labeledSpinner) {
    addSpacing();
    panel.add(labeledSpinner.getContainer());
    if (componentAlignmentX != null) {
      labeledSpinner.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("LabeledSpinner,");
    yDescription.append("LabeledSpinner,");
  }
  
  final void add(MultiLineButton multiLineButton) {
    addSpacing();
    multiLineButton.setSize();
    panel.add(multiLineButton.getComponent());
    if (componentAlignmentX != null) {
      multiLineButton.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("MultiLineButton,");
    yDescription.append("MultiLineButton,");
  }
  
  final void add(JButton button) {
    addSpacing();
    panel.add(button);
    if (componentAlignmentX != null) {
      button.setAlignmentX(componentAlignmentX.floatValue());
    }
    xDescription.append("JButton,");
    yDescription.append("JButton,");
  }
  
  final void addMouseListener(MouseListener mouseListener) {
    panel.addMouseListener(mouseListener);
  }
  
  final void addHorizontalGlue() {
    panel.add(Box.createHorizontalGlue());
    xDescription.append("HorizontalGlue,");
  }
  
  final void addRigidArea() {
    if (axis == BoxLayout.X_AXIS) {
      panel.add(Box.createRigidArea(FixedDim.x5_y0));
      xDescription.append("FixedDim.x5_y0,");
    }
    else if (axis == BoxLayout.Y_AXIS) {
      panel.add(Box.createRigidArea(FixedDim.x0_y5));
      yDescription.append("FixedDim.x0_y5,");
    }
  }
  
  final void removeAll() {
    componentAdded = false;
    panel.removeAll();
  }
  
  final void setComponentAlignmentX(float componentAlignmentX) {
    this.componentAlignmentX = new Float(componentAlignmentX);
  }
  
  final void alignComponentsX(float alignment) {
    setComponentAlignmentX(alignment);
    alignComponentsX(outerPanel, alignment);
    alignComponentsX(innerPanel, alignment);
    alignComponentsX(panel, alignment);
  }
  
  private final void alignComponentsX(JPanel panel, float alignment) {
    if (panel == null) {
      return;
    }
    Component[] children = panel.getComponents();
    for (int i = 0; i < children.length; i++) {
      if (children[i] instanceof JComponent) {
        JComponent jcomp = (JComponent) children[i];
        jcomp.setAlignmentX(alignment);
      }
    }
  }


  
  final Container getContainer() {
    if (outerPanel != null) {
      return outerPanel;
    }
    if (innerPanel != null) {
      return innerPanel;
    }
    return panel;
  }
  
  final JPanel getJPanel() {
    return (JPanel) getContainer();
  }
  
  final void setAlignmentX(float alignmentX) {
    if (outerPanel != null) {
      outerPanel.setAlignmentX(alignmentX);
    }
    if (innerPanel != null) {
      innerPanel.setAlignmentX(alignmentX);
    }
    panel.setAlignmentX(alignmentX);
  }
  
  final void setBorder(Border border) {
    panel.setBorder(border);
  }
  
  final void setVisible(boolean visible) {
    getContainer().setVisible(visible);
  }
}
