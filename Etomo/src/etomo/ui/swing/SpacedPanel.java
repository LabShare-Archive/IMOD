package etomo.ui.swing;

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
import javax.swing.border.TitledBorder;

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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.26  2009/06/05 02:18:32  sueh
 * <p> bug# 1219 Moved adding spacing to after the component is added.  Got
 * <p> rid of xDescription and yDescription which where for debugging.
 * <p>
 * <p> Revision 1.25  2009/01/20 20:29:05  sueh
 * <p> bug# 1102 Changed panel to type EtomoPanel so that it can name itself.
 * <p>
 * <p> Revision 1.24  2008/10/27 20:42:27  sueh
 * <p> bug# 1141 Added add(FioltTextField).
 * <p>
 * <p> Revision 1.23  2008/10/01 22:53:14  sueh
 * <p> bug# 1113 Added add(Component).
 * <p>
 * <p> Revision 1.22  2008/09/30 22:43:38  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.  Added
 * <p> FocusablePanel class.  Make outer panel a focusable panel when
 * <p> getFocusableInstance is called.
 * <p>
 * <p> Revision 1.21  2007/11/09 17:47:28  sueh
 * <p> bug# 1047 Added getComponent.
 * <p>
 * <p> Revision 1.20  2007/11/06 20:32:03  sueh
 * <p> bug# 1047 Added addRigidArea(Dimension).
 * <p>
 * <p> Revision 1.19  2007/05/26 00:33:23  sueh
 * <p> bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * <p> Setting button size in UI.
 * <p>
 * <p> Revision 1.18  2007/05/08 01:21:10  sueh
 * <p> bug# 964 Added setToolTipText().
 * <p>
 * <p> Revision 1.17  2007/05/01 00:45:48  sueh
 * <p> bug# 964 Added remove(SpacedPanel).
 * <p>
 * <p> Revision 1.16  2007/03/30 23:52:55  sueh
 * <p> bug# 964 Added add(etomo.ui.TextField)
 * <p>
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
  public static final String rcsid = "$Id$";

  private static final Dimension MULTI_LINE_BUTTON_DIM = UIParameters.INSTANCE
      .getButtonDimension();

  private final EtomoPanel panel = new EtomoPanel();
  private final JPanel innerPanel = new JPanel();
  private final JPanel outerPanel;

  private boolean layoutSet = false;
  private boolean previousComponentWasSpaced = false;
  private Float componentAlignmentX = null;

  private int axis;

  private static final class FocusablePanel extends JPanel {
    public FocusablePanel() {
      this.setFocusable(true);
    }
  }

  static SpacedPanel getInstance() {
    return new SpacedPanel(false, false);
  }

  static SpacedPanel getInstance(final boolean yAxisPadding) {
    return new SpacedPanel(yAxisPadding, false);
  }

  static SpacedPanel getFocusableInstance() {
    return new SpacedPanel(false, true);
  }

  static SpacedPanel getFocusableInstance(final boolean yAxisPadding) {
    return new SpacedPanel(yAxisPadding, true);
  }

  /**
   * 
   * @param yAxisPadding - add a rigid area at the top of the panel
   */
  private SpacedPanel(final boolean yAxisPadding, final boolean focusable) {
    //panels
    if (focusable) {
      outerPanel = new FocusablePanel();
    }
    else {
      outerPanel = new JPanel();
    }
    outerPanel.setLayout(new BoxLayout(outerPanel, BoxLayout.Y_AXIS));
    innerPanel.setLayout(new BoxLayout(innerPanel, BoxLayout.X_AXIS));
    //innerPanel
    innerPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    innerPanel.add(panel);
    innerPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    //outerPanel
    if (yAxisPadding) {
      outerPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    }
    outerPanel.add(innerPanel);
    outerPanel.add(Box.createRigidArea(FixedDim.x0_y5));
  }

  void requestFocus() {
    panel.requestFocus();
  }

  final void setBoxLayout(final int axis) {
    this.axis = axis;
    panel.setLayout(new BoxLayout(panel, axis));
    layoutSet = true;
  }

  private final void addSpacing() {
    if (!layoutSet) {
      return;
    }
    if (axis == BoxLayout.X_AXIS) {
      panel.add(Box.createRigidArea(FixedDim.x5_y0));
    }
    else if (axis == BoxLayout.Y_AXIS && !previousComponentWasSpaced) {
      panel.add(Box.createRigidArea(FixedDim.x0_y5));
    }
    if (previousComponentWasSpaced) {
      previousComponentWasSpaced = false;
    }
  }

  void remove(final SpacedPanel panel) {
    this.panel.remove(panel.getContainer());
  }

  final void add(final Container container) {
    panel.add(container);
    addSpacing();
  }

  final void add(final Component component) {
    panel.add(component);
    addSpacing();
  }

  final void add(final SpacedPanel spacedPanel) {
    panel.add(spacedPanel.getContainer());
    addSpacing();
    if (componentAlignmentX != null) {
      spacedPanel.setComponentAlignmentX(componentAlignmentX.floatValue());
    }
    previousComponentWasSpaced = true;
  }

  final void add(final JScrollPane jScrollPane) {
    panel.add(jScrollPane);
    addSpacing();
    if (componentAlignmentX != null) {
      jScrollPane.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  final void add(final JPanel jPanel) {
    panel.add(jPanel);
    addSpacing();
    if (componentAlignmentX != null) {
      jPanel.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  final void add(final JComboBox jComboBox) {
    panel.add(jComboBox);
    addSpacing();
    if (componentAlignmentX != null) {
      jComboBox.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final FileTextField fileTextField) {
    panel.add(fileTextField.getContainer());
    addSpacing();
    if (componentAlignmentX != null) {
      fileTextField.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final LabeledTextField labeledTextField) {
    panel.add(labeledTextField.getContainer());
    addSpacing();
    if (componentAlignmentX != null) {
      labeledTextField.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final Spinner spinner) {
    panel.add(spinner.getContainer());
    addSpacing();
    if (componentAlignmentX != null) {
      spinner.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final TextField textField) {
    panel.add(textField.getComponent());
    addSpacing();
    if (componentAlignmentX != null) {
      textField.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final JLabel jLabel) {
    panel.add(jLabel);
    addSpacing();
    if (componentAlignmentX != null) {
      jLabel.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final RadioButton radioButton) {
    //  addSpacing();
    panel.add(radioButton.getComponent());
    addSpacing();
    if (componentAlignmentX != null) {
      radioButton.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final CheckBox checkBox) {
    // addSpacing();
    panel.add(checkBox);
    addSpacing();
    if (componentAlignmentX != null) {
      checkBox.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final SpacedTextField spacedTextField) {
    // addSpacing();
    panel.add(spacedTextField.getContainer());
    addSpacing();
    if (componentAlignmentX != null) {
      spacedTextField.setAlignmentX(componentAlignmentX.floatValue());
    }
    previousComponentWasSpaced = true;
  }

  void add(final PanelHeader panelHeader) {
    // addSpacing();
    panel.add(panelHeader);
    addSpacing();
  }

  void add(final LabeledSpinner labeledSpinner) {
    //addSpacing();
    panel.add(labeledSpinner.getContainer());
    addSpacing();
    if (componentAlignmentX != null) {
      labeledSpinner.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final MultiLineButton multiLineButton) {
    // addSpacing();
    //multiLineButton.setSize();
    panel.add(multiLineButton.getComponent());
    addSpacing();
    if (componentAlignmentX != null) {
      multiLineButton.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void add(final JButton button) {
    // addSpacing();
    panel.add(button);
    addSpacing();
    if (componentAlignmentX != null) {
      button.setAlignmentX(componentAlignmentX.floatValue());
    }
  }

  void addMouseListener(final MouseListener mouseListener) {
    panel.addMouseListener(mouseListener);
  }

  void addHorizontalGlue() {
    panel.add(Box.createHorizontalGlue());
  }

  void addRigidArea() {
    if (axis == BoxLayout.X_AXIS) {
      panel.add(Box.createRigidArea(FixedDim.x5_y0));
    }
    else if (axis == BoxLayout.Y_AXIS) {
      panel.add(Box.createRigidArea(FixedDim.x0_y5));
    }
  }

  void addRigidArea(Dimension dim) {
    if (axis == BoxLayout.X_AXIS) {
      panel.add(Box.createRigidArea(dim));
    }
    else if (axis == BoxLayout.Y_AXIS) {
      panel.add(Box.createRigidArea(dim));
    }
  }

  void removeAll() {
    panel.removeAll();
  }

  void setComponentAlignmentX(final float componentAlignmentX) {
    this.componentAlignmentX = new Float(componentAlignmentX);
  }

  void alignComponentsX(final float alignment) {
    setComponentAlignmentX(alignment);
    alignComponentsX(outerPanel, alignment);
    alignComponentsX(innerPanel, alignment);
    alignComponentsX(panel, alignment);
  }

  void setToolTipText(final String text) {
    panel.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  private final void alignComponentsX(final JPanel panel, final float alignment) {
    Component[] children = panel.getComponents();
    for (int i = 0; i < children.length; i++) {
      if (children[i] instanceof JComponent) {
        JComponent jcomp = (JComponent) children[i];
        jcomp.setAlignmentX(alignment);
      }
    }
  }

  Container getContainer() {
    return outerPanel;
  }

  JPanel getJPanel() {
    return (JPanel) getContainer();
  }

  String getName() {
    return panel.getName();
  }

  void setName(String name) {
    panel.setName(name);
  }

  void setAlignmentX(final float alignmentX) {
    outerPanel.setAlignmentX(alignmentX);
  }

  void setBorder(final Border border) {
    panel.setBorder(border);
  }

  void setBorder(final TitledBorder border) {
    panel.setBorder(border);
  }

  void setVisible(final boolean visible) {
    getContainer().setVisible(visible);
  }
}
