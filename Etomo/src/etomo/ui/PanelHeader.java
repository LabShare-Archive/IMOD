package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSeparator;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
final class PanelHeader implements Expandable {
  public static  final String  rcsid =  "$Id$";
  
  private GridBagLayout layout = new GridBagLayout();
  private GridBagConstraints constraints = new GridBagConstraints();
  
  private JPanel rootPanel = new JPanel();
  private HeaderCell cellTitle = null;
  private ExpandButton btnOpenClose = null;
  private ExpandButton btnAdvancedBasic = null;
  private JSeparator separator = new JSeparator();
  private JPanel openClosePanel = null;
  private final BaseManager manager;
  private AxisID axisID;
  
  PanelHeader(BaseManager manager, AxisID axisID, String title, SpacedPanel openClosePanel) {
    this(manager, axisID, title, openClosePanel.getJPanel(), null);
  }
  
  PanelHeader(BaseManager manager, AxisID axisID, String title, SpacedPanel openClosePanel, Expandable container) {
    this(manager, axisID, title, openClosePanel.getJPanel(), container);
  }
  
  PanelHeader(BaseManager manager, AxisID axisID, String title, JPanel openClosePanel, Expandable container) {
    this.manager = manager;
    this.openClosePanel = openClosePanel;
    this.axisID = axisID;
    //panels
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    JPanel northPanel = new JPanel();
    northPanel.setLayout(layout);
    //northPanel
    constraints.fill = GridBagConstraints.BOTH;
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    //open/close button
    if (openClosePanel != null) {
      btnOpenClose = new ExpandButton(this, "-", "+");
      layout.setConstraints(btnOpenClose, constraints);
      northPanel.add(btnOpenClose);
    }
    //title
    if (container == null) {
      constraints.gridwidth = GridBagConstraints.REMAINDER;
    }
    constraints.weightx = 1.0;
    constraints.weighty = 1.0;
    cellTitle = new HeaderCell(title);
    cellTitle.setBorderPainted(false);
    cellTitle.add(northPanel, layout, constraints);
    //advanced/basic button
    if (container != null) {
      constraints.weightx = 0.0;
      constraints.weighty = 0.0;
      constraints.gridwidth = GridBagConstraints.REMAINDER;
      btnAdvancedBasic = new ExpandButton(container, "B", "A");
      layout.setConstraints(btnAdvancedBasic, constraints);
      northPanel.add(btnAdvancedBasic);
    }
    //rootPanel
    rootPanel.add(northPanel);
    separator.setMaximumSize(new Dimension(100, 1));
    separator.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(separator);
    if (!EtomoDirector.getInstance().isNewstuff()) {
      if (btnOpenClose != null) {
        btnOpenClose.setVisible(false);
      }
      if (btnAdvancedBasic != null) {
        btnAdvancedBasic.setVisible(false);
      }
    } 
  }
  
  final Container getContainer() {
    return rootPanel;
  }
  
  final boolean equalsAdvancedBasic(ExpandButton button) {
    return button == btnAdvancedBasic;
  }
  
  final void setAdvanced(boolean advanced) {
    if (btnAdvancedBasic == null) {
      return;
    }
    btnAdvancedBasic.setExpanded(advanced);
  }
  
  final void setOpen(boolean open) {
    if (btnOpenClose == null) {
      return;
    }
    btnOpenClose.setExpanded(open);
  }
  
  public final void expand(ExpandButton button) {
    if (openClosePanel == null || button != btnOpenClose) {
      return;
    }
    openClosePanel.setVisible(btnOpenClose.isExpanded());
    separator.setVisible(btnOpenClose.isExpanded());
    UIHarness.INSTANCE.pack(axisID, manager);
  }  
}
/**
* <p> $Log$
* <p> Revision 1.5  2005/07/29 00:54:23  sueh
* <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
* <p> because the current manager changes when the user changes the tab.
* <p> Passing the manager where its needed.
* <p>
* <p> Revision 1.4  2005/07/19 22:33:43  sueh
* <p> bug# 532 creating the buttons and hiding them when new stuff is true
* <p>
* <p> Revision 1.3  2005/07/11 23:04:59  sueh
* <p> bug# 619 Attempting to center the separator.
* <p>
* <p> Revision 1.2  2005/07/07 00:00:04  sueh
* <p> bug# 437 Removed unnecessary Constructor parameter
* <p> useAdvancedBasic.  If the container is not passed then the
* <p> advanced/basic button with not be used.
* <p>
* <p> Revision 1.1  2005/07/06 23:45:23  sueh
* <p> bug# 437 Class to place a header panel on a JPanel.  The header panel
* <p> uses the grid bag layout.  It can contain two buttons:  an open/close
* <p> button whose functionality is encapsulated, and an advanced/basic
* <p> button whose functionality is handled by a container which implements
* <p> Expandable.  PanelHeader implements expandable for the open/close
* <p> functionality.  Pass the panel to be made visible/invisible in the
* <p> constructor for the open/close button to be displayed.  Construct the
* <p> class with useAdvancedBasic to have an advanced/basic button
* <p> displayed.  Since this class continues to function after it is placed in the
* <p> dialog, it must not be sent to garbage collection until the dialog is gone.
* <p> </p>
*/