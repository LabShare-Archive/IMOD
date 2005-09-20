package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSeparator;

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
  private ExpandButton btnMoreLess = null;
  private JSeparator separator = new JSeparator();
  private AxisID axisID;
  private final Expandable container;
  
  static final PanelHeader getInstance(AxisID axisID, String title,
      Expandable container) {
    return new PanelHeader(axisID, title, container, false, false, false);
  }

  static final PanelHeader getAdvancedBasicInstance(AxisID axisID,
      String title, Expandable container) {
    return new PanelHeader(axisID, title, container, true, false, false);
  }

  static final PanelHeader getExpandedMoreLessInstance(AxisID axisID,
      String title, Expandable container) {
    return new PanelHeader(axisID, title, container, false, true, true);
  }

  private PanelHeader(AxisID axisID, String title, Expandable container,
      boolean advancedBasic, boolean moreLess, boolean expanded) {
    this.container = container;
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
    btnOpenClose = ExpandButton.getExpandedInstance(this, "-", "+");
    layout.setConstraints(btnOpenClose.getComponent(), constraints);
    northPanel.add(btnOpenClose.getComponent());
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
    if (advancedBasic) {
      constraints.weightx = 0.0;
      constraints.weighty = 0.0;
      if (!moreLess) {
        constraints.gridwidth = GridBagConstraints.REMAINDER;
      }
      if (expanded) {
        btnAdvancedBasic = ExpandButton.getExpandedInstance(container, "B", "A");
      }
      else {
        btnAdvancedBasic = ExpandButton.getInstance(container, "B", "A");
      }
      layout.setConstraints(btnAdvancedBasic.getComponent(), constraints);
      northPanel.add(btnAdvancedBasic.getComponent());
    }
    //more/less button
    if (moreLess) {
      constraints.weightx = 0.0;
      constraints.weighty = 0.0;
      if (expanded) {
        btnMoreLess = ExpandButton.getExpandedMoreLessInstance(container);
      }
      else {
        btnMoreLess = ExpandButton.getMoreLessInstance(container);
      }
      layout.setConstraints(btnMoreLess.getComponent(), constraints);
      northPanel.add(btnMoreLess.getComponent());
    }
    //rootPanel
    rootPanel.add(northPanel);
    separator.setMaximumSize(new Dimension(100, 1));
    separator.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(separator);
  }
  
  final Container getContainer() {
    return rootPanel;
  }
  
  final boolean equalsOpenClose(ExpandButton button) {
    return button == btnOpenClose;
  }
  
  final boolean equalsAdvancedBasic(ExpandButton button) {
    return button == btnAdvancedBasic;
  }
  
  final boolean equalsMoreLess(ExpandButton button) {
    return button == btnMoreLess;
  }
  
  final void setAdvanced(boolean advanced) {
    if (btnAdvancedBasic == null) {
      return;
    }
    btnAdvancedBasic.setExpanded(advanced);
  }
  
  public final void expand(ExpandButton button) {
    if (button == btnOpenClose) {
      separator.setVisible(button.isExpanded());
      container.expand(button);
    }
  } 
}
/**
* <p> $Log$
* <p> Revision 1.10  2005/09/19 16:38:57  sueh
* <p> bug# 532 Added more/less button.
* <p>
* <p> Revision 1.9  2005/08/30 19:20:15  sueh
* <p> bug# 437 Remove newstuff limit from panel headers.
* <p>
* <p> Revision 1.8  2005/08/22 18:00:37  sueh
* <p> bug# 532 Remove openClosePanel.  When handling the open/close
* <p> button, call container.expand() to do the expansion.
* <p>
* <p> Revision 1.7  2005/08/09 20:26:42  sueh
* <p> bug# 711  No longer inheriting JButton in MultiLineButton.
* <p>
* <p> Revision 1.6  2005/08/04 20:12:54  sueh
* <p> bug# 532  Centralizing fit window functionality by placing fitting functions
* <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
* <p> the manager to UIHarness.pack() so that packDialogs() can be called.
* <p>
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