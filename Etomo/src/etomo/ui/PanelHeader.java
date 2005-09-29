package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSeparator;

import etomo.type.PanelHeaderState;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
final class PanelHeader implements Expandable {
  public static final String rcsid = "$Id$";

  private GridBagLayout layout = new GridBagLayout();
  private GridBagConstraints constraints = new GridBagConstraints();

  private final JPanel rootPanel = new JPanel();
  private final JSeparator separator = new JSeparator();

  private final Expandable panel;
  private final ExpandButton btnOpenClose;
  private final HeaderCell cellTitle;

  private final String group;

  private ExpandButton btnAdvancedBasic = null;
  private ExpandButton btnMoreLess = null;

  static final PanelHeader getInstance(String group, String title,
      Expandable panel) {
    return new PanelHeader(group, title, panel, false, false);
  }

  static final PanelHeader getAdvancedBasicInstance(String group, String title,
      Expandable panel) {
    return new PanelHeader(group, title, panel, true, false);
  }

  static final PanelHeader getMoreLessInstance(String group, String title,
      Expandable panel) {
    return new PanelHeader(group, title, panel, false, true);
  }

  /**
   * creates a panel header
   * default creates only the open/close button
   * the open/close button's initial state is always open
   * 
   * @param title - original header string, also used to store state - required
   * @param panel - expandable panel this instances is the header of - required
   * @param advancedBasic - true if an advance/basic button should be created
   * @param moreLess - true if an more/less button should be created
   */
  private PanelHeader(String group, String title, Expandable panel,
      boolean advancedBasic, boolean moreLess) {
    this.group = group;
    this.panel = panel;
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
    //open/close button - default: open
    btnOpenClose = ExpandButton.getInstance(this, "-", "+", "closed", "open", true);
    layout.setConstraints(btnOpenClose.getComponent(), constraints);
    northPanel.add(btnOpenClose.getComponent());
    //title
    constraints.weightx = 1.0;
    constraints.weighty = 1.0;
    cellTitle = new HeaderCell(title);
    cellTitle.setBorderPainted(false);
    cellTitle.add(northPanel, layout, constraints);
    boolean expanded;
    //advanced/basic button - default: basic
    if (advancedBasic) {
      constraints.weightx = 0.0;
      constraints.weighty = 0.0;
      if (!moreLess) {
        constraints.gridwidth = GridBagConstraints.REMAINDER;
      }
      btnAdvancedBasic = ExpandButton.getInstance(panel, "B", "A", "basic",
          "advanced");
      layout.setConstraints(btnAdvancedBasic.getComponent(), constraints);
      northPanel.add(btnAdvancedBasic.getComponent());
    }
    //more/less button - default: more
    if (moreLess) {
      constraints.weightx = 0.0;
      constraints.weighty = 0.0;
      btnMoreLess = ExpandButton.getMoreLessInstance(panel, true);
      layout.setConstraints(btnMoreLess.getComponent(), constraints);
      northPanel.add(btnMoreLess.getComponent());
    }
    //rootPanel
    rootPanel.add(northPanel);
    separator.setMaximumSize(new Dimension(100, 1));
    separator.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(separator);
  }

  final void setText(String text) {
    cellTitle.setText(text);
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
  
  final boolean isAdvancedBasicExpanded() {
    if (btnAdvancedBasic == null) {
      return false;
    }
    return btnAdvancedBasic.isExpanded();
  }

  public final void expand(ExpandButton button) {
    if (button == btnOpenClose) {
      separator.setVisible(button.isExpanded());
      panel.expand(button);
    }
  }

  public final void getState(PanelHeaderState state) {
    state.setOpenCloseState(btnOpenClose.getState());
    if (btnAdvancedBasic != null) {
      state.setAdvancedBasicState(btnAdvancedBasic.getState());
    }
    if (btnMoreLess != null) {
      state.setMoreLessState(btnMoreLess.getState());
    }
  }

  /**
   * Change the state of the buttons, which causes calls to expanded() for each
   * button for which there is a valid state.
   * @param state
   */
  public final void setState(PanelHeaderState state) {
    if (state == null) {
      return;
    }
    btnOpenClose.setState(state.getOpenCloseState());
    if (btnAdvancedBasic != null) {
      btnAdvancedBasic.setState(state.getAdvancedBasicState());
    }
    if (btnMoreLess != null) {
      btnMoreLess.setState(state.getMoreLessState());
    }
  }
}
/**
* <p> $Log$
* <p> Revision 1.13  2005/09/27 23:33:09  sueh
* <p> bug# 532 Simplified PanelHeader and added a way to get and set the
* <p> state.  Removed axisID because it was not being used.
* <p>
* <p> Revision 1.12  2005/09/21 16:45:01  sueh
* <p> bug# 532 Added setText() to change the header's title.
* <p>
* <p> Revision 1.11  2005/09/20 19:00:18  sueh
* <p> bug# 532 Allowing control of whether buttons start in expanded or
* <p> contracted state.  OpenClose is always expanded.  The other can be
* <p> controlled.  Adeed getExpandedMoreLessInstance() to get a header with
* <p> a more/less button with an expanded initial state.  The default is contracted.
* <p>
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