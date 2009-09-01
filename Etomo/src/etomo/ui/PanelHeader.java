package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSeparator;

import etomo.type.BaseScreenState;
import etomo.type.ConstPanelHeaderState;
import etomo.type.DialogType;
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

  private final HeaderCell cellTitle;

  private final DialogType dialogType;
  private final String title;

  private ExpandButton btnOpenClose = null;
  private ExpandButton btnAdvancedBasic = null;
  private ExpandButton btnMoreLess = null;

  private final GlobalExpandButton globalAdvancedButton;

  static PanelHeader getInstance(String title, Expandable expandable,
      DialogType dialogType) {
    return new PanelHeader(title, expandable, false, false, dialogType, true,
        null);
  }

  static PanelHeader getAdvancedBasicInstance(String title,
      Expandable expandable, DialogType dialogType) {
    return new PanelHeader(title, expandable, true, false, dialogType, true,
        null);
  }

  static PanelHeader getAdvancedBasicInstance(String title,
      Expandable expandable, DialogType dialogType,
      GlobalExpandButton globalAdvancedButton) {
    return new PanelHeader(title, expandable, true, false, dialogType, true,
        globalAdvancedButton);
  }

  static PanelHeader getAdvancedBasicOnlyInstance(String title,
      Expandable expandable, DialogType dialogType,
      GlobalExpandButton globalAdvancedButton) {
    return new PanelHeader(title, expandable, true, false, dialogType, false,
        globalAdvancedButton);
  }

  static PanelHeader getMoreLessInstance(String title, Expandable expandable,
      DialogType dialogType) {
    return new PanelHeader(title, expandable, false, true, dialogType, true,
        null);
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
  private PanelHeader(String title, Expandable expandable,
      boolean advancedBasic, boolean moreLess, DialogType dialogType,
      boolean openClose, GlobalExpandButton globalAdvancedButton) {
    this.dialogType = dialogType;
    this.title = title;
    this.globalAdvancedButton = globalAdvancedButton;
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
    if (openClose) {
      //open/close button - default: open
      btnOpenClose = ExpandButton.getExpandedInstance(expandable, this,
          ExpandButton.Type.OPEN);
      btnOpenClose.setName(title);
      layout.setConstraints(btnOpenClose.getComponent(), constraints);
      northPanel.add(btnOpenClose.getComponent());
    }
    //title
    constraints.weightx = 1.0;
    constraints.weighty = 1.0;
    cellTitle = new HeaderCell(title, false);
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
      btnAdvancedBasic = ExpandButton.getGlobalInstance(expandable,
          ExpandButton.Type.ADVANCED, globalAdvancedButton);
      btnAdvancedBasic.setName(title);
      layout.setConstraints(btnAdvancedBasic.getComponent(), constraints);
      northPanel.add(btnAdvancedBasic.getComponent());
    }
    //more/less button - default: more
    if (moreLess) {
      constraints.weightx = 0.0;
      constraints.weighty = 0.0;
      btnMoreLess = ExpandButton.getExpandedInstance(expandable, null,
          ExpandButton.Type.MORE);
      btnMoreLess.setName(title);
      layout.setConstraints(btnMoreLess.getComponent(), constraints);
      northPanel.add(btnMoreLess.getComponent());
    }
    //rootPanel
    rootPanel.add(northPanel);
    separator.setMaximumSize(new Dimension(100, 1));
    separator.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(separator);
  }

  void setText(String text) {
    cellTitle.setText(text);
  }

  Container getContainer() {
    return rootPanel;
  }

  boolean equalsOpenClose(ExpandButton button) {
    return button == btnOpenClose;
  }

  boolean equalsAdvancedBasic(ExpandButton button) {
    return button == btnAdvancedBasic;
  }

  boolean equalsMoreLess(ExpandButton button) {
    return button == btnMoreLess;
  }

  void setAdvanced(boolean advanced) {
    if (btnAdvancedBasic == null) {
      return;
    }
    btnAdvancedBasic.setExpanded(advanced);
  }

  void setOpen(boolean open) {
    if (btnOpenClose == null) {
      return;
    }
    btnOpenClose.setExpanded(open);
  }

  String getTitle() {
    return title;
  }

  boolean isAdvanced() {
    if (btnAdvancedBasic == null) {
      return false;
    }
    return btnAdvancedBasic.isExpanded();
  }

  public void expand(GlobalExpandButton button) {
  }

  /**
   * Hide the separator when expand is called on an open/close button
   */
  public void expand(ExpandButton button) {
    if (button == btnOpenClose) {
      separator.setVisible(button.isExpanded());
    }
  }

  public void getState(PanelHeaderState state) {
    if (state == null) {
      return;
    }
    if (btnOpenClose != null) {
      state.setOpenCloseState(btnOpenClose.getState());
    }
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
  public void setState(ConstPanelHeaderState state) {
    if (state == null) {
      return;
    }
    if (btnOpenClose != null) {
      btnOpenClose.setState(state.getOpenCloseState());
    }
    if (btnAdvancedBasic != null) {
      btnAdvancedBasic.setState(state.getAdvancedBasicState());
    }
    if (btnMoreLess != null) {
      btnMoreLess.setState(state.getMoreLessState());
    }
  }

  public void setButtonStates(BaseScreenState screenState) {
    setButtonStates(screenState, true);
  }

  public void setButtonStates(BaseScreenState screenState, boolean defaultIsOpen) {
    if (screenState == null) {
      return;
    }
    if (btnOpenClose != null) {
      btnOpenClose.setButtonState(screenState.getButtonState(btnOpenClose
          .createButtonStateKey(dialogType), defaultIsOpen));
    }
    if (btnAdvancedBasic != null) {
      btnAdvancedBasic.setButtonState(screenState
          .getButtonState(btnAdvancedBasic.createButtonStateKey(dialogType)));
    }
    if (btnMoreLess != null) {
      btnMoreLess.setButtonState(screenState.getButtonState(btnMoreLess
          .createButtonStateKey(dialogType)));
    }
  }

  public void getButtonStates(BaseScreenState screenState) {
    if (screenState == null) {
      return;
    }
    if (btnOpenClose != null) {
      screenState.setButtonState(btnOpenClose.getButtonStateKey(), btnOpenClose
          .getButtonState());
    }
    if (btnAdvancedBasic != null) {
      screenState.setButtonState(btnAdvancedBasic.getButtonStateKey(),
          btnAdvancedBasic.getButtonState());
    }
    if (btnMoreLess != null) {
      screenState.setButtonState(btnMoreLess.getButtonStateKey(), btnMoreLess
          .getButtonState());
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.28  2009/01/20 20:17:15  sueh
 * <p> bug# 1102 Added title and getTitle.
 * <p>
 * <p> Revision 1.26  2008/10/27 20:40:53  sueh
 * <p> bug# 1141 Added setOpen.
 * <p>
 * <p> Revision 1.25  2007/03/31 03:01:22  sueh
 * <p> bug# 964 Changed PanelHeader.isAdvanceBasicExpanded to isAdvanced.
 * <p>
 * <p> Revision 1.24  2007/03/09 22:06:28  sueh
 * <p> bug# 964 Added getAdvancedBasicOnlyInstance().
 * <p>
 * <p> Revision 1.23  2007/02/21 04:23:26  sueh
 * <p> bug# 964 Changed setState(PanelHeaderState) to
 * <p> setState(ConstPanelHeaderState).
 * <p>
 * <p> Revision 1.22  2006/06/16 15:26:40  sueh
 * <p> bug# 834 Added setButtonStates(BaseScreenState, boolean) to set the states
 * <p> with open/close defaulting to closed.
 * <p>
 * <p> Revision 1.21  2006/04/28 21:00:35  sueh
 * <p> bug# 787 Removed the member variable title, which was not used.
 * <p> ExpandButton:  Moved type information (which kind of expander) and
 * <p> strings associated with the type to a static inner class called Type.
 * <p>
 * <p> Revision 1.20  2006/04/10 19:09:11  sueh
 * <p> bug# 846 Use the default color for the cell title.
 * <p>
 * <p> Revision 1.19  2006/04/06 20:17:29  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.18  2006/03/28 17:04:26  sueh
 * <p> bug# 437 In setButtonStates, the default for btnOpenClose's state is true.
 * <p>
 * <p> Revision 1.17  2006/03/28 00:55:33  sueh
 * <p> bug# 437 Change getButtonStateKey(DialogType) to
 * <p> createButtonStateKey(DialogType).  Set the name of each button.
 * <p>
 * <p> Revision 1.16  2006/03/27 21:04:36  sueh
 * <p> bug# 836 Added DialogType to get instances functions so
 * <p> that the buttons in PanelHeader could save themselves.  Added
 * <p> setButtonsStates() and getButtonStates().
 * <p>
 * <p> Revision 1.15  2005/12/14 20:57:16  sueh
 * <p> bug# 784 Added context sensitive tool tips.
 * <p>
 * <p> Revision 1.14  2005/09/29 19:10:35  sueh
 * <p> bug# 532 Added isAdvanceBasicExpanded().
 * <p>
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
