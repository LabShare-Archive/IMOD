package etomo.ui.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.util.UniqueHashedArray;
import etomo.util.UniqueKey;

/**
 * <p>Description: Manages switching between windows associated with different
 * .edf and .ejf files.  Keeps the menu and the tabs in sync on the display.
 * Hides the tabs when there is only one window.
 * 
 * MainFrame constructs one instance of WindowSwitch.  MainFrame retrieves and 
 * displays the window menu from it.
 * 
 * MainFrame uses the add, remove, and rename functions to manipulate windows
 * and keep in sync with EtomoDirector.controllerList.
 * MainFrame.setCurrentManager() displays the panel by removing everything from
 * the rootPanel in mainFrame and then calling getPanel() and placing its result
 * in rootPanel.</p>
 * 
 * GetPanel() return a MainPanel when there is only one window.  When there are
 * multiple windows getPanel() builds a tabbed panel containing only the
 * selected mainPanel.  Putting only one MainPanel in the tabPane allows 
 * resizing to work correctly.  When a new window is selected,
 * EtomoDirector.setCurrentManager() is called, which calls
 * MainFrame.setCurrentManager().
 * 
 * WindowSwitch keeps a HashedArray of menuItems and a menu.  It also keeps a
 * HashedArray of MainPanels.  It gets UniqueKeys for the HashedArrays from
 * EtomoDirector.controllerList.  It never builds its own keys.
 * 
 * 
 * 
 * <P>Copyright: Copyright (c) 2002,2003,2004,2005</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class WindowSwitch {
  public static final String rcsid = "$Id$";

  private static final char menuItemDividerChar = ':';
  private static final String menuItemDivider = menuItemDividerChar + " ";

  private Menu menu = new Menu("Window");
  private UniqueHashedArray menuList = null;
  private UniqueHashedArray mainPanelList = null;
  private TabbedPane tabbedPane = null;
  private MenuActionListener menuActionListener;
  private TabChangeListener tabChangeListener;

  WindowSwitch() {
    menuActionListener = new MenuActionListener(this);
    tabChangeListener = new TabChangeListener(this);
  }

  /**
   * Add a controller:  Add a menu item to the menu list.  Add the controller's
   * mainPanel to the mainPanelList.  Add the menu item to the menu.
   * @param controller
   * @param key
   */
  void add(BaseManager manager, UniqueKey key) {
    if (key == null) {
      return;
    }
    if (menuList == null) {
      menuList = new UniqueHashedArray();
      mainPanelList = new UniqueHashedArray();
      tabbedPane = new TabbedPane();
    }
    JCheckBoxMenuItem menuItem = new CheckBoxMenuItem();
    menuItem.addActionListener(menuActionListener);
    int index = menuList.size();
    menuItem.setText(Integer.toString(index + 1) + menuItemDivider + key.getName());
    menuItem.setVisible(true);
    menu.add(menuItem);
    menuList.add(key, menuItem);
    mainPanelList.add(key, manager.getMainPanel());
  }

  /**
   * Rename a window.  Change the menu item, rekey the menuList and the
   * mainPanelList.
   * @param oldKey
   * @param newKey
   */
  void rename(UniqueKey oldKey, UniqueKey newKey) {
    if (oldKey == null || newKey == null || menuList == null) {
      return;
    }
    JCheckBoxMenuItem menuItem = (JCheckBoxMenuItem) menuList.get(oldKey);
    int index = menuList.getIndex(oldKey);
    menuItem.setText(Integer.toString(index + 1) + menuItemDivider + newKey.getName());
    menuList.rekey(oldKey, newKey);
    mainPanelList.rekey(oldKey, newKey);
    if (mainPanelList.size() > 1 && tabbedPane != null
        && tabbedPane.getTabCount() > index) {
      tabbedPane.setTitleAt(index, newKey.getName());
    }
    EtomoDirector.INSTANCE.setCurrentManager(newKey);
  }

  /**
   * Remove a window.  Removed the associated menuItem from the menu and from 
   * menuList.  Removes the associated mainPanel from mainPanelList.
   * @param key
   */
  void remove(UniqueKey key) {
    if (key == null || menuList == null) {
      return;
    }
    JCheckBoxMenuItem menuItem = (JCheckBoxMenuItem) menuList.get(key);
    int index = menuList.getIndex(key);
    menu.remove(menuItem);
    menuList.remove(key);
    mainPanelList.remove(key);
    renumberMenu();
  }

  /**
   * Renumbers the menu's displayed text.  Used when a window is removed.
   *
   */
  private void renumberMenu() {
    if (menuList == null) {
      return;
    }
    for (int i = 0; i < menuList.size(); i++) {
      JCheckBoxMenuItem menuItem = (JCheckBoxMenuItem) menuList.get(i);
      String text = menuItem.getText();
      menuItem.setText(Integer.toString(i + 1)
          + text.substring(text.indexOf(menuItemDividerChar)));
    }
  }

  /**
   * Returns the menu.
   * @return
   */
  Menu getMenu() {
    return menu;
  }

  /**
   * Returns the mainPanel associated with key, if there is only one window.
   * For multiple windows, returns a tabbed pane, with the mainPanel on the
   * selected tab.
   * @param key
   * @return
   */
  JComponent getPanel(UniqueKey key) {
    if (mainPanelList == null || mainPanelList.size() == 0 || key == null) {
      return null;
    }
    if (mainPanelList.size() == 1) {
      return (MainPanel) mainPanelList.get(key);
    }
    setTabs(menuList.getIndex(key));
    return tabbedPane;
  }

  /**
   * Allows the program to select a window.
   * @param key
   */
  void selectWindow(UniqueKey key, boolean newWindow) {
    if (menuList == null || key == null) {
      return;
    }
    int newIndex = menuList.getIndex(key);
    selectMenuItem(newIndex);
  }

  /**
   * Selects a menu item at index.  Unselects all other menu items.  Index
   * starts from zero.
   * @param index
   */
  private void selectMenuItem(int index) {
    if (menuList == null) {
      return;
    }
    JMenuItem menuItem;
    for (int i = 0; i < menuList.size(); i++) {
      menuItem = (JMenuItem) menuList.get(i);
      if (i == index) {
        menuItem.setSelected(true);
      }
      else {
        menuItem.setSelected(false);
      }
    }
  }

  /**
   * Sets up the tabbed pane:
   * - Remove the change listener since it responds to changes caused by the
   *   program.
   * - Remove everything on the pane.
   * - Add the tabs, placing the selected mainPanel on the associated tab.
   * - Select the selected tab.
   * - Add the change listener.
   * This is necessary because adding mainPanel to existing tabs causes new tabs
   * to be created.
   * @param selectedTabIndex
   */
  private void setTabs(int selectedTabIndex) {
    if (menuList == null) {
      return;
    }
    //The MainPanel can't always measure its display state accurately when it is 
    //displayed on a tab.  Saving the display state allow MainPanel to display
    //correctly when it is brought up again.
    int oldIndex = tabbedPane.getSelectedIndex();
    if (oldIndex != -1) {
      MainPanel oldMainPanel = (MainPanel) mainPanelList.get(oldIndex);
      if (oldMainPanel != null) {
        oldMainPanel.saveDisplayState();
      }
    }
    tabbedPane.removeChangeListener(tabChangeListener);
    tabbedPane.removeAll();
    if (menuList.size() < 2) {
      return;
    }
    for (int i = 0; i < menuList.size(); i++) {
      String tabName = ((JMenuItem) menuList.get(i)).getText();
      tabName = tabName.substring(tabName.indexOf(menuItemDivider)
          + menuItemDivider.length());
      if (i == selectedTabIndex) {
        tabbedPane.addTab(tabName, (MainPanel) mainPanelList.get(i));
      }
      else {
        JLabel placeHolder = new JLabel();
        placeHolder.setVisible(false);
        tabbedPane.addTab(tabName, placeHolder);
      }
    }
    tabbedPane.setSelectedIndex(selectedTabIndex);
    tabbedPane.addChangeListener(tabChangeListener);
  }

  /**
   * Open the specified window when the user chooses a window menu item.
   * @param event
   */
  protected void menuAction(ActionEvent event) {
    String menuChoice = event.getActionCommand();
    int newIndex = Integer.parseInt(menuChoice.substring(0, menuChoice
        .indexOf(menuItemDividerChar))) - 1;
    selectMenuItem(newIndex);
    EtomoDirector.INSTANCE.setCurrentManager(menuList.getKey(newIndex));
  }

  /**
   * Open the specified window when the user chooses a tab.
   * @param event
   */
  protected void tabChanged(ChangeEvent event) {
    int newIndex = tabbedPane.getSelectedIndex();
    selectMenuItem(newIndex);
    EtomoDirector.INSTANCE.setCurrentManager(menuList.getKey(newIndex));
  }

  //  window list menu action listener
  class MenuActionListener implements ActionListener {
    WindowSwitch adaptee;

    MenuActionListener(WindowSwitch adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e) {
      adaptee.menuAction(e);
    }
  }

  class TabChangeListener implements ChangeListener {
    WindowSwitch adaptee;

    public TabChangeListener(WindowSwitch dialog) {
      adaptee = dialog;
    }

    public void stateChanged(ChangeEvent event) {
      adaptee.tabChanged(event);
    }
  }

}

/**
 * <p>$Log$
 * <p>Revision 1.1  2010/11/13 16:07:34  sueh
 * <p>bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p>Revision 1.15  2010/02/17 05:03:12  sueh
 * <p>bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p>Revision 1.14  2009/04/02 19:19:29  sueh
 * <p>bug# 1206 Handle a null managerKey parameter.
 * <p>
 * <p>Revision 1.13  2009/03/17 00:46:24  sueh
 * <p>bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p>Revision 1.12  2009/01/20 20:33:48  sueh
 * <p>bug# 1102 Changed menu items to self-naming menu items.
 * <p>
 * <p>Revision 1.11  2007/09/07 00:30:14  sueh
 * <p>bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p>instead of getInstance and createInstance.
 * <p>
 * <p>Revision 1.10  2005/11/14 22:36:17  sueh
 * <p>bug# 762 Made menuAction() and tabChanged protected.
 * <p>
 * <p>Revision 1.9  2005/08/22 18:21:35  sueh
 * <p>bug# 532 Moved HashedArray to UniqueHashedArray.  Added a simpler
 * <p>HashedArray class which does not use UniqueKey.
 * <p>
 * <p>Revision 1.8  2005/06/21 00:50:23  sueh
 * <p>bug# 522 In order to get a current manager when --test is set, moved call
 * <p>to EtomoDirector.setCurrentManager() out of WindowSwitch.setWindow().
 * <p>Now the call follows all the calls to UIHarness.selectWindowMenuItem().
 * <p>Delete selectWindow(UniqueKey), since its not being used.
 * <p>
 * <p>Revision 1.7  2005/06/01 21:29:30  sueh
 * <p>bug# 667 Removing the Controller classes.  Trying make meta data and
 * <p>app manager equals didn't work very well.  Meta data is created by and
 * <p>managed by app mgr and the class structure should reflect that.
 * <p>
 * <p>Revision 1.6  2005/04/20 01:56:13  sueh
 * <p>bug# 615 SetController was causing MainFrame.setCurrentManager() to
 * <p>be called twice.  Passing newWindow to selectWindow makes it possible
 * <p>to remove one of the SetController calls to
 * <p>MainFrame.setCurrentManager().  MainFrame.setCurrentManager() must
 * <p>be called with newWindow = true when the .edf file is first opened.
 * <p>
 * <p>Revision 1.5  2005/02/17 16:47:09  sueh
 * <p>bug# 605 In setTabs():  check for null to handle a MainPanel that has
 * <p>been removed on close
 * <p>
 * <p>Revision 1.4  2005/02/17 02:48:31  sueh
 * <p>bug# 605 Tell a mainPanel instance that is currently being displayed to
 * <p>save its display state before displaying another MainPanel instance.
 * <p>
 * <p>Revision 1.3  2005/02/11 19:05:17  sueh
 * <p>bug# 594 Putting setting tab title back into rename because it doesn't
 * <p>refresh New Join  when make sample is run.  Add checks to make sure
 * <p>this doesn't crash.
 * <p>
 * <p>Revision 1.2  2005/02/11 16:48:13  sueh
 * <p>bug# 594 bug fix:  trying to rename a tab when there is only one window,
 * <p>which means that the tab pane may not exist.
 * <p>
 * <p>Revision 1.1  2005/02/07 22:57:18  sueh
 * <p>bug# 594 Class to synchronize windows menu and tabs.
 * <p></p>
 */
