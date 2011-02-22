package etomo.ui.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import etomo.type.ActionElement;
import etomo.type.DialogType;

/**
 * <p>Description: MenuButton is a MultiLineButton which can create a custom
 * right-click menu.</p>
 * 
 * <p>Copyright: Copyright 2007</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
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
 * <p> Revision 1.5  2010/06/30 21:09:59  sueh
 * <p> bug# 1387 Added a debug instance of MultiLineButton.
 * <p>
 * <p> Revision 1.4  2009/01/20 20:15:14  sueh
 * <p> bug# 1102 Changed JMenuItems to MenuItems so they can name
 * <p> themselves.
 * <p>
 * <p> Revision 1.3  2008/05/03 00:50:52  sueh
 * <p> bug# 847 This class will not be used.
 * <p>
 * <p> Revision 1.2  2007/09/13 18:38:08  sueh
 * <p> bug# 847 Added action().  Using ActionElement instead of strings to identify the
 * <p> menu item selected.
 * <p>
 * <p> Revision 1.1  2007/08/10 17:32:53  sueh
 * <p> bug# 847 Class to create a MultiLineButton with a custom right-click menu.
 * <p> </p>
 */
final class MenuButton extends MultiLineButton implements ContextMenu {
  public static final String rcsid = "$Id$";

  private static final String MENU_STRING = "Run To";

  private MenuButtonContainer container = null;
  private JPopupMenu contextMenu = null;
  private JMenuItem[] menuItemArray = null;
  private ActionElement[] actionElementArray = null;
  private MenuActionListener listener = new MenuActionListener(this);

  /**
   * creates the right click menu
   * @param menuTitle
   * @param menuItemArray
   */
  MenuButton(String label, boolean toggleButton, DialogType dialogType) {
    super(label, toggleButton, dialogType, false);
  }

  static final MenuButton getToggleMenuButtonInstance(String label, DialogType dialogType) {
    return new MenuButton(label, true, dialogType);
  }

  void addMenu(MenuButtonContainer menuButtonContainer, ActionElement[] actionElementArray) {
    if (container != null) {
      return;
    }
    addMouseListener(new GenericMouseAdapter(this));
    this.actionElementArray = actionElementArray;
    container = menuButtonContainer;

    if (actionElementArray == null) {
      actionElementArray = new ActionElement[0];
    }
    menuItemArray = new MenuItem[actionElementArray.length];
    for (int i = 0; i < actionElementArray.length; i++) {
      menuItemArray[i] = new MenuItem(MENU_STRING + " "
          + actionElementArray[i].getActionCommand());
      contextMenu.add(menuItemArray[i]);
      menuItemArray[i].addActionListener(listener);
    }
  }

  public final void popUpContextMenu(MouseEvent mouseEvent) {
    contextMenu.show(getComponent(), mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }

  final void action(ActionEvent event) {
    if (container != null) {
      String command = event.getActionCommand();
      //Find the action element that matches the menu item and pass it to the
      //container.
      for (int i = 0; i < actionElementArray.length; i++) {
        if (actionElementArray[i].getActionCommand().equals(command)) {
          container.action(getActionCommand(), actionElementArray[i]);
        }
      }
    }
  }

  private final class MenuActionListener implements ActionListener {
    private final MenuButton adaptee;

    private MenuActionListener(MenuButton adaptee) {
      this.adaptee = adaptee;
    }

    public final void actionPerformed(ActionEvent event) {
      adaptee.action(event);
    }
  }
}
