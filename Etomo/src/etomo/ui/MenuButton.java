package etomo.ui;

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
 * <p> Revision 1.1  2007/08/10 17:32:53  sueh
 * <p> bug# 847 Class to create a MultiLineButton with a custom right-click menu.
 * <p> </p>
 */
final class MenuButton extends MultiLineButton implements ContextMenu {
  public static final String rcsid = "$Id$";

  private static final String MENU_STRING = "Run To";

  private MenuButtonContainer container = null;
  private JPopupMenu contextMenu = null;
  private JMenuItem[] jMenuItemArray = null;
  private ActionElement[] actionElementArray = null;
  private MenuActionListener listener = new MenuActionListener(this);

  /**
   * creates the right click menu
   * @param menuTitle
   * @param menuItemArray
   */
  MenuButton(String label, boolean toggleButton, DialogType dialogType) {
    super(label, toggleButton, dialogType);
  }

  void addMenu(MenuButtonContainer menuButtonContainer,
      ActionElement[] actionElementArray) {
    if (container != null) {
      return;
    }
    addMouseListener(new GenericMouseAdapter(this));
    this.actionElementArray = actionElementArray;
    container = menuButtonContainer;

    if (actionElementArray == null) {
      actionElementArray = new ActionElement[0];
    }
    jMenuItemArray = new JMenuItem[actionElementArray.length];
    for (int i = 0; i < actionElementArray.length; i++) {
      jMenuItemArray[i] = new JMenuItem(MENU_STRING + " "
          + actionElementArray[i].getActionCommand());
      contextMenu.add(jMenuItemArray[i]);
      jMenuItemArray[i].addActionListener(listener);
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
