package etomo.ui;

import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

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
 * <p> $Log$ </p>
 */
final class MenuButton extends MultiLineButton implements ContextMenu {
  public static final String rcsid = "$Id$";

  private final MenuButtonContainer container;
  private final JPopupMenu contextMenu;
  private final JMenuItem[] jMenuItemArray;

  /**
   * creates the right click menu
   * @param menuTitle
   * @param menuItemArray
   */
  MenuButton(MenuButtonContainer menuButtonContainer, String menuTitle, MenuItem[] menuItemArray) {
    container = menuButtonContainer;
    contextMenu = new JPopupMenu(menuTitle);
    if (menuItemArray == null) {
      menuItemArray = new MenuItem[0];
    }
    jMenuItemArray = new JMenuItem[menuItemArray.length];
    for (int i = 0; i < menuItemArray.length; i++) {
      jMenuItemArray[i] = new JMenuItem(menuItemArray[i].getTitle());
    }
  }
  
  public final void popUpContextMenu(MouseEvent mouseEvent) {
  }
}
