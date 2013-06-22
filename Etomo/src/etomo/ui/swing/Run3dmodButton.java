package etomo.ui.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import etomo.type.DialogType;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description:   Run3dmodButton extends MultiLineButton.  It creates a right
 * click menu to run 3dmod.  It requires a reference to a class which implements
 * Run3dmodButtonContainer.  The Run3dmodButtonContainer class runs 3dmod.</p>
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
final class Run3dmodButton extends MultiLineButton implements ContextMenu,
    Deferred3dmodButton {
  public static final String rcsid = "$Id$";

  private final JPopupMenu contextMenu = new JPopupMenu("3dmod Options");
  private final JMenuItem startupWindow;
  private final JMenuItem binBy2;
  // When the button is not a 3dmod button, then it may run 3dmod deferred; first
  // running the process associated with the button and then running 3dmod as
  // directed by the right-click menu. The right click menu contains a plain
  // 3dmod option (noMenuOption) when deferred is true.
  private final boolean deferred;

  private Run3dmodButtonContainer container = null;
  private JMenuItem noMenuOption = null;
  // When deferred is true, need a button that knows how to run the 3dmod command.
  private Deferred3dmodButton deferred3dmodButton = null;

  private Run3dmodButton(final String label, final Run3dmodButtonContainer container,
      final boolean toggleButton, final DialogType dialogType, boolean deferred,
      String description) {
    super(label, toggleButton, dialogType, false);
    this.container = container;
    this.deferred = deferred;
    String openString;
    if (deferred) {
      if (description == null) {
        openString = "And open 3dmod";
      }
      else {
        openString = "And open " + description;
      }
      noMenuOption = new MenuItem(openString);
    }
    else {
      if (description == null) {
        openString = "Open";
      }
      else {
        openString = "Open " + description;
      }
    }
    startupWindow = new MenuItem(openString + " with startup window");
    binBy2 = new MenuItem(openString + " binned by 2");
  }

  static Run3dmodButton get3dmodInstance(final String label,
      final Run3dmodButtonContainer container) {
    Run3dmodButton instance = new Run3dmodButton(label, container, false, null, false,
        null);
    instance.init();
    return instance;
  }

  static Run3dmodButton get3dmodInstance(final String label) {
    Run3dmodButton instance = new Run3dmodButton(label, null, false, null, false, null);
    instance.init();
    return instance;
  }

  static Run3dmodButton getToggle3dmodInstance(final String label,
      final DialogType dialogType) {
    Run3dmodButton instance = new Run3dmodButton(label, null, true, dialogType, false,
        null);
    instance.init();
    return instance;
  }

  static Run3dmodButton getDeferred3dmodInstance(final String label,
      final Run3dmodButtonContainer container) {
    Run3dmodButton instance = new Run3dmodButton(label, container, false, null, true,
        null);
    instance.init();
    return instance;
  }

  static Run3dmodButton getDeferred3dmodInstance(final String label) {
    Run3dmodButton instance = new Run3dmodButton(label, null, false, null, true, null);
    instance.init();
    return instance;
  }

  static Run3dmodButton getDeferred3dmodInstance(final String label,
      final Run3dmodButtonContainer container, String description) {
    Run3dmodButton instance = new Run3dmodButton(label, container, false, null, true,
        description);
    instance.init();
    return instance;
  }

  static Run3dmodButton getDeferredToggle3dmodInstance(final String label) {
    Run3dmodButton instance = new Run3dmodButton(label, null, true, null, true, null);
    instance.init();
    return instance;
  }

  static Run3dmodButton getDeferredToggle3dmodInstance(final String label,
      final DialogType dialogType) {
    Run3dmodButton instance = new Run3dmodButton(label, null, true, dialogType, true,
        null);
    instance.init();
    return instance;
  }

  void setDeferred3dmodButton(Deferred3dmodButton input) {
    if (input == null && deferred) {
      throw new NullPointerException(
          "A deferred instance needs to have a deferred3dmodButton.");
    }
    deferred3dmodButton = input;
  }

  void setDeferred3dmodButton(BinnedXY3dmodButton input) {
    if (input == null && deferred) {
      throw new NullPointerException(
          "A deferred instance needs to have a deferred3dmodButton.");
    }
    deferred3dmodButton = input.getButton();
  }

  Deferred3dmodButton getDeferred3dmodButton() {
    return deferred3dmodButton;
  }

  private void init() {
    if (noMenuOption != null) {
      contextMenu.add(noMenuOption);
    }
    contextMenu.add(startupWindow);
    contextMenu.add(binBy2);
    addListeners();
  }

  private void addListeners() {
    addMouseListener(new GenericMouseAdapter(this));
    MenuActionListener listener = new MenuActionListener(this);
    if (noMenuOption != null) {
      noMenuOption.addActionListener(listener);
    }
    startupWindow.addActionListener(listener);
    binBy2.addActionListener(listener);
  }

  public void popUpContextMenu(MouseEvent mouseEvent) {
    if (!isEnabled()) {
      return;
    }
    contextMenu.show(getComponent(), mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }

  void setContainer(final Run3dmodButtonContainer container) {
    this.container = container;
  }

  private void action(final ActionEvent event) {
    // MenuOptions holds the current menu choice.
    Run3dmodMenuOptions menuOptions = new Run3dmodMenuOptions();
    if (event.getActionCommand().equals(startupWindow.getText())) {
      menuOptions.setStartupWindow(true);
    }
    else if (event.getActionCommand().equals(binBy2.getText())) {
      menuOptions.setBinBy2(true);
    }
    action(menuOptions);
    if (isToggleButton()) {
      setSelected(true);
    }
  }

  public void action(Run3dmodMenuOptions menuOptions) {
    if (container != null) {
      container.action(this, menuOptions);
    }
  }

  private final class MenuActionListener implements ActionListener {
    private final Run3dmodButton adaptee;

    private MenuActionListener(final Run3dmodButton adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.15  2010/06/30 21:10:23  sueh
 * <p> bug# 1387 Added a debug instance of MultiLineButton.
 * <p>
 * <p> Revision 1.14  2009/09/01 03:18:24  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.13  2009/01/20 20:24:18  sueh
 * <p> bug# 1102 Changed JMenuItem variables to type MenuItem so that they can name themselves.
 * <p>
 * <p> Revision 1.12  2008/05/13 23:06:06  sueh
 * <p> bug# 847 Added deferred3dmodButton, which holds to method of
 * <p> running 3dmod that the button instance will use.
 * <p>
 * <p> Revision 1.11  2008/05/07 00:24:08  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.  Factor out the container.action call in the action function into a public action function
 * <p> with the menu option as a parameter.
 * <p>
 * <p> Revision 1.10  2008/05/03 00:55:20  sueh
 * <p> bug# 847 Added deferred to run 3dmod after another process is done.
 * <p> Added noMenuOption to run a plain 3dmod.  Added run3dmodProcess to
 * <p> collect information about the 3dmod so it can be run by the manager.
 * <p>
 * <p> Revision 1.9  2007/08/10 17:36:35  sueh
 * <p> bug# 847 Removed implements clause for ProcessResultDisplay because
 * <p> MultiLineButton already implements it.
 * <p>
 * <p> Revision 1.8  2006/02/06 21:21:45  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 1.7  2006/01/12 22:16:22  sueh
 * <p> bug# 401 For toggle buttons set selected to true every time a menu item is
 * <p> selected.
 * <p> bug# 798 Reducing the visibility and inheritability of ui classes.
 * <p>
 * <p> Revision 1.6  2006/01/11 22:37:22  sueh
 * <p> bug# 675 Naming Run3dmodButton's
 * <p>
 * <p> Revision 1.5  2006/01/03 23:44:59  sueh
 * <p> Added setName().
 * <p>
 * <p> Revision 1.4  2005/11/14 22:18:53  sueh
 * <p> Removed extra ;'s.
 * <p>
 * <p> Revision 1.3  2005/08/11 23:58:00  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Prevent context menu from popping up when button is
 * <p> disabled.
 * <p>
 * <p> Revision 1.2  2005/08/10 20:46:10  sueh
 * <p> bug# 711 Changed context menu to starting caps instead of all caps.
 * <p>
 * <p> Revision 1.1  2005/08/09 20:32:45  sueh
 * <p> bug# 711  Class to add a 3dmod context menu to a multi line button.
 * <p> Runs run3dmod in container class when context menu is used.
 * <p> </p>
 */
