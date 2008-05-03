package etomo.ui;

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
final class Run3dmodButton extends MultiLineButton implements ContextMenu {
  public static final String rcsid = "$Id$";

  private final JPopupMenu contextMenu = new JPopupMenu("3dmod Options");
  private final JMenuItem startupWindow;
  private final JMenuItem binBy2;
  //When the button is not a 3dmod button, then it may run 3dmod deferred; first
  //running the process associated with the button and then running 3dmod as
  //directed by the right-click menu.  The right click menu contains a plain
  //3dmod option (noMenuOption) when deferred is true.
  private final boolean deferred;

  //If deferred is true then the information needed by the manager to choose
  //which 3dmod process to run and any information needed to run it must be
  //included in run3dmodOptions.  If deferred is false then run3dmodOptions is
  //optional, but it can be used to share information between the real 3dmod
  //button and the deferred button.  This should allow the manager to use the
  //same function to run 3dmod for both buttons.
  private Run3dmodProcess run3dmodProcess = null;
  private Run3dmodButtonContainer container = null;
  private JMenuItem noMenuOption = null;

  private Run3dmodButton(final String label,
      final Run3dmodButtonContainer container, final boolean toggleButton,
      final DialogType dialogType, boolean deferred) {
    super(label, toggleButton, dialogType);
    this.container = container;
    this.deferred = deferred;
    String openString;
    if (deferred) {
      openString = "And open 3dmod";
      noMenuOption = new JMenuItem(openString);
    }
    else {
      openString = "Open";
    }
    startupWindow = new JMenuItem(openString + " with startup window");
    binBy2 = new JMenuItem(openString + " binned by 2");
  }

  static Run3dmodButton get3dmodInstance(final String label,
      final Run3dmodButtonContainer container) {
    Run3dmodButton instance = new Run3dmodButton(label, container, false, null,
        false);
    instance.init();
    return instance;
  }

  static Run3dmodButton getToggle3dmodInstance(final String label,
      final DialogType dialogType) {
    Run3dmodButton instance = new Run3dmodButton(label, null, true, dialogType,
        false);
    instance.init();
    return instance;
  }

  static Run3dmodButton getDeferredToggle3dmodInstance(final String label,
      final DialogType dialogType) {
    Run3dmodButton instance = new Run3dmodButton(label, null, true, dialogType,
        true);
    instance.init();
    return instance;
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

   void setRun3dmodProcess(Run3dmodProcess input) {
    run3dmodProcess = input;
  }
   
   Run3dmodProcess getRun3dmodOptions() {
     return run3dmodProcess;
   }

  public void popUpContextMenu(MouseEvent mouseEvent) {
    if (!isEnabled()) {
      return;
    }
    contextMenu.show(getComponent(), mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }

  void setRun3dmodButtonContainer(final Run3dmodButtonContainer container) {
    this.container = container;
  }

  private void performMenuAction(final ActionEvent event) {
    //MenuOptions holds the current menu choice.
    Run3dmodMenuOptions menuOptions = new Run3dmodMenuOptions();
    if (event.getActionCommand().equals(startupWindow.getText())) {
      menuOptions.setStartupWindow(true);
    }
    else if (event.getActionCommand().equals(binBy2.getText())) {
      menuOptions.setBinBy2(true);
    }
    if (container != null) {
      container.action(this, menuOptions);
    }
    if (isToggleButton()) {
      setSelected(true);
    }
  }

  private final class MenuActionListener implements ActionListener {
    private final Run3dmodButton adaptee;

    private MenuActionListener(final Run3dmodButton adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.performMenuAction(event);
    }
  }
}
/**
 * <p> $Log$
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
