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
  private final JMenuItem startupWindow = new JMenuItem(
      "Open with startup window");
  private final JMenuItem binBy2 = new JMenuItem("Open binned by 2");
  private Run3dmodButtonContainer container = null;

  Run3dmodButton(String label, Run3dmodButtonContainer container) {
    this(label, container, false);
  }

  protected Run3dmodButton(String label, boolean toggleButton,
      DialogType dialogType) {
    super(label, toggleButton, dialogType);
    init(null);
  }

  private Run3dmodButton(String label, Run3dmodButtonContainer container,
      boolean toggleButton) {
    super(label, toggleButton);
    init(container);
  }

  private void init(Run3dmodButtonContainer container) {
    this.container = container;
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    addMouseListener(mouseAdapter);
    MenuActionListener menuActionListener = new MenuActionListener(this);
    contextMenu.add(startupWindow);
    contextMenu.add(binBy2);
    startupWindow.addActionListener(menuActionListener);
    binBy2.addActionListener(menuActionListener);
  }
  
  static final Run3dmodButton getToggle3dmodButtonInstance(String label,
      DialogType dialogType) {
    return new Run3dmodButton(label, true, dialogType);
  }

  public final void popUpContextMenu(MouseEvent mouseEvent) {
    if (!isEnabled()) {
      return;
    }
    contextMenu.show(getComponent(), mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }

  final void setRun3dmodButtonContainer(Run3dmodButtonContainer container) {
    this.container = container;
  }

  protected final void performMenuAction(ActionEvent event) {
    Run3dmodMenuOptions menuOptions = new Run3dmodMenuOptions();
    if (event.getActionCommand().equals(startupWindow.getText())) {
      menuOptions.setStartupWindow(true);
      if (container != null) {
        container.run3dmod(this, menuOptions);
      }
      if (isToggleButton()) {
        setSelected(true);
      }
    }
    else if (event.getActionCommand().equals(binBy2.getText())) {
      menuOptions.setBinBy2(true);
      if (container != null) {
        container.run3dmod(this, menuOptions);
      }
      if (isToggleButton()) {
        setSelected(true);
      }
    }
  }

  private final class MenuActionListener implements ActionListener {
    private final Run3dmodButton adaptee;

    private MenuActionListener(Run3dmodButton adaptee) {
      this.adaptee = adaptee;
    }

    public final void actionPerformed(ActionEvent event) {
      adaptee.performMenuAction(event);
    }
  }
}
/**
 * <p> $Log$
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