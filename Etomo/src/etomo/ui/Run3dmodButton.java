package etomo.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import etomo.type.Run3dmodMenuOptions;

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
class Run3dmodButton extends MultiLineButton implements ContextMenu {
  public static  final String  rcsid =  "$Id$";
  
  private final JPopupMenu contextMenu = new JPopupMenu("3dmod Options");;
  private final JMenuItem startupWindow = new JMenuItem("Open with start up window");
  private final JMenuItem binBy2 = new JMenuItem("Open binned by 2");
  private final Run3dmodButtonContainer container;
  
  Run3dmodButton(String text, Run3dmodButtonContainer container) {
    this(text, container, false);
  }
  
  private Run3dmodButton(String text, Run3dmodButtonContainer container, boolean toggleButton) {
    super(text, toggleButton);
    this.container = container;
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    addMouseListener(mouseAdapter);
    MenuActionListener menuActionListener = new MenuActionListener(this);
    contextMenu.add(startupWindow);
    contextMenu.add(binBy2);
    startupWindow.addActionListener(menuActionListener);
    binBy2.addActionListener(menuActionListener);
  }
  
  static Run3dmodButton getToggleButtonInstance(String text, Run3dmodButtonContainer container) {
    return new Run3dmodButton(text, container, true);
  }

  public final void popUpContextMenu(MouseEvent mouseEvent) {
    if (!isEnabled()) {
      return;
    }
    contextMenu.show(getComponent(), mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }
  
  private final void performMenuAction(ActionEvent event) {
    Run3dmodMenuOptions menuOptions = new Run3dmodMenuOptions();
    if (event.getActionCommand().equals(startupWindow.getText())) {
      menuOptions.setStartupWindow(true);
      container.run3dmod(this, menuOptions);
    }
    else if (event.getActionCommand().equals(binBy2.getText())) {
      menuOptions.setBinBy2(true);
      container.run3dmod(this, menuOptions);
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
* <p> Revision 1.2  2005/08/10 20:46:10  sueh
* <p> bug# 711 Changed context menu to starting caps instead of all caps.
* <p>
* <p> Revision 1.1  2005/08/09 20:32:45  sueh
* <p> bug# 711  Class to add a 3dmod context menu to a multi line button.
* <p> Runs run3dmod in container class when context menu is used.
* <p> </p>
*/