package etomo.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import etomo.type.Run3dmodMenuOption;

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
  private final JMenuItem startupWindow = new JMenuItem("Open With Startup Window");
  private final JMenuItem binBy2 = new JMenuItem("Open Binned By 2");
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
    contextMenu.show(getComponent(), mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }
  
  private final void performMenuAction(ActionEvent event) {
    if (event.getActionCommand().equals(startupWindow.getText())) {
      container.run3dmod(this, Run3dmodMenuOption.STARTUP_WINDOW);
    }
    else if (event.getActionCommand().equals(binBy2.getText())) {
      container.run3dmod(this, Run3dmodMenuOption.BIN_BY_2);
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
* <p> $Log$ </p>
*/