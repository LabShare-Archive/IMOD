package etomo.ui;

import java.awt.BorderLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import etomo.BaseManager;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class SubFrame extends EtomoFrame {
  public static  final String  rcsid =  "$Id$";
  
  private MainFrame mainFrame;
  private JPanel rootPanel;
  private JLabel statusBar;
  
  SubFrame(MainFrame mainFrame) {
    this.mainFrame = mainFrame;
  }
  
  /**
   * One-time initialization.  Sets the mainPanel, creates the menus, and
   * displays the axis panel.
   * @param title
   * @param currentManager
   * @param mRUList
   */
  void initialize(String title, BaseManager currentManager, String[] mRUList) {
    this.currentManager = currentManager;
    ImageIcon iconEtomo = new ImageIcon(ClassLoader
        .getSystemResource("images/etomo.png"));
    setTitle(title);
    setIconImage(iconEtomo.getImage());
    mainPanel = mainFrame.getMainPanel();
    rootPanel = (JPanel) getContentPane();
    rootPanel.setLayout(new BorderLayout());
    statusBar = new JLabel(mainPanel.getStatusBarText());
    setAxis();
    menu = EtomoMenu.newEtomoMenu();
    createMenus();
    menu.setEnabled(currentManager, true);
    menu.setMRUFileLabels(mRUList, true);
  }
  
  /**
   * Set the main panel when switching managers.
   *
   */
  void setMainPanel(String title, BaseManager currentManager) {
    this.currentManager = currentManager;
    mainPanel = mainFrame.getMainPanel();
    setTitle(title);
    statusBar.setText(mainPanel.getStatusBarText());
  }
  
  /**
   * Override superclass to call mainFrame for command which require switching
   * axis.
   */
  void menuOptionsAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(menu.getActionCommandAxisA())
     || command.equals(menu.getActionCommandAxisB())
     || command.equals(menu.getActionCommandAxisBoth())) {
      mainFrame.menuOptionsAction(event);
    }
    else {
      super.menuOptionsAction(event);
    }
  }
  
  /**
   * Update the axis when switch from single axis display to dual axis display.
   *
   */
  void updateAxis() {
    rootPanel.removeAll();
    setAxis();
  }
  
  /**
   * Add the axis scroll panel and the status bar, and set the location.
   *
   */
  private void setAxis() {
    rootPanel.removeAll();
    JScrollPane axis = mainPanel.showBothAxis();
    if (axis != null)
    {
      rootPanel.add(axis, BorderLayout.CENTER);
    }
    rootPanel.add(statusBar, BorderLayout.SOUTH);
    Rectangle mainFrameBounds = mainFrame.getBounds();
    setLocation(mainFrameBounds.x + mainFrameBounds.width, mainFrameBounds.y);
    validate();
    setVisible(true);
  }

}
/**
* <p> $Log$
* <p> Revision 1.2  2005/04/20 01:53:04  sueh
* <p> bug# 615 Overrode pack() to prevent the scrollbar from disappearing.
* <p> Added a menu (EtomoMenu).  Added action functions.
* <p>
* <p> Revision 1.1  2005/04/16 02:04:48  sueh
* <p> bug# 615 Subordinate frame to hold the B axis when both axis are
* <p> displayed.
* <p> </p>
*/
