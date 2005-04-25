package etomo.ui;

import java.awt.BorderLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;

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
public final class SubFrame extends EtomoFrame {
  public static  final String  rcsid =  "$Id$";
  
  private MainFrame mainFrame;
  private JPanel rootPanel;
  private JLabel statusBar;
  
  protected SubFrame(MainFrame mainFrame) {
    register();
    this.mainFrame = mainFrame;
  }
  
  protected final synchronized void register() {
    if (subFrame != null) {
      throw new IllegalStateException("Only one instance of SubFrame is allowed.");
    }
    subFrame = this;
    main = false;
  }
  
  /**
   * One-time initialization.  Sets the mainPanel, creates the menus, and
   * displays the axis panel.
   * @param title
   * @param currentManager
   * @param mRUList
   */
  protected void initialize(String title, BaseManager currentManager, String[] mRUList) {
    super.initialize();
    this.currentManager = currentManager;
    setTitle(title);
    mainPanel = mainFrame.getMainPanel();
    rootPanel = (JPanel) getContentPane();
    rootPanel.setLayout(new BorderLayout());
    statusBar = new JLabel(mainPanel.getStatusBarText());
    setAxis();
    menu.setEnabled(currentManager);
    menu.setMRUFileLabels(mRUList);
  }
  
  /**Overridden so we can exit when window is closed*/
  protected void processWindowEvent(WindowEvent event) {
    super.processWindowEvent(event);
    if (event.getID() == WindowEvent.WINDOW_CLOSING) {
      mainFrame.showAxisA();
    }
  }
  
  /**
   * Set the main panel when switching managers.
   *
   */
  protected void setMainPanel(String title, BaseManager currentManager) {
    this.currentManager = currentManager;
    mainPanel = mainFrame.getMainPanel();
    setTitle(title);
    statusBar.setText(mainPanel.getStatusBarText());
  }
  
  /**
   * Override superclass to call mainFrame for command which require switching
   * axis.
   */
  protected void menuOptionsAction(ActionEvent event) {
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
  protected void updateAxis() {
    rootPanel.removeAll();
    setAxis();
  }
  
  /**
   * Add the axis scroll panel and the status bar, and set the location.
   *
   */
  protected void setAxis() {
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
* <p> Revision 1.3  2005/04/21 20:54:49  sueh
* <p> bug# 615 Moved menu handling to EtomoFrame so that SubFrame can
* <p> respond directly to menu commands.  Added title bar.
* <p>
* <p> Revision 1.2  2005/04/20 01:53:04  sueh
* <p> bug# 615 Overrode pack() to prevent the scrollbar from disappearing.
* <p> Added a menu (EtomoMenu).  Added action functions.
* <p>
* <p> Revision 1.1  2005/04/16 02:04:48  sueh
* <p> bug# 615 Subordinate frame to hold the B axis when both axis are
* <p> displayed.
* <p> </p>
*/
