package etomo.ui;

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

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
public class SubFrame extends JFrame implements EtomoFrame {
  public static  final String  rcsid =  "$Id$";
  
  private MainFrame mainFrame;
  private MainPanel mainPanel;
  private JPanel rootPanel;
  private EtomoMenu menu;
  private JMenuBar menuBar;
  
  SubFrame(MainFrame mainFrame) {
    this.mainFrame = mainFrame;

  }
  
  void initialize() {
    mainPanel = mainFrame.getMainPanel();
    rootPanel = (JPanel) getContentPane();
    rootPanel.setLayout(new BorderLayout());
    setAxis();
    menu = EtomoMenu.newEtomoMenu();
    createMenus();
  }
  
  /**
   * Set the main panel when switching managers.
   *
   */
  void setMainPanel() {
    mainPanel = mainFrame.getMainPanel();
  }
  
  /**
   * Increase the bounds by one pixel before packing.  This preserves the
   * scrollbar when the window size doesn't change.
   */
  public void pack() {
    Rectangle bounds = getBounds();
    bounds.height++;
    bounds.width++;
    setBounds(bounds);
    super.pack();
  }
  
  public void menuOptionsAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(menu.getActionCommandFitWindow())) {
      pack();
    }
    else {
      mainFrame.menuOptionsAction(event);
    }
  }
  
  public void menuFileMRUListAction(ActionEvent event) {
    mainFrame.menuFileMRUListAction(event);
  }
  
  public void menuFileAction(ActionEvent event) {
    mainFrame.menuFileAction(event);
  }
  
  public void menuHelpAction(ActionEvent event) {
    mainFrame.menuHelpAction(event);
  }
  
  void updateAxis() {
    rootPanel.removeAll();
    setAxis();
  }
  
  private void setAxis() {
    JScrollPane axis = mainPanel.showAxisB(true);
    if (axis != null)
    {
      rootPanel.add(axis, BorderLayout.CENTER);
    }
    /*Point location = mainPanel.getPreviousSubFrameLocation();
    if (location == null) {
      Rectangle mainFrameBounds = mainFrame.getBounds();
      location = new Point(mainFrameBounds.x + mainFrameBounds.width, mainFrameBounds.y);
    }*/
    Rectangle mainFrameBounds = mainFrame.getBounds();
    setLocation(mainFrameBounds.x + mainFrameBounds.width, mainFrameBounds.y);
    validate();
    setVisible(true);
  }
  /*
  void saveLocation() {
    mainPanel.setPreviousSubFrameLocation(getLocation());
  }*/
  
  /**
   * Create the menus for the subordinate frame
   */
  private void createMenus() {
    menu.createMenus(this);
    menuBar = menu.getMenuBar();
    setJMenuBar(menuBar);
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/04/16 02:04:48  sueh
* <p> bug# 615 Subordinate frame to hold the B axis when both axis are
* <p> displayed.
* <p> </p>
*/
