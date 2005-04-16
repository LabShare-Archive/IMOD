package etomo.ui;

import java.awt.BorderLayout;
import java.awt.Rectangle;

import javax.swing.JFrame;
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
public class SubFrame extends JFrame {
  public static  final String  rcsid =  "$Id$";
  
  private MainFrame mainFrame;
  private MainPanel mainPanel;
  private JPanel rootPanel;
  
  SubFrame(MainFrame mainFrame) {
    this.mainFrame = mainFrame;
    mainPanel = mainFrame.getMainPanel();
    rootPanel = (JPanel) getContentPane();
    rootPanel.setLayout(new BorderLayout());
    setAxis();
  }
  
  void updateAxis() {
    rootPanel.removeAll();
    setAxis();
  }
  
  private void setAxis() {
    JScrollPane axis = mainPanel.getAxisB();
    if (axis != null)
    {
      rootPanel.add(axis, BorderLayout.CENTER);
    }
    Rectangle mainFrameBounds = mainFrame.getBounds();
    setLocation(mainFrameBounds.x + mainFrameBounds.width, mainFrameBounds.y);
    validate();
    setVisible(true);
  }
}
/**
* <p> $Log$ </p>
*/
