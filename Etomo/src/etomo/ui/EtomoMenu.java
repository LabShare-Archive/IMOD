package etomo.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import etomo.BaseManager;
import etomo.type.AxisType;

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
public class EtomoMenu {
  public static final String rcsid = "$Id$";

  private static final int nMRUFileMax = 10;
  
  private JMenuBar menuBar = new JMenuBar();

  private JMenu menuFile = new JMenu("File");
  private JMenuItem menuFileNewTomogram = new JMenuItem("New Tomogram",
      KeyEvent.VK_N);
  private JMenuItem menuFileNewJoin = new JMenuItem("New Join", KeyEvent.VK_J);
  private JMenuItem menuFileOpen = new JMenuItem("Open...", KeyEvent.VK_O);
  private JMenuItem menuFileSave = new JMenuItem("Save", KeyEvent.VK_S);
  private JMenuItem menuFileSaveAs = new JMenuItem("Save As", KeyEvent.VK_A);
  private JMenuItem menuFileClose = new JMenuItem("Close", KeyEvent.VK_C);
  private JMenuItem menuFileExit = new JMenuItem("Exit", KeyEvent.VK_X);
  private JMenuItem menuFileTomosnapshot = new JMenuItem("Run Tomosnapshot", KeyEvent.VK_R);
  private JMenuItem[] menuMRUList = new JMenuItem[nMRUFileMax];

  private JMenu menuOptions = new JMenu("Options");
  private JMenuItem menuAxisA = new JMenuItem("Axis A", KeyEvent.VK_A);
  private JMenuItem menuAxisB = new JMenuItem("Axis B", KeyEvent.VK_B);
  private JMenuItem menuAxisBoth = new JMenuItem("Both Axes", KeyEvent.VK_2);
  private JMenuItem menuSettings = new JMenuItem("Settings", KeyEvent.VK_S);
  private JMenuItem menuFitWindow = new JMenuItem("Fit Window", KeyEvent.VK_F);
  private JCheckBoxMenuItem menu3dmodStartupWindow = new JCheckBoxMenuItem("Open 3dmod with Startup Window");
  private JCheckBoxMenuItem menu3dmodBinBy2 = new JCheckBoxMenuItem("Open 3dmod Binned by 2");
  
  private JMenu menuHelp = new JMenu("Help");
  private JMenuItem menuTomoGuide = new JMenuItem("Tomography Guide",
      KeyEvent.VK_T);
  private JMenuItem menuImodGuide = new JMenuItem("Imod Users Guide",
      KeyEvent.VK_I);
  private JMenuItem menu3dmodGuide = new JMenuItem("3dmod Users Guide",
      KeyEvent.VK_3);
  private JMenuItem menuEtomoGuide = new JMenuItem("Etomo Users Guide",
      KeyEvent.VK_E);
  private JMenuItem menuJoinGuide = new JMenuItem("Join Users Guide",
      KeyEvent.VK_J);
  private JMenuItem menuHelpAbout = new JMenuItem("About", KeyEvent.VK_A);

  void createMenus(EtomoFrame frame) {
    //  Mnemonics for the main menu bar
    menuFile.setMnemonic(KeyEvent.VK_F);
    menuOptions.setMnemonic(KeyEvent.VK_O);
    menuHelp.setMnemonic(KeyEvent.VK_H);

    //  Accelerators
    menuSettings.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
        ActionEvent.CTRL_MASK));

    menuAxisA.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
        ActionEvent.CTRL_MASK));
    menuAxisB.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B,
        ActionEvent.CTRL_MASK));
    menuAxisBoth.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_2,
        ActionEvent.CTRL_MASK));

    menuFitWindow.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F,
        ActionEvent.CTRL_MASK));

    //  Bind the menu items to their listeners
    FileActionListener fileActionListener = new FileActionListener(frame);
    menuFileNewTomogram.addActionListener(fileActionListener);
    menuFileNewJoin.addActionListener(fileActionListener);
    menuFileOpen.addActionListener(fileActionListener);
    menuFileSave.addActionListener(fileActionListener);
    menuFileSaveAs.addActionListener(fileActionListener);
    menuFileClose.addActionListener(fileActionListener);
    menuFileExit.addActionListener(fileActionListener);
    menuFileTomosnapshot.addActionListener(fileActionListener);

    OptionsActionListener optionsActionListener = new OptionsActionListener(
        frame);
    menuSettings.addActionListener(optionsActionListener);
    menuFitWindow.addActionListener(optionsActionListener);
    menuAxisA.addActionListener(optionsActionListener);
    menuAxisB.addActionListener(optionsActionListener);
    menuAxisBoth.addActionListener(optionsActionListener);
    menu3dmodStartupWindow.addActionListener(optionsActionListener);
    menu3dmodBinBy2.addActionListener(optionsActionListener);
    
    HelpActionListener helpActionListener = new HelpActionListener(frame);
    menuTomoGuide.addActionListener(helpActionListener);
    menuImodGuide.addActionListener(helpActionListener);
    menu3dmodGuide.addActionListener(helpActionListener);
    menuEtomoGuide.addActionListener(helpActionListener);
    menuJoinGuide.addActionListener(helpActionListener);
    menuHelpAbout.addActionListener(helpActionListener);

    //  File menu
    menuFile.add(menuFileNewTomogram);
    menuFile.add(menuFileNewJoin);
    menuFile.add(menuFileOpen);
    menuFile.add(menuFileSave);
    menuFile.add(menuFileSaveAs);
    menuFile.add(menuFileClose);
    menuFile.add(menuFileExit);
    menuFile.addSeparator();
    menuFile.add(menuFileTomosnapshot);
    menuFile.addSeparator();

    //  Initialize all of the MRU file menu items
    FileMRUListActionListener fileMRUListActionListener = new FileMRUListActionListener(
        frame);
    for (int i = 0; i < nMRUFileMax; i++) {
      menuMRUList[i] = new JMenuItem();
      menuMRUList[i].addActionListener(fileMRUListActionListener);
      menuMRUList[i].setVisible(false);
      menuFile.add(menuMRUList[i]);
    }

    // Options menu
    menuOptions.add(menuSettings);
    menuOptions.add(menu3dmodStartupWindow);
    menuOptions.add(menu3dmodBinBy2);
    menuOptions.add(menuAxisA);
    menuOptions.add(menuAxisB);
    menuOptions.add(menuAxisBoth);
    menuOptions.add(menuFitWindow);

    // Help menu
    menuHelp.add(menuTomoGuide);
    menuHelp.add(menuImodGuide);
    menuHelp.add(menu3dmodGuide);
    menuHelp.add(menuEtomoGuide);
    menuHelp.add(menuJoinGuide);
    menuHelp.add(menuHelpAbout);

    //  Construct menu bar
    menuBar.add(menuFile);
    menuBar.add(menuOptions);
    menuBar.add(menuHelp);
  }
  
  JMenuBar getMenuBar() {
    return menuBar;
  }
  
  /**
   * Enable/disable menu items based on currentManager.
   * @param currentManager
   */
  void setEnabled(BaseManager currentManager) {
    if (currentManager == null) {
      menuFileSave.setEnabled(false);
      menuFileSaveAs.setEnabled(false);
      menuFileClose.setEnabled(false);
      menuAxisA.setEnabled(false);
      menuAxisB.setEnabled(false);
      menuAxisBoth.setEnabled(false);
    }
    else {
      menuFileSave.setEnabled(true);
      menuFileSaveAs.setEnabled(currentManager.canChangeParamFileName());
      menuFileClose.setEnabled(true);
      boolean dualAxis = currentManager.getBaseMetaData().getAxisType() == AxisType.DUAL_AXIS;
      menuAxisA.setEnabled(dualAxis);
      menuAxisB.setEnabled(dualAxis);
      menuAxisBoth.setEnabled(dualAxis);
    }
    if (currentManager == null || !currentManager.canSnapshot()) {
      menuFileTomosnapshot.setEnabled(false);
    }
    else {
      menuFileTomosnapshot.setEnabled(true);
    }
  }
  
  /**
   * Enable/disable menu items based on otherMenu.
   * @param otherMenu
   */
  void setEnabled(EtomoMenu otherMenu) {
    menuFileNewTomogram.setEnabled(otherMenu.menuFileNewTomogram.isEnabled());
    menuFileNewJoin.setEnabled(otherMenu.menuFileNewJoin.isEnabled());
    menuFileSaveAs.setEnabled(otherMenu.menuFileSaveAs.isEnabled());
    menuAxisA.setEnabled(otherMenu.menuAxisA.isEnabled());
    menuAxisB.setEnabled(otherMenu.menuAxisB.isEnabled());
    menuAxisBoth.setEnabled(otherMenu.menuAxisBoth.isEnabled());
  }

  /**
   * Set the MRU etomo data file list.  This fills in the MRU menu items
   * on the File menu
   */
  void setMRUFileLabels(String[] mRUList) {
    for (int i = 0; i < mRUList.length; i++) {
      if (i == nMRUFileMax) {
        return;
      }
      if (mRUList[i].equals("")) {
        menuMRUList[i].setVisible(false);
      }
      else {
        menuMRUList[i].setText(mRUList[i]);
        menuMRUList[i].setVisible(true);
      }
    }
    for (int i = mRUList.length; i < nMRUFileMax; i++) {
      menuMRUList[i].setVisible(false);
    }
  }
  
  final boolean isMenu3dmodStartupWindow() {
    return menu3dmodStartupWindow.isSelected();
  }
  
  final boolean isMenu3dmodBinBy2() {
    return menu3dmodBinBy2.isSelected();
  }
  
  final void setMenu3dmodStartupWindow(boolean menu3dmodStartupWindow) {
    this.menu3dmodStartupWindow.setSelected(menu3dmodStartupWindow);
  }
  
  final void setMenu3dmodBinBy2(boolean menu3dmodBinBy2) {
    this.menu3dmodBinBy2.setSelected(menu3dmodBinBy2);
  }
  
  void doClickFileExit() {
    menuFileExit.doClick();
  }
  
  void setEnabledFileNewTomogram(boolean enable) {
    menuFileNewTomogram.setEnabled(enable);
  }
  
  void setEnabledFileNewJoin(boolean enable) {
    menuFileNewJoin.setEnabled(enable);
  }
  
  final boolean equalsFileNewTomogram(ActionEvent event) {
    return equals(menuFileNewTomogram, event);
  }

  final boolean equalsFileNewJoin(ActionEvent event) {
    return equals(menuFileNewJoin, event);
  }

  final boolean equalsFileOpen(ActionEvent event) {
    return equals(menuFileOpen, event);
  }

  final boolean equalsFileSave(ActionEvent event) {
    return equals(menuFileSave, event);
  }

  final boolean equalsFileSaveAs(ActionEvent event) {
    return equals(menuFileSaveAs, event);
  }

  final boolean equalsFileClose(ActionEvent event) {
    return equals(menuFileClose, event);
  }

  final boolean equalsFileExit(ActionEvent event) {
    return equals(menuFileExit, event);
  }
  
  final boolean equalsFileTomosnapshot(ActionEvent event) {
    return equals(menuFileTomosnapshot, event);
  }
  
  final boolean equalsSettings(ActionEvent event) {
    return equals(menuSettings, event);
  }
  
  final boolean equalsAxisA(ActionEvent event) {
    return equals(menuAxisA, event);
  }
  
  final boolean equalsAxisB(ActionEvent event) {
    return equals(menuAxisB, event);
  }
  
  final boolean equalsAxisBoth(ActionEvent event) {
    return equals(menuAxisBoth, event);
  }
  
  final boolean equals3dmodStartUpWindow(ActionEvent event) {
    return equals(menu3dmodStartupWindow, event);
  }
  
  private final boolean equals(JMenuItem menuItem, ActionEvent event) {
    return menuItem.getActionCommand().equals(event.getActionCommand());
  }
  
  final boolean equals3dmodBinBy2(ActionEvent event) {
    return equals(menu3dmodBinBy2, event);
  }
  
  final boolean equalsFitWindow(ActionEvent event) {
    return equals(menuFitWindow, event);
  }
  
  final boolean equalsTomoGuide(ActionEvent event) {
    return equals(menuTomoGuide, event);
  }

  final boolean equalsImodGuide(ActionEvent event) {
    return equals(menuImodGuide, event);
  }

  final boolean equals3dmodGuide(ActionEvent event) {
    return equals(menu3dmodGuide, event);
  }

  final boolean equalsEtomoGuide(ActionEvent event) {
    return equals(menuEtomoGuide, event);
  }
  
  final boolean equalsJoinGuide(ActionEvent event) {
    return equals(menuJoinGuide, event);
  }

  final boolean equalsHelpAbout(ActionEvent event) {
    return equals(menuHelpAbout, event);
  }

  //  File menu action listener
  class FileActionListener implements ActionListener {
    EtomoFrame adaptee;
    
    FileActionListener(EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.menuFileAction(event);
    }
  }

  //  MRU file list action listener
  class FileMRUListActionListener implements ActionListener {
    EtomoFrame adaptee;
    
    FileMRUListActionListener(EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.menuFileMRUListAction(event);
    }
  }

  // Options file action listener
  class OptionsActionListener implements ActionListener {
    EtomoFrame adaptee;
    
    OptionsActionListener(EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.menuOptionsAction(event);
    }
  }

  // Help file action listener
  class HelpActionListener implements ActionListener {
    EtomoFrame adaptee;
    
    HelpActionListener(EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.menuHelpAction(event);
    }
  }
}
/**
* <p> $Log$
* <p> Revision 1.5  2005/08/11 23:50:12  sueh
* <p> bug# 711  Add menu3dmodStartupWindow and menu3dmodBinBy2.  Add
* <p> is and set functions from menu3dmodStartupWindow and
* <p> menu3dmodBinBy2.  In menuOptionsAction() handle the coordination of
* <p> the two frame menus.
* <p>
* <p> Revision 1.4  2005/05/12 22:13:01  sueh
* <p> bug# 615 Change setEnabled(BaseManager) to handle a null
* <p> BaseManager.
* <p>
* <p> Revision 1.3  2005/04/27 02:15:39  sueh
* <p> bug# 615 Added setEnabled(EtomoMenu) to match the enabled settings
* <p> of one EtomoMenu against another.
* <p>
* <p> Revision 1.2  2005/04/25 21:05:43  sueh
* <p> bug# 615 Moved menu management and the coordination of the two EtomoFrames
* <p> from EtomoMenu to EtomoFrame.  This class only contains the listeners
* <p> and the code to build the physical menu.
* <p>
* <p> Revision 1.1  2005/04/20 01:43:35  sueh
* <p> bug# 615 Moved the menu functionality from MainFrame to this class.
* <p> The class is designed to used by MainFrame and SubFrame.  The
* <p> listeners are defined in this class and the actions functions they call
* <p> are in the frame classes.
* <p> Added the ability to call functions in both MainFrame and SubFrame from
* <p> one menu click.  This is used only for fitting.
* <p> Turned the class into a double-ton.  Added the ability to call functions in
* <p> all instances of the class.  This is used to keep the phyical menus
* <p> up-to-date.
* <p> </p>
*/