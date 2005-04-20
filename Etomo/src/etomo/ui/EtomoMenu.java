package etomo.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

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
  private JMenuItem[] menuMRUList = new JMenuItem[nMRUFileMax];

  private JMenu menuOptions = new JMenu("Options");
  private JMenuItem menuAxisA = new JMenuItem("Axis A", KeyEvent.VK_A);
  private JMenuItem menuAxisB = new JMenuItem("Axis B", KeyEvent.VK_B);
  private JMenuItem menuAxisBoth = new JMenuItem("Both Axes", KeyEvent.VK_2);
  private JMenuItem menuSettings = new JMenuItem("Settings", KeyEvent.VK_S);
  private JMenuItem menuFitWindow = new JMenuItem("Fit Window", KeyEvent.VK_F);

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
  
  private static EtomoFrame frame1 = null;
  private static EtomoFrame frame2 = null;
  private static EtomoMenu menu1 = null;
  private static EtomoMenu menu2 = null;
  
  private int menuNumber = 0;

  private EtomoMenu(int menuNumber) {
    this.menuNumber = menuNumber;
  }
  
  static EtomoMenu newEtomoMenu() {
    //Assign MainFrame's menu to one EtomoMenu member variable and assign
    //SubFrame to the other
    synchronized (MainFrame.class) {
      if (menu1 == null) {
        menu1 = new EtomoMenu(1);
        return menu1;
      }
      if (menu2 == null) {
        menu2 = new EtomoMenu(2);
        return menu2;
      }
      throw new IllegalStateException("There can be only two menus.");
    }
  }

  void createMenus(EtomoFrame frame) {
    //Assign MainFrame to one EtomoFrame member variable and assign SubFrame to
    //the other.
    synchronized (MainFrame.class) {
      if (frame1 == null) {
        frame1 = frame;
      }
      else if (frame2 == null) {
        frame2 = frame;
      }
      else {
        throw new IllegalStateException("There can be only two frames.\nframe=" + frame);
      }
    }
    
    SharedActionListener sharedActionListener = new SharedActionListener(this);
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

    OptionsActionListener optionsActionListener = new OptionsActionListener(
        frame);
    menuSettings.addActionListener(optionsActionListener);
    menuFitWindow.addActionListener(sharedActionListener);
    menuAxisA.addActionListener(optionsActionListener);
    menuAxisB.addActionListener(optionsActionListener);
    menuAxisBoth.addActionListener(optionsActionListener);

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
  
  private void sharedAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(menuFitWindow.getActionCommand())) {
      if (frame1 != null) {
        frame1.menuOptionsAction(event);
      }
      if (frame2 != null) {
        frame2.menuOptionsAction(event);
      }
    }
    else {
      throw new IllegalStateException("No such shared action exists:  event=" + event);
    }
  }
  
  JMenuBar getMenuBar() {
    return menuBar;
  }
  
  void setEnabled(BaseManager currentManager) {
    setEnabled(currentManager, false);
  }
  
  void setEnabled(BaseManager currentManager, boolean otherMenuDone) {
    if (!otherMenuDone) {
      //Also run this function in the other EtomoMenu instance
      if (menuNumber == 1 && menu2 != null) {
        menu2.setEnabled(currentManager, true);
      }
      else if (menuNumber == 2 && menu1 != null) {
        menu1.setEnabled(currentManager, true);
      }
    }
    
    menuFileSaveAs.setEnabled(currentManager.canChangeParamFileName());
    boolean dualAxis = currentManager.getBaseMetaData().getAxisType() == AxisType.DUAL_AXIS;
    menuAxisA.setEnabled(dualAxis);
    menuAxisB.setEnabled(dualAxis);
    menuAxisBoth.setEnabled(dualAxis);
  }
  
  void setMRUFileLabels(String[] mRUList) {
    setMRUFileLabels(mRUList, false);
  }
  /**
   * Set the MRU etomo data file list.  This fills in the MRU menu items
   * on the File menu
   */
  void setMRUFileLabels(String[] mRUList, boolean otherMenuDone) {
    //Also run this function in the other EtomoMenu instance
    if (!otherMenuDone) {
      if (menuNumber == 1 && menu2 != null) {
        menu2.setMRUFileLabels(mRUList, true);
      }
      else if (menuNumber == 2 && menu1 != null) {
        menu1.setMRUFileLabels(mRUList, true);
      }
    }
    
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
  
  void doClickFileExit() {
    menuFileExit.doClick();
  }
  
  void setEnabledFileNewTomogram(boolean enable) {
    menuFileNewTomogram.setEnabled(enable);
  }
  
  void setEnabledFileNewJoin(boolean enable) {
    menuFileNewJoin.setEnabled(enable);
  }
  
  String getActionCommandFileNewTomogram() {
    return menuFileNewTomogram.getActionCommand();
  }

  String getActionCommandFileNewJoin() {
    return menuFileNewJoin.getActionCommand();
  }

  String getActionCommandFileOpen() {
    return menuFileOpen.getActionCommand();
  }

  String getActionCommandFileSave() {
    return menuFileSave.getActionCommand();
  }

  String getActionCommandFileSaveAs() {
    return menuFileSaveAs.getActionCommand();
  }

  String getActionCommandFileClose() {
    return menuFileClose.getActionCommand();
  }

  String getActionCommandFileExit() {
    return menuFileExit.getActionCommand();
  }
  
  String getActionCommandSettings() {
    return menuSettings.getActionCommand();
  }
  
  String getActionCommandAxisA() {
    return menuAxisA.getActionCommand();
  }
  
  String getActionCommandAxisB() {
    return menuAxisB.getActionCommand();
  }
  
  String getActionCommandAxisBoth() {
    return menuAxisBoth.getActionCommand();
  }
  
  String getActionCommandFitWindow() {
    return menuFitWindow.getActionCommand();
  }
  
  String getActionCommandTomoGuide() {
    return menuTomoGuide.getActionCommand();
  }

  String getActionCommandImodGuide() {
    return menuImodGuide.getActionCommand();
  }

  String getActionCommand3dmodGuide() {
    return menu3dmodGuide.getActionCommand();
  }

  String getActionCommandEtomoGuide() {
    return menuEtomoGuide.getActionCommand();
  }
  
  String getActionCommandJoinGuide() {
    return menuJoinGuide.getActionCommand();
  }

  String getActionCommandHelpAbout() {
    return menuHelpAbout.getActionCommand();
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
  
  class SharedActionListener implements ActionListener {
    EtomoMenu adaptee;
    
    SharedActionListener(EtomoMenu adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.sharedAction(event);
    }
  }

}
/**
* <p> $Log$ </p>
*/