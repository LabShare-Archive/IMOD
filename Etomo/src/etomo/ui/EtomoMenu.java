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
public final class EtomoMenu {
  public static final String rcsid = "$Id$";

  private static final int nMRUFileMax = 10;

  private final JMenuBar menuBar = new JMenuBar();

  private final JMenu menuFile = new Menu("File");
  private final JMenuItem menuFileNewTomogram = new MenuItem("New Tomogram",
      KeyEvent.VK_N);
  private final JMenuItem menuFileNewJoin = new MenuItem("New Join",
      KeyEvent.VK_J);
  private final JMenuItem menuFileNewParallel = new MenuItem(
      "New Parallel Process", KeyEvent.VK_P);
  private final JMenuItem menuFileNewPeet = new MenuItem("New PEET",
      KeyEvent.VK_E);
  private final JMenuItem menuFileOpen = new MenuItem("Open...", KeyEvent.VK_O);
  private final JMenuItem menuFileSave = new MenuItem("Save", KeyEvent.VK_S);
  private final JMenuItem menuFileSaveAs = new MenuItem("Save As",
      KeyEvent.VK_A);
  private final JMenuItem menuFileClose = new MenuItem("Close", KeyEvent.VK_C);
  private final JMenuItem menuFileExit = new MenuItem("Exit", KeyEvent.VK_X);
  private final JMenuItem menuFileTomosnapshot = new MenuItem(
      "Run Tomosnapshot", KeyEvent.VK_R);
  private final JMenuItem[] menuMRUList = new MenuItem[nMRUFileMax];

  private final JMenu menuOptions = new Menu("Options");
  private final JMenuItem menuAxisA = new MenuItem("Axis A", KeyEvent.VK_A);
  private final JMenuItem menuAxisB = new MenuItem(TomogramProcessPanel.AXIS_B_LABEL, KeyEvent.VK_B);
  private final JMenuItem menuAxisBoth = new MenuItem("Both Axes",
      KeyEvent.VK_2);
  private final JMenuItem menuSettings = new MenuItem("Settings",
      KeyEvent.VK_S);
  private final JMenuItem menuFitWindow = new MenuItem("Fit Window",
      KeyEvent.VK_F);
  private final JCheckBoxMenuItem menu3dmodStartupWindow = new CheckBoxMenuItem(
      "Open 3dmod with Startup Window");
  private final JCheckBoxMenuItem menu3dmodBinBy2 = new CheckBoxMenuItem(
      "Open 3dmod Binned by 2");

  private final JMenu menuHelp = new Menu("Help");
  private final JMenuItem menuTomoGuide = new MenuItem("Tomography Guide",
      KeyEvent.VK_T);
  private final JMenuItem menuImodGuide = new MenuItem("Imod Users Guide",
      KeyEvent.VK_I);
  private final JMenuItem menu3dmodGuide = new MenuItem("3dmod Users Guide",
      KeyEvent.VK_3);
  private final JMenuItem menuEtomoGuide = new MenuItem("Etomo Users Guide",
      KeyEvent.VK_E);
  private final JMenuItem menuJoinGuide = new MenuItem("Join Users Guide",
      KeyEvent.VK_J);
  private final JMenuItem menuHelpAbout = new MenuItem("About", KeyEvent.VK_A);

  void createMenus(final EtomoFrame frame) {
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
    menuFileNewParallel.addActionListener(fileActionListener);
    menuFileNewPeet.addActionListener(fileActionListener);
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
    menuFile.add(menuFileNewParallel);
    menuFile.add(menuFileNewPeet);
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
      menuMRUList[i] = new MenuItem();
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
  void setEnabled(final BaseManager currentManager) {
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
   * Enable/disable menu items based on main Frame Menu.
   * Only used by the subframe
   * @param otherMenu
   */
  void setEnabled(final EtomoMenu mainFrameMenu) {
    menuFileNewTomogram.setEnabled(mainFrameMenu.menuFileNewTomogram
        .isEnabled());
    menuFileNewJoin.setEnabled(mainFrameMenu.menuFileNewJoin.isEnabled());
    menuFileNewParallel.setEnabled(mainFrameMenu.menuFileNewParallel
        .isEnabled());
    menuFileNewPeet.setEnabled(mainFrameMenu.menuFileNewPeet.isEnabled());
    menuFileSaveAs.setEnabled(mainFrameMenu.menuFileSaveAs.isEnabled());
    menuFileTomosnapshot.setEnabled(menuFileTomosnapshot.isEnabled());
    menuAxisA.setEnabled(mainFrameMenu.menuAxisA.isEnabled());
    menuAxisB.setEnabled(mainFrameMenu.menuAxisB.isEnabled());
    menuAxisBoth.setEnabled(mainFrameMenu.menuAxisBoth.isEnabled());
  }

  /**
   * Set the MRU etomo data file list.  This fills in the MRU menu items
   * on the File menu
   */
  void setMRUFileLabels(final String[] mRUList) {
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

  boolean isMenu3dmodStartupWindow() {
    return menu3dmodStartupWindow.isSelected();
  }

  boolean isMenu3dmodBinBy2() {
    return menu3dmodBinBy2.isSelected();
  }

  void setMenu3dmodStartupWindow(final boolean menu3dmodStartupWindow) {
    this.menu3dmodStartupWindow.setSelected(menu3dmodStartupWindow);
  }

  void setMenu3dmodBinBy2(final boolean menu3dmodBinBy2) {
    this.menu3dmodBinBy2.setSelected(menu3dmodBinBy2);
  }

  void doClickFileExit() {
    menuFileExit.doClick();
  }

  void setEnabledFileNewTomogram(final boolean enable) {
    menuFileNewTomogram.setEnabled(enable);
  }

  void setEnabledFileNewJoin(final boolean enable) {
    menuFileNewJoin.setEnabled(enable);
  }

  void setEnabledFileNewParallel(final boolean enable) {
    menuFileNewParallel.setEnabled(enable);
  }

  void setEnabledFileNewPeet(final boolean enable) {
    menuFileNewPeet.setEnabled(enable);
  }

  boolean equalsFileNewTomogram(final ActionEvent event) {
    return equals(menuFileNewTomogram, event);
  }

  boolean equalsFileNewJoin(final ActionEvent event) {
    return equals(menuFileNewJoin, event);
  }

  boolean equalsFileNewParallel(final ActionEvent event) {
    return equals(menuFileNewParallel, event);
  }

  boolean equalsFileNewPeet(final ActionEvent event) {
    return equals(menuFileNewPeet, event);
  }

  boolean equalsFileOpen(final ActionEvent event) {
    return equals(menuFileOpen, event);
  }

  boolean equalsFileSave(final ActionEvent event) {
    return equals(menuFileSave, event);
  }

  boolean equalsFileSaveAs(final ActionEvent event) {
    return equals(menuFileSaveAs, event);
  }

  boolean equalsFileClose(final ActionEvent event) {
    return equals(menuFileClose, event);
  }

  boolean equalsFileExit(final ActionEvent event) {
    return equals(menuFileExit, event);
  }

  boolean equalsFileTomosnapshot(final ActionEvent event) {
    return equals(menuFileTomosnapshot, event);
  }

  boolean equalsSettings(final ActionEvent event) {
    return equals(menuSettings, event);
  }

  boolean equalsAxisA(final ActionEvent event) {
    return equals(menuAxisA, event);
  }

  boolean equalsAxisB(final ActionEvent event) {
    return equals(menuAxisB, event);
  }

  boolean equalsAxisBoth(final ActionEvent event) {
    return equals(menuAxisBoth, event);
  }

  boolean equals3dmodStartUpWindow(final ActionEvent event) {
    return equals(menu3dmodStartupWindow, event);
  }

  boolean equals3dmodBinBy2(final ActionEvent event) {
    return equals(menu3dmodBinBy2, event);
  }

  boolean equalsFitWindow(final ActionEvent event) {
    return equals(menuFitWindow, event);
  }

  boolean equalsTomoGuide(final ActionEvent event) {
    return equals(menuTomoGuide, event);
  }

  boolean equalsImodGuide(final ActionEvent event) {
    return equals(menuImodGuide, event);
  }

  boolean equals3dmodGuide(final ActionEvent event) {
    return equals(menu3dmodGuide, event);
  }

  boolean equalsEtomoGuide(final ActionEvent event) {
    return equals(menuEtomoGuide, event);
  }

  boolean equalsJoinGuide(final ActionEvent event) {
    return equals(menuJoinGuide, event);
  }

  boolean equalsHelpAbout(final ActionEvent event) {
    return equals(menuHelpAbout, event);
  }

  private boolean equals(final JMenuItem menuItem, final ActionEvent event) {
    return menuItem.getActionCommand().equals(event.getActionCommand());
  }

  //  File menu action listener
  private static final class FileActionListener implements ActionListener {
    private EtomoFrame adaptee;

    private FileActionListener(final EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.menuFileAction(event);
    }
  }

  //  MRU file list action listener
  private static final class FileMRUListActionListener implements
      ActionListener {
    private EtomoFrame adaptee;

    private FileMRUListActionListener(final EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.menuFileMRUListAction(event);
    }
  }

  // Options file action listener
  private static final class OptionsActionListener implements ActionListener {
    private EtomoFrame adaptee;

    private OptionsActionListener(final EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.menuOptionsAction(event);
    }
  }

  // Help file action listener
  private static final class HelpActionListener implements ActionListener {
    private EtomoFrame adaptee;

    private HelpActionListener(final EtomoFrame adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.menuHelpAction(event);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.12  2008/01/14 22:04:24  sueh
 * <p> bug# 1050 Moved string "Axis B" to TomogramProcessPanel.
 * <p>
 * <p> Revision 1.11  2007/05/02 21:07:33  sueh
 * <p> bug# 964 Removed Import PRM and Duplicate PEET menu items.
 * <p>
 * <p> Revision 1.10  2007/05/02 16:34:46  sueh
 * <p> bug# 964 Moved newstuff into mainstream.
 * <p>
 * <p> Revision 1.9  2007/03/31 03:00:38  sueh
 * <p> bug# 964 Added Duplicate Peet and Import .prm File menu items.
 * <p>
 * <p> Revision 1.8  2007/02/19 22:01:28  sueh
 * <p> bug# 964 Added New PEET option.
 * <p>
 * <p> Revision 1.7  2006/03/20 18:02:27  sueh
 * <p> bug# 835 Added menu option to create a new ParallelManager.
 * <p>
 * <p> Revision 1.6  2005/12/09 20:30:19  sueh
 * <p> bug# 776 Added menuFileTomosnapshot.
 * <p>
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
