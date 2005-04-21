package etomo.ui;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.DataFileFilter;

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
public class EtomoFrame extends JFrame {
  public static  final String  rcsid =  "$Id$";

  protected EtomoMenu menu;
  protected JMenuBar menuBar;
  protected MainPanel mainPanel;
  protected BaseManager currentManager;
  
  /**
   * Handle File menu actions
   * @param event
   */
  void menuFileAction(ActionEvent event) {
    if (event.getActionCommand().equals(menu.getActionCommandFileNewTomogram())) {
      EtomoDirector.getInstance().openTomogram(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileNewJoin())) {
      EtomoDirector.getInstance().openJoin(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileOpen())) {
      File dataFile = openDataFileDialog();
      if (dataFile != null) {
        EtomoDirector.getInstance().openManager(dataFile, true);
      }
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileSave())) {
      //  Check to see if there is a current parameter file chosen
      //  if not open a dialog box to select the name
      boolean haveTestParamFilename = true;
      if (currentManager.getTestParamFile() == null) {
        haveTestParamFilename = getTestParamFilename();
      }
      if (haveTestParamFilename) {
        currentManager.saveTestParamFile();
      }
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileSaveAs())) {
      boolean haveTestParamFilename = getTestParamFilename();
      if (haveTestParamFilename) {
        currentManager.saveTestParamFile();
      }
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileClose())) {
      EtomoDirector.getInstance().closeCurrentManager();
    }

    if (event.getActionCommand().equals(menu.getActionCommandFileExit())) {
      //  Check to see if we need to save any data
      if (EtomoDirector.getInstance().exitProgram()) {
        System.exit(0);
      }
    }
  }
  
  /**
   * Open the specified MRU EDF file
   * @param event
   */
  void menuFileMRUListAction(ActionEvent event) {
    EtomoDirector.getInstance().openManager(new File(event.getActionCommand()),
        true);
  }
  
  /**
   * Handle help menu actions
   * @param event
   */
  void menuHelpAction(ActionEvent event) {
    // Get the URL to the IMOD html directory
    String imodURL = "";
    try {
      imodURL = EtomoDirector.getInstance().getIMODDirectory().toURL().toString()
          + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.getInstance().getIMODDirectory().toString());
      return;
    }

    if (event.getActionCommand().equals(menu.getActionCommandTomoGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomoguide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandImodGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "guide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommand3dmodGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "3dmodguide.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandEtomoGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "UsingEtomo.html");
      manpage.setVisible(true);
    }
    
    if (event.getActionCommand().equals(menu.getActionCommandJoinGuide())) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "tomojoin.html");
      manpage.setVisible(true);
    }

    if (event.getActionCommand().equals(menu.getActionCommandHelpAbout())) {
      MainFrame_AboutBox dlg = new MainFrame_AboutBox(this);
      Dimension dlgSize = dlg.getPreferredSize();
      Dimension frmSize = getSize();
      Point loc = getLocation();
      dlg.setLocation((frmSize.width - dlgSize.width) / 2 + loc.x,
          (frmSize.height - dlgSize.height) / 2 + loc.y);
      dlg.setModal(true);
      dlg.setVisible(true);
      //dlg.show();
    }
  }

  /**
   * Handle some of the options menu events.  Axis switch events should be
   * handled in the child classes.
   * @param event
   */
  void menuOptionsAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(menu.getActionCommandSettings())) {
      EtomoDirector.getInstance().openSettingsDialog();
    }
    else if (command.equals(menu.getActionCommandFitWindow())) {
      fitWindow(true);
    }
    else {
      throw new IllegalStateException("Cannot handled menu command in this class.  command="+command);
    }
  }
  
  public void fitWindow() {
    fitWindow(false);
  }
  
  /**
   * fit window to its components and to the screen
   *
   */
  public void fitWindow(boolean force) {
    if (!force && !EtomoDirector.getInstance().getUserConfiguration().isAutoFit()) {
      setVisible(true);
      //EtomoDirector.getInstance().getMainFrame().show();
    }
    else {
      pack();
    }
  }
    
  /**
   * Open a file chooser to get an .edf or .ejf file.
   * @return
   */
  public boolean getTestParamFilename() {
    //  Open up the file chooser in current working directory
    File workingDir = new File(currentManager.getPropertyUserDir());
    JFileChooser chooser =
      new JFileChooser(workingDir);
    DataFileFilter fileFilter = mainPanel.getDataFileFilter();
    chooser.setFileFilter(fileFilter);
    chooser.setDialogTitle("Save " + fileFilter.getDescription());
    chooser.setDialogType(JFileChooser.SAVE_DIALOG);
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    File[] edfFiles = workingDir.listFiles(fileFilter);
    if (edfFiles.length == 0) {
      File defaultFile = new File(workingDir, currentManager.getBaseMetaData().getMetaDataFileName());
      chooser.setSelectedFile(defaultFile);
    }
    int returnVal = chooser.showSaveDialog(this);

    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return false;
    }
    // If the file does not already have an extension appended then add an edf
    // extension
    File dataFile = chooser.getSelectedFile();
    String fileName = chooser.getSelectedFile().getName();
    if (fileName.indexOf(".") == -1) {
      dataFile = new File(chooser.getSelectedFile().getAbsolutePath() + currentManager.getBaseMetaData().getFileExtension());

    }
    currentManager.setTestParamFile(dataFile);
    return true;
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
  
  /**
   * Create the Etomo menus
   */
  public void createMenus() {
    menu.createMenus(this);
    menuBar = menu.getMenuBar();
    setJMenuBar(menuBar);
  }
  
  /**
   * Open a File Chooser dialog with an data file filter, if the user selects
   * or names a file return a File object wiht that slected, otherwise
   * return null.
   * @return A File object specifiying the selected file or null if none
   * was selected. 
   */
  private File openDataFileDialog() {
    //  Open up the file chooser in current working directory
    JFileChooser chooser = new JFileChooser(new File(System
        .getProperty("user.dir")));
    DataFileFilter fileFilter = new DataFileFilter();
    chooser.setFileFilter(fileFilter);

    chooser.setDialogTitle("Open " + fileFilter.getDescription());
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(this);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      return chooser.getSelectedFile();
    }
    return null;
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/04/20 01:37:15  sueh
* <p> bug# 615 Added a interface for MainFrame and SubFrame to they can
* <p> work with the same menu class.
* <p> </p>
*/