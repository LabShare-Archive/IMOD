package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Vector;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.ImodqtassistProcess;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.15  2009/01/20 19:51:31  sueh
 * <p> bug# 1102 Changed menu items to type MenuItem so that they can
 * <p> name themselves.
 * <p>
 * <p> Revision 3.14  2008/03/19 01:01:25  sueh
 * <p> bug# 1099 TabbedTextWindow.openFiles return boolean.
 * <p>
 * <p> Revision 3.13  2007/11/12 22:13:53  sueh
 * <p> bug# 1047 Added an ContextPopup constructor with a subdir name string to
 * <p> popup a .log file in a subdirectory.
 * <p>
 * <p> Revision 3.12  2007/09/07 00:26:32  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.11  2007/06/06 16:58:37  sueh
 * <p> bug# 1015 Added ContextPopup constructor with no guide anchor.
 * <p> Handled a null guide in globalItemAction().
 * <p>
 * <p> Revision 3.10  2006/06/21 15:51:43  sueh
 * <p> bug# 581 Using Imodqtassist instead of HTMLPageWindow() to pop up help.
 * <p>
 * <p> Revision 3.9  2005/11/14 21:46:24  sueh
 * <p> bug# 762 The internal class is now accessing protected functions instead
 * <p> of private variables.
 * <p>
 * <p> Revision 3.8  2005/05/20 21:18:16  sueh
 * <p> bug# 664 Attempting to recover from OutOfMemoryError, version 2:
 * <p> call logFileWindow.dispose() when an OutOfMemoryError is caught.
 * <p> There is no guarentee that this will solve the problem or that the error will
 * <p> be caught in this place.  Tell the user to close windows or Etomo and
 * <p> rethrow the error.
 * <p>
 * <p> Revision 3.7  2005/05/20 03:22:38  sueh
 * <p> bug# 664 Attempting to recover from an OutOfMemoryError in the
 * <p> ContextPopup which displays the fine alignment tabbed text window.
 * <p>
 * <p> Revision 3.6  2005/03/24 17:51:29  sueh
 * <p> bug# 621 Added a constructor to make a popup menu with only the
 * <p> standard items and an anchor into one of the guides.
 * <p>
 * <p> Revision 3.5  2004/12/03 02:32:25  sueh
 * <p> bug# 566 Corrected JOIN_GUIDE.  Added name of guide to be anchored
 * <p> in globalItemAction() so that it will use the anchor when it matches what
 * <p> the user chose.
 * <p>
 * <p> Revision 3.4  2004/12/02 20:39:09  sueh
 * <p> bug# 566 Changed ContextPopup to specify an anchor in both the t
 * <p> omo guide and the join guide.  Passing in guideToAnchor to the
 * <p> ContextPopup constructor that handles man pages and logs, and the
 * <p> constructor that handles man pages.
 * <p>
 * <p> Revision 3.3  2004/11/19 23:50:26  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.2.4.2  2004/10/11 02:12:11  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.  Passed the manager to the
 * <p> ContextPopup object in order to get the propertyUserDir.
 * <p>
 * <p> Revision 3.2.4.1  2004/10/08 16:25:55  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 3.2  2004/06/05 00:55:28  sueh
 * <p> bug# 433 call ApplicationManager.updateLog() when the command equals
 * <p> the updateLogCommandName
 * <p>
 * <p> Revision 3.1  2003/11/10 07:42:08  rickg
 * <p> No longer needs to be initialized with applicationManager since
 * <p> getIMODDirectory is static
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.9  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p> Revision 2.8  2003/10/23 17:05:43  rickg
 * <p> Bug# 257 Added IMOD and eTomo guides to generic menu
 * <p>
 * <p> Revision 2.7  2003/06/04 23:39:50  rickg
 * <p> Added independent labels for tabs
 * <p>
 * <p> Revision 2.6  2003/05/27 08:54:52  rickg
 * <p> Added constructor to open tabbed editor pane
 * <p>
 * <p> Revision 2.5  2003/05/23 22:14:38  rickg
 * <p> Put log files before man pages in context menu
 * <p>
 * <p> Revision 2.4  2003/05/12 01:27:13  rickg
 * <p> Keep imod URL as a static object, don't need appManager
 * <p> Windows compliant now
 * <p>
 * <p> Revision 2.3  2003/05/10 19:12:42  rickg
 * <p> OS independent path implementation
 * <p>
 * <p> Revision 2.2  2003/05/07 17:51:09  rickg
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.1  2003/03/20 17:43:32  rickg
 * <p> Only display window if URL is sucessfully opened
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/11/14 04:41:46  rickg
 * <p> Had to move imodURL inside of inner class
 * <p>
 * <p> Revision 1.2  2002/11/14 04:22:31  rickg
 * <p> HTMLPage and ContextPopup now work with URLS
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ContextPopup {
  public static final String rcsid = "$Id$";

  public static final String TOMO_GUIDE = "tomoguide.html";
  public static final String JOIN_GUIDE = "tomojoin.html";

  private JPopupMenu contextMenu = new JPopupMenu("Help Documents");
  private JMenuItem[] manPageItem;
  private JMenuItem[] logFileItem;
  private JMenuItem tomoGuideItem = new MenuItem("Tomography Guide ...");
  private JMenuItem modelGuideItem = new MenuItem("IMOD Users Guide ...");
  private JMenuItem it3dmodGuide = new MenuItem("3dmod Users Guide ...");
  private JMenuItem etomoGuideItem = new MenuItem("Etomo Users Guide ...");
  private JMenuItem joinGuideItem = new MenuItem("Join Users Guide ...");
  private ActionListener actionListener;
  private MouseEvent mouseEvent;

  private String[] manPageName;
  private String[] logFileName;

  private String imodURL;
  private String anchor;

  /**
   * Simple context popup constructor.  Only the default menu items are
   * displayed.
   * @param component The component to which the popup is attached.
   * @param mouseEvent The mouse event that opened the menu.
   * @param tomoAnchor The tomography guide HTML anchor for the current popup.
   */
  public ContextPopup(Component component, MouseEvent mouseEvent,
      String tomoAnchor, final BaseManager manager, final AxisID axisID) {

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String tomoGuideLocation = "tomoguide.html";
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          tomoGuideLocation += "#" + anchor;
        }
        globalItemAction(actionEvent, tomoGuideLocation, manager, axisID);
        setVisible(false);
      }
    };

    // add the menu items
    addStandardMenuItems();
    showMenu(component);
  }

  /**
   * Constructor to show a man page list in addition to the the standard menu
   * items.
   * @param component The component to which the popup is attached.
   * @param mouseEvent The mouse event that opened the menu.
   * @param tomoAnchor The tomography guide HTML anchor for the current popup.
   * @param manPageLabel The string array of man page labels for the menu.
   * @param manPage The name of the HTML man pages.
   */
  public ContextPopup(Component component, MouseEvent mouseEvent,
      String tomoAnchor, final String guideToAnchor, String[] manPageLabel,
      String[] manPage, final BaseManager manager, final AxisID axisID) {

    // Check to make sure that the menu label and man page arrays are the same
    // length
    if (manPageLabel.length != manPage.length) {
      String message = "menu label and man page arrays must be the same length";
      throw new IllegalArgumentException(message);
    }

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }

        for (int i = 0; i < getManPageItem().length; i++) {
          if (actionEvent.getActionCommand() == getManPageItem()[i].getText()) {
            ImodqtassistProcess.INSTANCE.open(manager, "man/"
                + getManPageName()[i], axisID);
          }
        }

        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager,
            axisID);

        //  Close the menu
        setVisible(false);
      }
    };

    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems();
    showMenu(component);
  }

  /**
   * Constructor to show the standard items with a anchor into one of the guides.
   * @param component
   * @param mouseEvent
   * @param tomoAnchor
   * @param guideToAnchor
   */
  public ContextPopup(Component component, MouseEvent mouseEvent,
      String tomoAnchor, final String guideToAnchor, final BaseManager manager,
      final AxisID axisID) {

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager,
            axisID);
        //  Close the menu
        setVisible(false);
      }
    };

    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems();
    showMenu(component);
  }

  /**
   * Constructor to show a man page list and log file items in addition to the
   * the standard menu items.
   * @param component The component to which the popup is attached.
   * @param mouseEvent The mouse event that opened the menu.
   * @param tomoAnchor The guide HTML anchor for the current popup.
   * @param guideToAnchor The guide containing the HTML anchor.
   * @param manPageLabel The string array of man page labels for the menu.
   * @param manPage The name of the HTML man pages.
   * @param logFileLabel The string arrays of log file labels for the menu.
   * @param logFile The string arrays of names of the log files.
   * @param manager
   */
  public ContextPopup(Component component, MouseEvent mouseEvent,
      String tomoAnchor, final String guideToAnchor, String[] manPageLabel,
      String[] manPage, String[] logFileLabel, String[] logFile,
      final BaseManager manager, final AxisID axisID) {

    // Check to make sure that the menu label and man page arrays are the same
    // length
    if (manPageLabel.length != manPage.length) {
      String message = "menu label and man page arrays must be the same length";
      throw new IllegalArgumentException(message);
    }
    if (logFileLabel.length != logFile.length) {
      String message = "log file label and log file arrays must be the same length";
      throw new IllegalArgumentException(message);
    }

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /*HTMLPageWindow manpage = new HTMLPageWindow();
             manpage.openURL(getImodURL() + "man/" + getManPageName()[i]);
             manpage.setVisible(true);*/
            ImodqtassistProcess.INSTANCE.open(manager, "man/"
                + getManPageName()[i], axisID);
          }
        }

        //  Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager
                .getPropertyUserDir()
                + File.separator + getLogFileName()[i]));
          }
        }

        //  Search the standard items
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager,
            axisID);

        //  Close the  the menu
        setVisible(false);
      }
    };

    addLogFileMenuItems(logFileLabel, logFile);
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems();
    showMenu(component);
  }

  /**
   * Constructor to show a man page list and log file items in addition to the
   * the standard menu items.
   * @param component The component to which the popup is attached.
   * @param mouseEvent The mouse event that opened the menu.
   * @param tomoAnchor The guide HTML anchor for the current popup.
   * @param guideToAnchor The guide containing the HTML anchor.
   * @param manPageLabel The string array of man page labels for the menu.
   * @param manPage The name of the HTML man pages.
   * @param logFileLabel The string arrays of log file labels for the menu.
   * @param logFile The string arrays of names of the log files.
   * @param subdirName The subdirectory where the .log files are.
   * @param manager
   */
  public ContextPopup(Component component, MouseEvent mouseEvent,
      String tomoAnchor, final String guideToAnchor, String[] manPageLabel,
      String[] manPage, String[] logFileLabel, String[] logFile,
      final BaseManager manager, final AxisID axisID, final String subdirName) {

    // Check to make sure that the menu label and man page arrays are the same
    // length
    if (manPageLabel.length != manPage.length) {
      String message = "menu label and man page arrays must be the same length";
      throw new IllegalArgumentException(message);
    }
    if (logFileLabel.length != logFile.length) {
      String message = "log file label and log file arrays must be the same length";
      throw new IllegalArgumentException(message);
    }

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /*HTMLPageWindow manpage = new HTMLPageWindow();
             manpage.openURL(getImodURL() + "man/" + getManPageName()[i]);
             manpage.setVisible(true);*/
            ImodqtassistProcess.INSTANCE.open(manager, "man/"
                + getManPageName()[i], axisID);
          }
        }

        //  Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager
                .getPropertyUserDir()
                + (subdirName != null ? File.separator + subdirName : "")
                + File.separator + getLogFileName()[i]));
          }
        }

        //  Search the standard items
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager,
            axisID);

        //  Close the  the menu
        setVisible(false);
      }
    };

    addLogFileMenuItems(logFileLabel, logFile);
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems();
    showMenu(component);
  }

  /**
   * Constructor to show a man page list and log file items in addition to the
   * the standard menu items.
   * @param component The component to which the popup is attached.
   * @param mouseEvent The mouse event that opened the menu.
   * @param tomoAnchor The guide HTML anchor for the current popup.
   * @param guideToAnchor The guide containing the HTML anchor.
   * @param manPageLabel The string array of man page labels for the menu.
   * @param manPage The name of the HTML man pages.
   * @param logFileLabel The string arrays of log file labels for the menu.
   * @param logFile The string arrays of names of the log files.
   * @param manager
   */
  public ContextPopup(Component component, MouseEvent mouseEvent,
      String[] manPageLabel, String[] manPage, String[] logFileLabel,
      String[] logFile, final BaseManager manager, final AxisID axisID) {

    // Check to make sure that the menu label and man page arrays are the same
    // length
    if (manPageLabel.length != manPage.length) {
      String message = "menu label and man page arrays must be the same length";
      throw new IllegalArgumentException(message);
    }
    if (logFileLabel.length != logFile.length) {
      String message = "log file label and log file arrays must be the same length";
      throw new IllegalArgumentException(message);
    }

    this.mouseEvent = mouseEvent;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String anchor = getAnchor();
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /*HTMLPageWindow manpage = new HTMLPageWindow();
             manpage.openURL(getImodURL() + "man/" + getManPageName()[i]);
             manpage.setVisible(true);*/
            ImodqtassistProcess.INSTANCE.open(manager, "man/"
                + getManPageName()[i], axisID);
          }
        }

        //  Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager
                .getPropertyUserDir()
                + File.separator + getLogFileName()[i]));
          }
        }

        //  Search the standard items
        globalItemAction(actionEvent, null, null, manager, axisID);

        //  Close the  the menu
        setVisible(false);
      }
    };

    addLogFileMenuItems(logFileLabel, logFile);
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems();
    showMenu(component);
  }

  /**
   * Constructor to show a man page list and tabbed log file items in addition
   * to the the standard menu items.
   * @param component The component to which the popup is attached.
   * @param mouseEvent The mouse event that opened the menu.
   * @param tomoAnchor The tomography guide HTML anchor for the current popup.
   * @param manPageLabel The string array of man page labels for the menu.
   * @param manPage The name of the HTML man pages.
   * @param logWindowLabel The window title for each tabbed window. 
   * @param logFileLabel The vector string arrays of log file labels for the
   * menu.
   * @param logFile The vector of string arrays of names of the log files.
   * @param applicationManager used to update the log file
   * @param updateLogCommandName name of the log that must be updated before it
   * is displayed
   * @param axisID used for updating the log file
   */
  public ContextPopup(Component component, MouseEvent mouseEvent,
      String tomoAnchor, String[] manPageLabel, String[] manPage,
      final String[] logWindowLabel, final Vector logFileLabel,
      final Vector logFile, final ApplicationManager applicationManager,
      final String updateLogCommandName, final AxisID axisID) {

    // Check to make sure that the menu label and man page arrays are the same
    // length
    if (manPageLabel.length != manPage.length) {
      String message = "menu label and man page arrays must be the same length";
      throw new IllegalArgumentException(message);
    }
    if (logFileLabel.size() != logFile.size()) {
      String message = "log file label and log file vectors must be the same length";
      throw new IllegalArgumentException(message);
    }

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {

      public void actionPerformed(ActionEvent actionEvent) {
        String tomoGuideLocation = "tomoguide.html";
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          tomoGuideLocation += "#" + anchor;
        }

        for (int i = 0; i < getManPageItem().length; i++) {
          if (actionEvent.getActionCommand() == getManPageItem()[i].getText()) {
            ImodqtassistProcess.INSTANCE.open(applicationManager, "man/"
                + getManPageName()[i], axisID);
          }
        }

        //  Search the logfile items
        for (int i = 0; i < getLogFileItem().length; i++) {
          if (actionEvent.getActionCommand() == getLogFileItem()[i].getText()) {
            if (actionEvent.getActionCommand().startsWith(updateLogCommandName)) {
              applicationManager.updateLog(updateLogCommandName, axisID);
            }
            //  Create full path to the appropriate log file items
            String[] logFileList = (String[]) logFile.get(i);
            String[] logFileFullPath = new String[logFileList.length];
            String path = applicationManager.getPropertyUserDir()
                + File.separator;
            for (int j = 0; j < logFileList.length; j++) {
              logFileFullPath[j] = path + logFileList[j];
            }
            TabbedTextWindow logFileWindow = new TabbedTextWindow(
                logWindowLabel[i], axisID);
            try {
              if (logFileWindow.openFiles(logFileFullPath,
                  (String[]) logFileLabel.get(i), axisID, applicationManager
                      .getManagerKey())) {
                logFileWindow.setVisible(true);
              }
              else {
                logFileWindow.dispose();
              }
            }
            catch (FileNotFoundException e) {
              e.printStackTrace();
              System.err.println("File not file exception: " + logFileFullPath);
            }
            catch (IOException e) {
              e.printStackTrace();
              System.err.println("IO exception: " + logFileFullPath);
            }
            catch (OutOfMemoryError e) {
              e.printStackTrace();
              if (logFileWindow != null) {
                logFileWindow.dispose();
              }
              UIHarness.INSTANCE.openMessageDialog(
                  "WARNING:  Ran out of memory.  Will not display log file."
                      + "\nPlease close open windows or exit Etomo.",
                  "Out of Memory", applicationManager.getManagerKey());
              throw e;
            }
          }
        }
        //  Search the standard items
        globalItemAction(actionEvent, tomoGuideLocation, applicationManager,
            axisID);

        //  Close the  the menu
        setVisible(false);
      }
    };

    addTabbedLogFileMenuItems(logWindowLabel);
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems();
    showMenu(component);
  }

  /**
   *
   */
  private void addStandardMenuItems() {
    //  Construct the context menu
    contextMenu.add(tomoGuideItem);
    tomoGuideItem.addActionListener(actionListener);
    contextMenu.add(modelGuideItem);
    modelGuideItem.addActionListener(actionListener);
    contextMenu.add(it3dmodGuide);
    it3dmodGuide.addActionListener(actionListener);
    contextMenu.add(etomoGuideItem);
    etomoGuideItem.addActionListener(actionListener);
    contextMenu.add(joinGuideItem);
    joinGuideItem.addActionListener(actionListener);
  }

  /**
   * 
   * @param manPageLabel
   * @param manPage
   */
  private void addManPageMenuItems(String[] manPageLabel, String manPage[]) {
    manPageItem = new MenuItem[manPageLabel.length];
    manPageName = new String[manPage.length];
    for (int i = 0; i < manPageItem.length; i++) {
      manPageItem[i] = new MenuItem();
      manPageItem[i].setText(manPageLabel[i] + " man page ...");
      manPageItem[i].addActionListener(actionListener);
      contextMenu.add(manPageItem[i]);
      manPageName[i] = manPage[i];
    }
  }

  protected void globalItemAction(ActionEvent actionEvent,
      String tomoGuideLocation, BaseManager manager, AxisID axisID) {
    globalItemAction(actionEvent, tomoGuideLocation, TOMO_GUIDE, manager,
        axisID);
  }

  /**
   * Open the appropriate file if the event is one of the global menu items 
   * @param actionEvent
   * @param tomoGuideLocation
   */
  protected void globalItemAction(ActionEvent actionEvent,
      String guideLocation, String guide, BaseManager manager, AxisID axisID) {
    if (actionEvent.getActionCommand() == tomoGuideItem.getText()) {
      /*HTMLPageWindow manpage = new HTMLPageWindow();
       if (guide.equals(TOMO_GUIDE)) {
       manpage.openURL(imodURL + guideLocation);
       }
       else {
       manpage.openURL(imodURL + TOMO_GUIDE);
       }
       manpage.setVisible(true);*/
      if (guide != null && guide.equals(TOMO_GUIDE)) {
        ImodqtassistProcess.INSTANCE.open(manager, guideLocation, axisID);
      }
      else {
        ImodqtassistProcess.INSTANCE.open(manager, TOMO_GUIDE, axisID);
      }
    }

    if (actionEvent.getActionCommand() == modelGuideItem.getText()) {
      /*HTMLPageWindow manpage = new HTMLPageWindow();
       manpage.openURL(imodURL + "guide.html");
       manpage.setVisible(true);*/
      ImodqtassistProcess.INSTANCE.open(manager, "guide.html", axisID);
    }

    if (actionEvent.getActionCommand() == it3dmodGuide.getText()) {
      /*HTMLPageWindow manpage = new HTMLPageWindow();
       manpage.openURL(imodURL + "3dmodguide.html");
       manpage.setVisible(true);*/
      ImodqtassistProcess.INSTANCE.open(manager, "3dmodguide.html", axisID);
    }

    if (actionEvent.getActionCommand() == etomoGuideItem.getText()) {
      /*HTMLPageWindow manpage = new HTMLPageWindow();
       manpage.openURL(imodURL + "UsingEtomo.html");
       manpage.setVisible(true);*/
      ImodqtassistProcess.INSTANCE.open(manager, "UsingEtomo.html", axisID);
    }

    if (actionEvent.getActionCommand() == joinGuideItem.getText()) {
      /*HTMLPageWindow manpage = new HTMLPageWindow();
       if (guide.equals(JOIN_GUIDE)) {
       manpage.openURL(imodURL + guideLocation);
       }
       else {
       manpage.openURL(imodURL + JOIN_GUIDE);
       }
       manpage.setVisible(true);*/
      if (guide != null && guide.equals(JOIN_GUIDE)) {
        ImodqtassistProcess.INSTANCE.open(manager, guideLocation, axisID);
      }
      else {
        ImodqtassistProcess.INSTANCE.open(manager, JOIN_GUIDE, axisID);
      }
    }
  }

  /**
   * 
   * @param logFileLabel
   * @param logFile
   */
  private void addLogFileMenuItems(String[] logFileLabel, String logFile[]) {
    logFileItem = new MenuItem[logFileLabel.length];
    logFileName = new String[logFile.length];
    for (int i = 0; i < logFileItem.length; i++) {
      logFileItem[i] = new MenuItem();
      logFileItem[i].setText(logFileLabel[i] + " log file ...");
      logFileItem[i].addActionListener(actionListener);
      contextMenu.add(logFileItem[i]);
      logFileName[i] = logFile[i];
    }
  }

  private void addTabbedLogFileMenuItems(String[] logWindowLabel) {
    logFileItem = new MenuItem[logWindowLabel.length];
    for (int i = 0; i < logFileItem.length; i++) {
      logFileItem[i] = new MenuItem();
      logFileItem[i].setText(logWindowLabel[i] + " log file ...");
      logFileItem[i].addActionListener(actionListener);
      contextMenu.add(logFileItem[i]);
    }

  }

  /**
   * 
   * @param component
   */
  private void showMenu(Component component) {
    contextMenu.show(component, mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }

  /**
   * Calculate the IMOD URL
   */
  private void calcImodURL() {
    try {
      imodURL = EtomoDirector.INSTANCE.getIMODDirectory().toURL().toString()
          + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.INSTANCE.getIMODDirectory().toString());
    }
  }

  protected final String getAnchor() {
    return anchor;
  }

  protected final void setVisible(boolean visible) {
    contextMenu.setVisible(visible);
  }

  protected final JMenuItem[] getManPageItem() {
    return manPageItem;
  }

  protected final String[] getManPageName() {
    return manPageName;
  }

  protected final String getImodURL() {
    return imodURL;
  }

  protected final JMenuItem[] getLogFileItem() {
    return logFileItem;
  }

  protected final String[] getLogFileName() {
    return logFileName;
  }
}
