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
  public static final String rcsid =
    "$Id$";
  
  public static final String TOMO_GUIDE = "tomoguide.html";
  public static final String JOIN_GUIDE = "tomojoin.html";

  private JPopupMenu contextMenu = new JPopupMenu("Help Documents");
  private JMenuItem[] manPageItem;
  private JMenuItem[] logFileItem;
  private JMenuItem tomoGuideItem = new JMenuItem("Tomography Guide ...");
  private JMenuItem modelGuideItem = new JMenuItem("IMOD Users Guide ...");
  private JMenuItem it3dmodGuide = new JMenuItem("3dmod Users Guide ...");
  private JMenuItem etomoGuideItem = new JMenuItem("Etomo Users Guide ...");
  private JMenuItem joinGuideItem = new JMenuItem("Join Users Guide ...");
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
  public ContextPopup(
    Component component,
    MouseEvent mouseEvent,
    String tomoAnchor) {

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String tomoGuideLocation = "tomoguide.html";

        if (anchor != null && !anchor.equals("")) {
          tomoGuideLocation += "#" + anchor;
        }
        globalItemAction(actionEvent, tomoGuideLocation);
        contextMenu.setVisible(false);
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
  public ContextPopup(
    Component component,
    MouseEvent mouseEvent,
    String tomoAnchor,
    final String guideToAnchor,
    String[] manPageLabel,
    String[] manPage) {

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
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }

        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            HTMLPageWindow manpage = new HTMLPageWindow();
            manpage.openURL(imodURL + "man/" + manPageName[i]);
            manpage.setVisible(true);
          }
        }

        globalItemAction(actionEvent, guideLocation, guideToAnchor);

        //  Close the menu
        contextMenu.setVisible(false);
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
  public ContextPopup(
      Component component,
      MouseEvent mouseEvent,
      String tomoAnchor,
      final String guideToAnchor) {

      this.mouseEvent = mouseEvent;
      anchor = tomoAnchor;
      calcImodURL();

      //  Instantiate a new ActionListener to handle the menu selection
      actionListener = new ActionListener() {
        public void actionPerformed(ActionEvent actionEvent) {
          String guideLocation = guideToAnchor;
          if (anchor != null && !anchor.equals("")) {
            guideLocation += "#" + anchor;
          }
          globalItemAction(actionEvent, guideLocation, guideToAnchor);
          //  Close the menu
          contextMenu.setVisible(false);
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
  public ContextPopup(
      Component component,
      MouseEvent mouseEvent,
      String tomoAnchor,
      final String guideToAnchor,
      String[] manPageLabel,
      String[] manPage,
      String[] logFileLabel,
      String[] logFile,
      final BaseManager manager) {

      // Check to make sure that the menu label and man page arrays are the same
      // length
      if (manPageLabel.length != manPage.length) {
        String message = "menu label and man page arrays must be the same length";
        throw new IllegalArgumentException(message);
      }
      if (logFileLabel.length != logFile.length) {
        String message =
          "log file label and log file arrays must be the same length";
        throw new IllegalArgumentException(message);
      }

      this.mouseEvent = mouseEvent;
      anchor = tomoAnchor;
      calcImodURL();

      //  Instantiate a new ActionListener to handle the menu selection
      actionListener = new ActionListener() {
        public void actionPerformed(ActionEvent actionEvent) {
          String guideLocation = guideToAnchor;
          if (anchor != null && !anchor.equals("")) {
            guideLocation += "#" + anchor;
          }

          for (int i = 0; i < manPageItem.length; i++) {
            if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
              HTMLPageWindow manpage = new HTMLPageWindow();
              manpage.openURL(imodURL + "man/" + manPageName[i]);
              manpage.setVisible(true);
            }
          }

          //  Search the logfile items
          for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager
                .getPropertyUserDir()
                + File.separator + logFileName[i]));
          }
        }

          //  Search the standard items
          globalItemAction(actionEvent, guideLocation, guideToAnchor);

          //  Close the  the menu
          contextMenu.setVisible(false);
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
  public ContextPopup(
    Component component,
    MouseEvent mouseEvent,
    String tomoAnchor,
    String[] manPageLabel,
    String[] manPage,
    final String[] logWindowLabel,
    final Vector logFileLabel,
    final Vector logFile,
    final ApplicationManager applicationManager,
    final String updateLogCommandName,
    final AxisID axisID) {

    // Check to make sure that the menu label and man page arrays are the same
    // length
    if (manPageLabel.length != manPage.length) {
      String message = "menu label and man page arrays must be the same length";
      throw new IllegalArgumentException(message);
    }
    if (logFileLabel.size() != logFile.size()) {
      String message =
        "log file label and log file vectors must be the same length";
      throw new IllegalArgumentException(message);
    }

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {

      public void actionPerformed(ActionEvent actionEvent) {
        boolean succeeded = true;
        String tomoGuideLocation = "tomoguide.html";
        if (anchor != null && !anchor.equals("")) {
          tomoGuideLocation += "#" + anchor;
        }

        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            HTMLPageWindow manpage = new HTMLPageWindow();
            manpage.openURL(imodURL + "man/" + manPageName[i]);
            manpage.setVisible(true);
          }
        }

        //  Search the logfile items
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            if (actionEvent
              .getActionCommand()
              .startsWith(updateLogCommandName)) {
              applicationManager.updateLog(updateLogCommandName, axisID);
            }
            //  Create full path to the appropriate log file items
            String[] logFileList = (String[]) logFile.get(i);
            String[] logFileFullPath = new String[logFileList.length];
            String path = applicationManager.getPropertyUserDir() + File.separator;
            for (int j = 0; j < logFileList.length; j++) {
              logFileFullPath[j] = path + logFileList[j];
            }
            TabbedTextWindow logFileWindow =
              new TabbedTextWindow(logWindowLabel[i]);

            try {
              logFileWindow.openFiles(
                logFileFullPath,
                (String[]) logFileLabel.get(i), axisID);
            }
            catch (FileNotFoundException e) {
              e.printStackTrace();
              System.err.println("File not file exception: " + logFileFullPath);
            }
            catch (IOException e) {
              e.printStackTrace();
              System.err.println("IO exception: " + logFileFullPath);
            }
            try {
              logFileWindow.setVisible(true);
            }
            catch (OutOfMemoryError e) {
              e.printStackTrace();
              logFileWindow = null;
              UIHarness.INSTANCE.openMessageDialog(
                  "Ran out of memory.  Will not display log file"
                      + ".  To avoid running out of memory, edit the etomo script"
                      + " and set javaMemLim to a larger number of megabytes.",
                  "Out of Memory", axisID);
              succeeded = false;
              break;
            }
          }
        }

        if (succeeded) {
        //  Search the standard items
        globalItemAction(actionEvent, tomoGuideLocation);

        //  Close the  the menu
        contextMenu.setVisible(false);
        }
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
    manPageItem = new JMenuItem[manPageLabel.length];
    manPageName = new String[manPage.length];
    for (int i = 0; i < manPageItem.length; i++) {
      manPageItem[i] = new JMenuItem();
      manPageItem[i].setText(manPageLabel[i] + " man page ...");
      manPageItem[i].addActionListener(actionListener);
      contextMenu.add(manPageItem[i]);
      manPageName[i] = manPage[i];
    }
  }

  private void globalItemAction(
      ActionEvent actionEvent,
      String tomoGuideLocation) {
    globalItemAction(actionEvent, tomoGuideLocation, TOMO_GUIDE);
  }
  
  /**
   * Open the appropriate file if the event is one of the global menu items 
   * @param actionEvent
   * @param tomoGuideLocation
   */
  private void globalItemAction(
    ActionEvent actionEvent,
    String guideLocation, String guide) {
    if (actionEvent.getActionCommand() == tomoGuideItem.getText()) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      if (guide.equals(TOMO_GUIDE)) {
        manpage.openURL(imodURL + guideLocation);
      }
      else {
        manpage.openURL(imodURL + TOMO_GUIDE);
      }
      manpage.setVisible(true);
    }

    if (actionEvent.getActionCommand() == modelGuideItem.getText()) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "guide.html");
      manpage.setVisible(true);
    }

    if (actionEvent.getActionCommand() == it3dmodGuide.getText()) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "3dmodguide.html");
      manpage.setVisible(true);
    }

    if (actionEvent.getActionCommand() == etomoGuideItem.getText()) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      manpage.openURL(imodURL + "UsingEtomo.html");
      manpage.setVisible(true);
    }
    
    if (actionEvent.getActionCommand() == joinGuideItem.getText()) {
      HTMLPageWindow manpage = new HTMLPageWindow();
      if (guide.equals(JOIN_GUIDE)) {
        manpage.openURL(imodURL + guideLocation);
      }
      else {
        manpage.openURL(imodURL + JOIN_GUIDE);
      }
      manpage.setVisible(true);
    }
  }

  /**
   * 
   * @param logFileLabel
   * @param logFile
   */
  private void addLogFileMenuItems(String[] logFileLabel, String logFile[]) {
    logFileItem = new JMenuItem[logFileLabel.length];
    logFileName = new String[logFile.length];
    for (int i = 0; i < logFileItem.length; i++) {
      logFileItem[i] = new JMenuItem();
      logFileItem[i].setText(logFileLabel[i] + " log file ...");
      logFileItem[i].addActionListener(actionListener);
      contextMenu.add(logFileItem[i]);
      logFileName[i] = logFile[i];
    }
  }

  private void addTabbedLogFileMenuItems(String[] logWindowLabel) {
    logFileItem = new JMenuItem[logWindowLabel.length];
    for (int i = 0; i < logFileItem.length; i++) {
      logFileItem[i] = new JMenuItem();
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
      imodURL =
        EtomoDirector.getInstance().getIMODDirectory().toURL().toString() + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.getInstance().getIMODDirectory().toString());
    }
  }
}
