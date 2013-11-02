package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.TaskInterface;
import etomo.comscript.TomodataplotsParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodqtassistProcess;
import etomo.type.AxisID;
import etomo.util.EnvironmentVariable;

/**
 * <p>Description: Handles context menu popup functionality.</p>
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.20  2010/05/28 20:00:12  sueh
 * <p> bug# 1382 Removed calls to a deprecated Java function (File.toURL) in
 * <p> calcImodURL.
 * <p>
 * <p> Revision 3.19  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.18  2009/12/19 01:14:01  sueh
 * <p> bug# 1294 In contextPopup(Component,MouseEvent,String,String,
 * <p> String[],String[],String[],String[],BaseManager,AxisID) added more
 * <p> information to an error message.
 * <p>
 * <p> Revision 3.17  2009/11/04 20:56:07  sueh
 * <p> bug# 1242 Added optional standard menu iterm PEET user guide.
 * <p>
 * <p> Revision 3.16  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
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

public final class ContextPopup {
  public static final String rcsid = "$Id$";

  public static final String TOMO_GUIDE = "tomoguide.html";
  public static final String JOIN_GUIDE = "tomojoin.html";
  public static final String SERIAL_GUIDE = "serialalign.html";

  private final JPopupMenu contextMenu = new JPopupMenu("Help Documents");
  private final JMenuItem serialSectionsGuideItem = new MenuItem(
      "Serial Section Guide ...");
  private final JMenuItem tomoGuideItem = new MenuItem("Tomography Guide ...");
  private final JMenuItem modelGuideItem = new MenuItem("IMOD Users Guide ...");
  private final JMenuItem it3dmodGuide = new MenuItem("3dmod Users Guide ...");
  private final JMenuItem etomoGuideItem = new MenuItem("Etomo Users Guide ...");
  private final JMenuItem joinGuideItem = new MenuItem("Join Users Guide ...");
  private final JMenuItem peetGuideItem = new MenuItem("PEET Users Guide ...");
  private final JMenuItem peetHelpItem = new MenuItem("PEET Help ...");

  private final ActionListener actionListener;
  private final MouseEvent mouseEvent;

  private String[] manPageName = null;
  private String[] logFileName = null;
  private JMenuItem[] manPageItem = null;
  private JMenuItem[] logFileItem = null;
  private String imodURL = null;
  private String anchor = null;
  private JMenuItem[] graphItem = null;
  private List<TaskInterface> graphTask = null;
  private boolean serialSections = false;

  /**
   * Simple context popup constructor.  Only the default menu items are
   * displayed.
   * @param component The component to which the popup is attached.
   * @param mouseEvent The mouse event that opened the menu.
   * @param tomoAnchor The tomography guide HTML anchor for the current popup.
   */
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final BaseManager manager, final AxisID axisID) {

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
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
    addStandardMenuItems(false);
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
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final String guideToAnchor, final String[] manPageLabel,
      final String[] manPage, final BaseManager manager, final AxisID axisID) {
    this.mouseEvent = mouseEvent;
    validate(manPageLabel, manPage);
    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }

        for (int i = 0; i < getManPageItem().length; i++) {
          if (actionEvent.getActionCommand() == getManPageItem()[i].getText()) {
            ImodqtassistProcess.INSTANCE.open(manager, "man/" + getManPageName()[i],
                axisID);
          }
        }

        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager, axisID);

        // Close the menu
        setVisible(false);
      }
    };

    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
    showMenu(component);
  }

  /**
   * Constructor to show the standard items with a anchor into one of the guides.
   * @param component
   * @param mouseEvent
   * @param tomoAnchor
   * @param guideToAnchor
   */
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final String guideToAnchor, final BaseManager manager,
      final AxisID axisID) {

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager, axisID);
        // Close the menu
        setVisible(false);
      }
    };

    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
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
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final String guideToAnchor, final String[] manPageLabel,
      final String[] manPage, final String[] logFileLabel, final String[] logFile,
      final BaseManager manager, final AxisID axisID) {
    this.mouseEvent = mouseEvent;
    validate(manPageLabel, manPage);
    if (logFileLabel.length != logFile.length) {
      StringBuffer message = new StringBuffer();
      message
          .append("log file label and log file arrays must be the same length\nlogFileLabel=\n");
      for (int i = 0; i < logFileLabel.length; i++) {
        message.append(logFileLabel[i] + "\n");
      }
      message.append("logFile=\n");
      for (int i = 0; i < logFile.length; i++) {
        message.append(logFile[i] + "\n");
      }
      throw new IllegalArgumentException(message.toString());
    }

    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(getImodURL()
             * + "man/" + getManPageName()[i]); manpage.setVisible(true); */
            ImodqtassistProcess.INSTANCE.open(manager, "man/" + getManPageName()[i],
                axisID);
          }
        }

        // Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager.getPropertyUserDir()
                + File.separator + getLogFileName()[i]));
          }
        }

        // Search the standard items
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager, axisID);

        // Close the the menu
        setVisible(false);
      }
    };

    addLogFileMenuItems(logFileLabel, logFile);
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
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
   * @param graph information about the graphs
   * @param manager
   * @param axisID
   */
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final String guideToAnchor, final String[] manPageLabel,
      final String[] manPage, final String[] logFileLabel, final String[] logFile,
      final TomodataplotsParam.Task[] graph, final BaseManager manager,
      final AxisID axisID) {
    this.mouseEvent = mouseEvent;
    validate(manPageLabel, manPage);
    if (logFileLabel.length != logFile.length) {
      StringBuffer message = new StringBuffer();
      message
          .append("log file label and log file arrays must be the same length\nlogFileLabel=\n");
      for (int i = 0; i < logFileLabel.length; i++) {
        message.append(logFileLabel[i] + "\n");
      }
      message.append("logFile=\n");
      for (int i = 0; i < logFile.length; i++) {
        message.append(logFile[i] + "\n");
      }
      throw new IllegalArgumentException(message.toString());
    }

    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        setVisible(false);
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(getImodURL()
             * + "man/" + getManPageName()[i]); manpage.setVisible(true); */
            ImodqtassistProcess.INSTANCE.open(manager, "man/" + getManPageName()[i],
                axisID);
            return;
          }
        }

        // Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager.getPropertyUserDir()
                + File.separator + getLogFileName()[i]));
            return;
          }
        }

        JMenuItem[] graphItem = getGraphItem();
        List<TaskInterface> graphTask = getGraphTask();
        if (graphItem != null) {
          for (int i = 0; i < graphItem.length; i++) {
            if (actionEvent.getActionCommand() == graphItem[i].getText()) {
              manager.tomodataplots(graphTask.get(i), axisID, null);
              return;
            }
          }
        }
        // Search the standard items
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager, axisID);
      }
    };

    addLogFileMenuItems(logFileLabel, logFile);
    if (graph != null && graph.length > 0) {
      contextMenu.add(new JPopupMenu.Separator());
      addGraphMenuItems(manager, axisID, graph);
    }
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
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
   * @param graph information about the graphs
   * @param manager
   * @param axisID
   * @param serialSections
   */
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final String guideToAnchor, final String[] manPageLabel,
      final String[] manPage, final String[] logFileLabel, final String[] logFile,
      final TomodataplotsParam.Task[] graph, final BaseManager manager,
      final AxisID axisID, final boolean serialSections) {
    this.mouseEvent = mouseEvent;
    this.serialSections = serialSections;
    validate(manPageLabel, manPage);
    if (logFileLabel.length != logFile.length) {
      StringBuffer message = new StringBuffer();
      message
          .append("log file label and log file arrays must be the same length\nlogFileLabel=\n");
      for (int i = 0; i < logFileLabel.length; i++) {
        message.append(logFileLabel[i] + "\n");
      }
      message.append("logFile=\n");
      for (int i = 0; i < logFile.length; i++) {
        message.append(logFile[i] + "\n");
      }
      throw new IllegalArgumentException(message.toString());
    }

    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        setVisible(false);
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(getImodURL()
             * + "man/" + getManPageName()[i]); manpage.setVisible(true); */
            ImodqtassistProcess.INSTANCE.open(manager, "man/" + getManPageName()[i],
                axisID);
            return;
          }
        }

        // Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager.getPropertyUserDir()
                + File.separator + getLogFileName()[i]));
            return;
          }
        }

        JMenuItem[] graphItem = getGraphItem();
        List<TaskInterface> graphTask = getGraphTask();
        if (graphItem != null) {
          for (int i = 0; i < graphItem.length; i++) {
            if (actionEvent.getActionCommand() == graphItem[i].getText()) {
              manager.tomodataplots(graphTask.get(i), axisID, null);
              return;
            }
          }
        }
        // Search the standard items
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager, axisID);
      }
    };

    addLogFileMenuItems(logFileLabel, logFile);
    if (graph != null && graph.length > 0) {
      contextMenu.add(new JPopupMenu.Separator());
      addGraphMenuItems(manager, axisID, graph);
    }
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
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
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final String guideToAnchor, final String[] manPageLabel,
      final String[] manPage, final String[] logFileLabel, final String[] logFile,
      final BaseManager manager, final AxisID axisID, final String subdirName) {
    this.mouseEvent = mouseEvent;
    validate(manPageLabel, manPage);
    if (logFileLabel.length != logFile.length) {
      String message = "log file label and log file arrays must be the same length";
      throw new IllegalArgumentException(message);
    }

    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        String guideLocation = guideToAnchor;
        String anchor = getAnchor();
        if (anchor != null && !anchor.equals("")) {
          guideLocation += "#" + anchor;
        }
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(getImodURL()
             * + "man/" + getManPageName()[i]); manpage.setVisible(true); */
            ImodqtassistProcess.INSTANCE.open(manager, "man/" + getManPageName()[i],
                axisID);
          }
        }

        // Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        for (int i = 0; i < logFileItem.length; i++) {
          if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
            TextPageWindow logFileWindow = new TextPageWindow();
            logFileWindow.setVisible(logFileWindow.setFile(manager.getPropertyUserDir()
                + (subdirName != null ? File.separator + subdirName : "")
                + File.separator + getLogFileName()[i]));
          }
        }

        // Search the standard items
        globalItemAction(actionEvent, guideLocation, guideToAnchor, manager, axisID);

        // Close the the menu
        setVisible(false);
      }
    };

    addLogFileMenuItems(logFileLabel, logFile);
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
    showMenu(component);
  }

  /**
  * Shows a man page list and log file items in addition
  * to the standard menu items (including the peet guide if required).
  * @param component The component to which the popup is attached.
  * @param mouseEvent The mouse event that opened the menu.
  * @param tomoAnchor The guide HTML anchor for the current popup.
  * @param guideToAnchor The guide containing the HTML anchor.
  * @param manPageLabel The string array of man page labels for the menu.
  * @param manPage The name of the HTML man pages.
  * @param manager
  */
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String[] manPageLabel, final String[] manPage, final boolean addPeetGuide,
      final BaseManager manager, final AxisID axisID) {
    this.mouseEvent = mouseEvent;
    validate(manPageLabel, manPage);
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        String anchor = getAnchor();
        JMenuItem[] manPageItem = getManPageItem();
        for (int i = 0; i < manPageItem.length; i++) {
          if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
            /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(getImodURL()
             * + "man/" + getManPageName()[i]); manpage.setVisible(true); */
            ImodqtassistProcess.INSTANCE.open(manager, "man/" + getManPageName()[i],
                axisID);
          }
        }

        // Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        if (logFileItem != null) {
          for (int i = 0; i < logFileItem.length; i++) {
            if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
              TextPageWindow logFileWindow = new TextPageWindow();
              logFileWindow.setVisible(logFileWindow.setFile(manager.getPropertyUserDir()
                  + File.separator + getLogFileName()[i]));
            }
          }
        }

        // Search the standard items
        globalItemAction(actionEvent, null, null, manager, axisID);

        // Close the the menu
        setVisible(false);
      }
    };

    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(addPeetGuide);
    showMenu(component);
  }

  /**
   * Shows a man page list and log file items in addition
   * to the standard menu items.
   * @param component
   * @param mouseEvent
   * @param manager
   * @param axisID
   */
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final BaseManager manager, final AxisID axisID, final boolean serialSections) {
    this.serialSections = serialSections;
    this.mouseEvent = mouseEvent;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(final ActionEvent actionEvent) {
        String anchor = getAnchor();
        JMenuItem[] manPageItem = getManPageItem();
        if (manPageItem != null) {
          for (int i = 0; i < manPageItem.length; i++) {
            if (actionEvent.getActionCommand() == manPageItem[i].getText()) {
              ImodqtassistProcess.INSTANCE.open(manager, "man/" + getManPageName()[i],
                  axisID);
            }
          }
        }

        // Search the logfile items
        JMenuItem[] logFileItem = getLogFileItem();
        if (logFileItem != null) {
          for (int i = 0; i < logFileItem.length; i++) {
            if (actionEvent.getActionCommand() == logFileItem[i].getText()) {
              TextPageWindow logFileWindow = new TextPageWindow();
              logFileWindow.setVisible(logFileWindow.setFile(manager.getPropertyUserDir()
                  + File.separator + getLogFileName()[i]));
            }
          }
        }

        // Search the standard items
        globalItemAction(actionEvent, null, null, manager, axisID);

        // Close the the menu
        setVisible(false);
      }
    };

    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
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
  public ContextPopup(final Component component, final MouseEvent mouseEvent,
      final String tomoAnchor, final String[] manPageLabel, final String[] manPage,
      final String[] logWindowLabel, final Vector logFileLabel, final Vector logFile,
      final TomodataplotsParam.Task[] graph, final ApplicationManager applicationManager,
      final String updateLogCommandName, final AxisID axisID) {
    this.mouseEvent = mouseEvent;
    validate(manPageLabel, manPage);
    if (logFileLabel.size() != logFile.size()) {
      String message = "log file label and log file vectors must be the same length";
      throw new IllegalArgumentException(message);
    }

    anchor = tomoAnchor;
    calcImodURL();

    // Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {

      public void actionPerformed(final ActionEvent actionEvent) {
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

        // Search the logfile items
        for (int i = 0; i < getLogFileItem().length; i++) {
          if (actionEvent.getActionCommand() == getLogFileItem()[i].getText()) {
            if (actionEvent.getActionCommand().startsWith(updateLogCommandName)) {
              applicationManager.updateLog(updateLogCommandName, axisID);
            }
            // Create full path to the appropriate log file items
            String[] logFileList = (String[]) logFile.get(i);
            String[] logFileFullPath = new String[logFileList.length];
            String path = applicationManager.getPropertyUserDir() + File.separator;
            for (int j = 0; j < logFileList.length; j++) {
              logFileFullPath[j] = path + logFileList[j];
            }
            TabbedTextWindow logFileWindow = new TabbedTextWindow(logWindowLabel[i],
                axisID);
            try {
              if (logFileWindow.openFiles(applicationManager, logFileFullPath,
                  (String[]) logFileLabel.get(i), axisID)) {
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
              UIHarness.INSTANCE.openMessageDialog(applicationManager,
                  "WARNING:  Ran out of memory.  Will not display log file."
                      + "\nPlease close open windows or exit Etomo.", "Out of Memory");
              throw e;
            }
          }
        }

        JMenuItem[] graphItem = getGraphItem();
        List<TaskInterface> graphTask = getGraphTask();
        if (graphItem != null) {
          for (int i = 0; i < graphItem.length; i++) {
            if (actionEvent.getActionCommand() == graphItem[i].getText()) {
              applicationManager.tomodataplots(graphTask.get(i), axisID, null);
              return;
            }
          }
        }
        // Search the standard items
        globalItemAction(actionEvent, tomoGuideLocation, applicationManager, axisID);

        // Close the the menu
        setVisible(false);
      }
    };

    addTabbedLogFileMenuItems(logWindowLabel);
    if (graph != null && graph.length > 0) {
      contextMenu.add(new JPopupMenu.Separator());
      addGraphMenuItems(applicationManager, axisID, graph);
    }
    contextMenu.add(new JPopupMenu.Separator());
    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems(false);
    showMenu(component);
  }

  /**
   *
   */
  private void addStandardMenuItems(final boolean addPeetGuide) {
    // Construct the context menu
    if (addPeetGuide) {
      contextMenu.add(peetGuideItem);
      contextMenu.add(peetHelpItem);
      peetGuideItem.addActionListener(actionListener);
      peetHelpItem.addActionListener(actionListener);
      if (!EnvironmentVariable.INSTANCE.exists(null, null,
          EnvironmentVariable.PARTICLE_DIR, null)) {
        peetHelpItem.setEnabled(false);
      }
    }
    if (serialSections) {
      contextMenu.add(serialSectionsGuideItem);
      serialSectionsGuideItem.addActionListener(actionListener);
    }
    else {
      contextMenu.add(tomoGuideItem);
      tomoGuideItem.addActionListener(actionListener);
    }
    contextMenu.add(modelGuideItem);
    modelGuideItem.addActionListener(actionListener);
    contextMenu.add(it3dmodGuide);
    it3dmodGuide.addActionListener(actionListener);
    contextMenu.add(etomoGuideItem);
    etomoGuideItem.addActionListener(actionListener);
    if (!serialSections) {
      contextMenu.add(joinGuideItem);
      joinGuideItem.addActionListener(actionListener);
    }
  }

  /**
   * 
   * @param manPageLabel
   * @param manPage
   */
  private void addManPageMenuItems(final String[] manPageLabel, final String manPage[]) {
    manPageItem = new MenuItem[manPageLabel.length];
    manPageName = new String[manPage.length];
    for (int i = 0; i < manPageItem.length; i++) {
      manPageItem[i] = new MenuItem();
      manPageItem[i].setText(manPageLabel[i] + " man page ...");
      manPageItem[i].addActionListener(actionListener);
      contextMenu.add(manPageItem[i]);
      manPageName[i] = manPage[i] + Constants.TOP_ANCHOR;
    }
  }

  private void globalItemAction(final ActionEvent actionEvent,
      final String tomoGuideLocation, final BaseManager manager, final AxisID axisID) {
    globalItemAction(actionEvent, tomoGuideLocation, TOMO_GUIDE, manager, axisID);
  }

  /**
   * Open the appropriate file if the event is one of the global menu items 
   * @param actionEvent
   * @param tomoGuideLocation
   */
  private void globalItemAction(final ActionEvent actionEvent, String guideLocation,
      final String guide, final BaseManager manager, final AxisID axisID) {
    // Add TOP anchor when no anchor has been set.
    if (guideLocation != null && !guideLocation.matches("\\s*")
        && guideLocation.indexOf("#") == -1) {
      guideLocation += Constants.TOP_ANCHOR;
    }
    if (actionEvent.getActionCommand() == tomoGuideItem.getText()) {
      /* HTMLPageWindow manpage = new HTMLPageWindow(); if (guide.equals(TOMO_GUIDE)) {
       * manpage.openURL(imodURL + guideLocation); } else { manpage.openURL(imodURL +
       * TOMO_GUIDE); } manpage.setVisible(true); */
      if (guide != null && guide.equals(TOMO_GUIDE)) {
        ImodqtassistProcess.INSTANCE.open(manager, guideLocation, axisID);
      }
      else {
        ImodqtassistProcess.INSTANCE.open(manager, TOMO_GUIDE + Constants.TOP_ANCHOR,
            axisID);
      }
    }

    if (actionEvent.getActionCommand() == modelGuideItem.getText()) {
      /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(imodURL +
       * "guide.html"); manpage.setVisible(true); */
      ImodqtassistProcess.INSTANCE.open(manager, "guide.html" + Constants.TOP_ANCHOR,
          axisID);
    }

    if (actionEvent.getActionCommand() == it3dmodGuide.getText()) {
      /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(imodURL +
       * "3dmodguide.html"); manpage.setVisible(true); */
      ImodqtassistProcess.INSTANCE.open(manager,
          "3dmodguide.html" + Constants.TOP_ANCHOR, axisID);
    }

    if (actionEvent.getActionCommand() == etomoGuideItem.getText()) {
      /* HTMLPageWindow manpage = new HTMLPageWindow(); manpage.openURL(imodURL +
       * "UsingEtomo.html"); manpage.setVisible(true); */
      ImodqtassistProcess.INSTANCE.open(manager,
          "UsingEtomo.html" + Constants.TOP_ANCHOR, axisID);
    }

    if (actionEvent.getActionCommand() == joinGuideItem.getText()) {
      /* HTMLPageWindow manpage = new HTMLPageWindow(); if (guide.equals(JOIN_GUIDE)) {
       * manpage.openURL(imodURL + guideLocation); } else { manpage.openURL(imodURL +
       * JOIN_GUIDE); } manpage.setVisible(true); */
      if (guide != null && guide.equals(JOIN_GUIDE)) {
        ImodqtassistProcess.INSTANCE.open(manager, guideLocation, axisID);
      }
      else {
        ImodqtassistProcess.INSTANCE.open(manager, JOIN_GUIDE + Constants.TOP_ANCHOR,
            axisID);
      }
    }

    if (actionEvent.getActionCommand() == peetGuideItem.getText()) {
      ImodqtassistProcess.INSTANCE.open(manager,
          "PEETmanual.html" + Constants.TOP_ANCHOR, axisID);
    }
    if (actionEvent.getActionCommand() == peetHelpItem.getText()) {
      BaseProcessManager.startSystemProgramThread(
          new String[] { new File(new File(new File(EnvironmentVariable.INSTANCE
              .getValue(null, null, EnvironmentVariable.PARTICLE_DIR, null)), "bin"),
              "PEETHelp").getAbsolutePath() }, axisID, manager);
    }
    if (actionEvent.getActionCommand() == serialSectionsGuideItem.getText()) {
      if (guide != null && guide.equals(SERIAL_GUIDE)) {
        ImodqtassistProcess.INSTANCE.open(manager, guideLocation, axisID);
      }
      else {
        ImodqtassistProcess.INSTANCE.open(manager, SERIAL_GUIDE + Constants.TOP_ANCHOR,
            axisID);
      }
    }
  }

  /**
   * 
   * @param logFileLabel
   * @param logFile
   */
  private void addLogFileMenuItems(final String[] logFileLabel, final String logFile[]) {
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

  private void addGraphMenuItems(final BaseManager manager, final AxisID axisID,
      final TomodataplotsParam.Task[] graph) {
    graphTask = new ArrayList<TaskInterface>();
    for (int i = 0; i < graph.length; i++) {
      if (graph[i].isAvailable(manager, axisID)) {
        graphTask.add(graph[i]);
      }
    }
    graphItem = new MenuItem[graphTask.size()];
    for (int i = 0; i < graphItem.length; i++) {
      graphItem[i] = new MenuItem();
      graphItem[i].setText(graphTask.get(i).toString());
      graphItem[i].addActionListener(actionListener);
      contextMenu.add(graphItem[i]);
    }
  }

  private void addTabbedLogFileMenuItems(final String[] logWindowLabel) {
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
  private void showMenu(final Component component) {
    contextMenu.show(component, mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }

  /**
   * Calculate the IMOD URL
   */
  private void calcImodURL() {
    try {
      imodURL = EtomoDirector.INSTANCE.getIMODDirectory().toURI().toURL().toString()
          + "/html/";
    }
    catch (MalformedURLException except) {
      except.printStackTrace();
      System.err.println("Malformed URL:");
      System.err.println(EtomoDirector.INSTANCE.getIMODDirectory().toString());
    }
  }

  private void validate(final String[] manPageLabel, final String[] manPage)
      throws IllegalArgumentException {
    // Check to make sure that the menu label and man page arrays are the same
    // length
    if (manPageLabel.length != manPage.length) {
      String message = "menu label and man page arrays must be the same length";
      throw new IllegalArgumentException(message);
    }
  }

  private String getAnchor() {
    return anchor;
  }

  private void setVisible(final boolean visible) {
    contextMenu.setVisible(visible);
  }

  private JMenuItem[] getManPageItem() {
    return manPageItem;
  }

  private String[] getManPageName() {
    return manPageName;
  }

  private JMenuItem[] getLogFileItem() {
    return logFileItem;
  }

  private JMenuItem[] getGraphItem() {
    return graphItem;
  }

  private List<TaskInterface> getGraphTask() {
    return graphTask;
  }

  private String[] getLogFileName() {
    return logFileName;
  }
}
