package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import etomo.ApplicationManager;

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

  private JPopupMenu contextMenu = new JPopupMenu("Help Documents");
  private JMenuItem[] manPageItem;
  private JMenuItem[] logFileItem;
  private JMenuItem tomoGuideItem = new JMenuItem("Tomography Guide ...");
  private JMenuItem modelGuideItem = new JMenuItem("Imod Users Guide ...");
  private ActionListener actionListener;
  private MouseEvent mouseEvent;

  private static ApplicationManager appManager;
  private String[] manPageName;
  private String[] logFileName;

  private String anchor;

  /**
   * This constructor is used to set the static ApplicationMananger reference,
   * this must be called before any other instances are constructred or a null
   * reference exception will be thrown.
   */
  public ContextPopup(ApplicationManager appMgr) {
    appManager = appMgr;
  }

  /**
   * Simple context popup constructor.  Only the default menu items are
   * displayed.
   */
  public ContextPopup(
    Component component,
    MouseEvent mouseEvent,
    String tomoAnchor) {

    this.mouseEvent = mouseEvent;
    anchor = tomoAnchor;

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {

      public void actionPerformed(ActionEvent actionEvent) {
        String imodURL = "file://" + appManager.getIMODDirectory() + "/html/";
        String tomoGuideLocation = "tomoguide.html";

        if (anchor != null && !anchor.equals("")) {
          tomoGuideLocation += "#" + anchor;
        }

        if (actionEvent.getActionCommand() == tomoGuideItem.getText()) {
          HTMLPageWindow manpage = new HTMLPageWindow();
          manpage.openURL(imodURL + tomoGuideLocation);
          manpage.setVisible(true);
        }

        if (actionEvent.getActionCommand() == modelGuideItem.getText()) {
          HTMLPageWindow manpage = new HTMLPageWindow();
          manpage.openURL(imodURL + "guide.html");
          manpage.setVisible(true);
        }

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
   */
  public ContextPopup(
    Component component,
    MouseEvent mouseEvent,
    String tomoAnchor,
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

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String imodURL = "file://" + appManager.getIMODDirectory() + "/html/";

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

        //  Search the standard items
        if (actionEvent.getActionCommand() == tomoGuideItem.getText()) {
          HTMLPageWindow manpage = new HTMLPageWindow();
          manpage.openURL(imodURL + tomoGuideLocation);
          manpage.setVisible(true);
        }

        if (actionEvent.getActionCommand() == modelGuideItem.getText()) {
          HTMLPageWindow manpage = new HTMLPageWindow();
          manpage.openURL(imodURL + "guide.html");
          manpage.setVisible(true);
        }

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
   * Constructor to show a man page list and log file items in addition to the
   * the standard menu items.
   */
  public ContextPopup(
    Component component,
    MouseEvent mouseEvent,
    String tomoAnchor,
    String[] manPageLabel,
    String[] manPage,
    String[] logFileLabel,
    String[] logFile) {

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

    //  Instantiate a new ActionListener to handle the menu selection
    actionListener = new ActionListener() {
      public void actionPerformed(ActionEvent actionEvent) {
        String imodURL = "file://" + appManager.getIMODDirectory() + "/html/";

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
            TextPageWindow logFile = new TextPageWindow();
            logFile.setVisible(
              logFile.setFile(
                appManager.getWorkingDirectory() + "/" + logFileName[i]));
          }
        }

        //  Search the standard items
        if (actionEvent.getActionCommand() == tomoGuideItem.getText()) {
          HTMLPageWindow manpage = new HTMLPageWindow();
          manpage.openURL(imodURL + tomoGuideLocation);
          manpage.setVisible(true);
        }

        if (actionEvent.getActionCommand() == modelGuideItem.getText()) {
          HTMLPageWindow manpage = new HTMLPageWindow();
          manpage.openURL(imodURL + "guide.html");
          manpage.setVisible(true);
        }

        //  Close the  the menu
        contextMenu.setVisible(false);
      }
    };

    addManPageMenuItems(manPageLabel, manPage);
    contextMenu.add(new JPopupMenu.Separator());
    addLogFileMenuItems(logFileLabel, logFile);
    contextMenu.add(new JPopupMenu.Separator());
    addStandardMenuItems();
    showMenu(component);
  }

  private void addStandardMenuItems() {
    //  Construct the context menu
    contextMenu.add(tomoGuideItem);
    tomoGuideItem.addActionListener(actionListener);
    contextMenu.add(modelGuideItem);
    modelGuideItem.addActionListener(actionListener);
  }

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

  private void showMenu(Component component) {
    contextMenu.show(component, mouseEvent.getX(), mouseEvent.getY());
    contextMenu.setVisible(true);
  }
}
