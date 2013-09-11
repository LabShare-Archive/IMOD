package etomo.ui.swing;

import java.awt.AWTEvent;
import java.awt.BorderLayout;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;

import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.FrameType;
import etomo.type.UITestFieldType;
import etomo.util.UniqueKey;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2005</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 18:14:39  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.62  2010/06/04 20:02:33  sueh
 * <p> bug# 1380 In saveLocation checking the --ignoreloc argument before
 * <p> using saving the location.
 * <p>
 * <p> Revision 3.61  2010/06/02 21:46:24  sueh
 * <p> bug# 1380 Added saveLocation.
 * <p>
 * <p> Revision 3.60  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.59  2009/11/20 17:27:52  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 3.58  2009/10/27 20:42:53  sueh
 * <p> bug# 1275 Moved FrontPageDialog to FrontPageManager.
 * <p>
 * <p> Revision 3.57  2009/10/23 19:47:26  sueh
 * <p> bug# 1275 Make separate menu items for generic parallel process and
 * <p> NAD.  Added default display FrontPageDialog.
 * <p>
 * <p> Revision 3.56  2009/09/29 00:23:00  sueh
 * <p> bug# 1228 Move MainFrame to the front after a call to LogFrame.refresh().
 * <p>
 * <p> Revision 3.55  2009/04/02 19:19:14  sueh
 * <p> bug# 1206 Only need one call to setEnabled in
 * <p> setCurrentManager(BaseManager,ManagerKey).
 * <p>
 * <p> Revision 3.54  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.53  2009/03/05 23:31:05  sueh
 * <p> bug# 1194 Added msgUpdateLogProperties.
 * <p>
 * <p> Revision 3.52  2009/03/02 19:00:02  sueh
 * <p> bug# 1193 Removed reconnect calls from showAxisB and
 * <p> showBothAxis.  Reconnects for all axes must always be called from
 * <p> openProcessingPanel.
 * <p>
 * <p> Revision 3.51  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 3.50  2009/01/20 20:13:24  sueh
 * <p> bug# 1102 Printing out the name of the main frame root panel.
 * <p>
 * <p> Revision 3.49  2008/12/10 18:34:39  sueh
 * <p> bug# 1162 Added a manager stamp to setCurrentManager.
 * <p>
 * <p> Revision 3.48  2008/09/30 21:57:47  sueh
 * <p> bug# 1113 Reformatted
 * <p>
 * <p> Revision 3.47  2008/05/30 21:32:15  sueh
 * <p> bug# 1102 Made MainFrame public for ui test.
 * <p>
 * <p> Revision 3.46  2008/01/14 22:06:15  sueh
 * <p> bug# 1050 Moved axis B reconnect from ApplicationManager to showAxisB and
 * <p> showBothAxis because reconnect to processchunks cannot be done unless the
 * <p> axis is visible.
 * <p>
 * <p> Revision 3.45  2007/12/26 22:24:56  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 3.44  2007/09/07 00:27:30  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.43  2006/06/21 15:54:04  sueh
 * <p> bug# 581 Passing manager and axis to ContextPopup, so that imodqtassist can
 * <p> be run.
 * <p>
 * <p> Revision 3.42  2006/04/25 19:16:14  sueh
 * <p> bug# 787 Named the main frame so it can be found by a JfcUnit finder.
 * <p>
 * <p> Revision 3.41  2006/03/20 18:03:09  sueh
 * <p> bug# 835 Added getName (a convenience function) to the managers.
 * <p>
 * <p> Revision 3.40  2006/01/11 22:14:53  sueh
 * <p> bug# 675 During JFCUnitTests prevent the main Etomo window from
 * <p> closing before the tearDown function is called by preventing
 * <p> processWindowEvent() from calling menu.doClickFileExit() when
 * <p> EtomoDirector.test is true.
 * <p>
 * <p> Revision 3.39  2006/01/04 20:26:13  sueh
 * <p> bug# 675 Removed kill Etomo functionality.  Running each test separately
 * <p> from a script so that each instance of Etomo will have a separate virtual
 * <p> machine.
 * <p>
 * <p> Revision 3.38  2006/01/03 23:41:37  sueh
 * <p> bug# 675 Created a way to kill the current Etomo.
 * <p>
 * <p> Revision 3.37  2005/12/09 20:34:50  sueh
 * <p> bug# 776 In EtomoMenu removed the getActionCommand... functions and
 * <p> replaced them with equals().
 * <p>
 * <p> Revision 3.36  2005/09/22 21:21:02  sueh
 * <p> bug# 532 In showAxisA, B, and Both:  using UIHarness.pack() to get a
 * <p> complete pack.
 * <p>
 * <p> Revision 3.35  2005/08/22 22:08:41  sueh
 * <p> bug# 714 Adding call to makeCurrent() when setCurrentManager is called.
 * <p>
 * <p> Revision 3.34  2005/07/11 23:02:24  sueh
 * <p> bug# 619 Moved code to get the screen size to UIUtilities so it can be
 * <p> used in ProcessorTable.
 * <p>
 * <p> Revision 3.33  2005/06/01 21:27:21  sueh
 * <p> bug# 667 Removing the Controller classes.  Trying make meta data and
 * <p> app manager equals didn't work very well.  Meta data is created by and
 * <p> managed by app mgr and the class structure should reflect that.
 * <p>
 * <p> Revision 3.32  2005/05/12 22:14:55  sueh
 * <p> bug# 615 Change setCurrentManager() to handle a null currentManager.
 * <p> Hide the second window.  Remove the axis and the dataset from the title.
 * <p> Disable menu items with work with an open dataset.
 * <p>
 * <p> Revision 3.31  2005/05/10 19:44:54  sueh
 * <p> bug# 615 Added setTitle(AxisID).  Removing A Axis portion of the title for
 * <p> Setup, Join, and single axis.
 * <p>
 * <p> Revision 3.30  2005/04/27 02:16:40  sueh
 * <p> bug# 615 Calling setVisible(true) instead of updateAxis().  SubFrame
 * <p> had overridden setVisible(boolean).
 * <p>
 * <p> Revision 3.29  2005/04/26 17:40:09  sueh
 * <p> bug# 615 Made MainFrame, SubFrame, and EtomoFrame package-level
 * <p> classes.  All MainFrame functionality is handled through UIHarness to
 * <p> make Etomo more compatible with JUnit.  Fixed function access levels.
 * <p>
 * <p> Revision 3.28  2005/04/25 21:08:29  sueh
 * <p> bug# 615 Moving message dialog functions, menu appearance functions,
 * <p> and fitting and repainting functions from MainFrame to EtomoFrame.
 * <p> Added EtomoFrame static instance variables to EtomoFrame
 * <p> for an instance of the MainFrame and an instance of the SubFrame.
 * <p> Added an abstract register() function to initialize the static instance
 * <p> variables.
 * <p>
 * <p> Revision 3.27  2005/04/21 20:37:48  sueh
 * <p> bug# 615 Make EtomoFrame is a class.  It now handles the menu action
 * <p> functions common to MainFrame and SubFrame.  Added
 * <p> fitWindow(AxisID) and repaint(AxisID) to handle single frame autofits.
 * <p>
 * <p> Revision 3.26  2005/04/20 01:47:37  sueh
 * <p> bug# 615 Moved menu functionality to EtomoMenu.  When setting the
 * <p> current manager, change the SubFrame to reflect the situation of the
 * <p> newly displayed manager.  Take into account whether the manager has
 * <p> two axis and whether they are both being displayed.
 * <p>
 * <p> Revision 3.25  2005/04/16 01:57:54  sueh
 * <p> bug# 615 Added subFrame.  Changed show functions to display axis B
 * <p> in subFrame.  Changed Axis A command and Axis B command to hide
 * <p> subFrame.  Override pack() to also pack subFrame.
 * <p>
 * <p> Revision 3.24  2005/04/12 19:38:39  sueh
 * <p> bug# 615 Do not disable fit window menu option.
 * <p>
 * <p> Revision 3.23  2005/04/01 02:53:07  sueh
 * <p> bug# 622 newstuff: turning off fit after fit is excuted.  Added setEnabledFit().
 * <p>
 * <p> Revision 3.22  2005/04/01 00:11:45  sueh
 * <p> bug# 622 Disable A, B, and Both menu items when doing a single axis
 * <p> tomogram.
 * <p>
 * <p> Revision 3.21  2005/03/30 23:44:14  sueh
 * <p> bug# 622 Removed the divider in Axis A only and B only.
 * <p>
 * <p> Revision 3.20  2005/02/17 20:24:47  sueh
 * <p> Removed unused code.
 * <p>
 * <p> Revision 3.19  2005/02/11 23:14:14  sueh
 * <p> bug# 594 Removing the Window menu, since it is redundant.
 * <p>
 * <p> Revision 3.18  2005/02/09 22:29:53  sueh
 * <p> bug# 594 Calling pack() for a new window and MainPanel.fitWindow() for
 * <p> an existing window.  MainPanel.fitWindow() functionality doesn't work
 * <p> when opening Etomo.
 * <p>
 * <p> Revision 3.17  2005/02/09 20:51:36  sueh
 * <p> bug# 594 Moved maximumSize from MainPanel to MainFrame so that it
 * <p> will work with the tabbedPane.  Changing rootPanel to Border layout.
 * <p>
 * <p> Revision 3.16  2005/02/07 22:42:52  sueh
 * <p> bug# 594 Removed setWindowMenuLabels(ConstHashedArray).  Adding
 * <p> functions to call WindowSwitch.add, remove, and rename().  In
 * <p> setCurrentManager(), removing everything from rootPanel before adding
 * <p> result of WindowSwitch.getPanel().
 * <p>
 * <p> Revision 3.15  2005/01/27 00:11:49  sueh
 * <p> bug# 543 For Ctrl-F (fit window) call MainPanel.fitWindow() with force set to
 * <p> true, to cause a fit window with autofit off.
 * <p>
 * <p> Revision 3.14  2004/12/02 20:41:30  sueh
 * <p> bug# 566 Added Join Guide menu item.
 * <p>
 * <p> Revision 3.13  2004/11/19 23:58:35  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.12.2.16  2004/11/19 00:23:00  sueh
 * <p> bug# 520 Enabling or disabling Save As according to whether the current
 * <p> manager will allow its param file to be changed.
 * <p>
 * <p> Revision 3.12.2.15  2004/10/28 22:16:20  sueh
 * <p> bug# 520 Changes HashedArray function to a set a value to an existing
 * <p> key from add() to set().
 * <p>
 * <p> Revision 3.12.2.14  2004/10/18 19:12:24  sueh
 * <p> bug# 520 In menuFileMRUListAction changed call to openManager instead
 * <p> of openTomogram, so Joins can be opened this way.
 * <p>
 * <p> Revision 3.12.2.13  2004/10/15 00:48:52  sueh
 * <p> bug# 520 Removed getTestParamFilename() because it is used to get
 * <p> either .edf or .ejf, but not both.  Changed openEtomoDataFileDialog() to
 * <p> openDataFileDialog() and changed the function to get either .ejf or .edf
 * <p> files.
 * <p>
 * <p> Revision 3.12.2.12  2004/10/11 02:14:34  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.12.2.11  2004/10/08 16:33:08  sueh
 * <p> bug# 520 Moved SettingsDialog to EtomoDirector.  Since EtomoDirector
 * <p> is a singleton, made all functions and member variables non-static.
 * <p> Made openMessageDialog() available to EtomoDirector.
 * <p>
 * <p> Revision 3.12.2.10  2004/10/07 16:38:40  sueh
 * <p> bug# 520 formatted
 * <p>
 * <p> Revision 3.12.2.9  2004/10/06 02:24:30  sueh
 * <p> bug# 520 Created functions to enable and disable New Tomogram and
 * <p> New Join menu items.
 * <p>
 * <p> Revision 3.12.2.8  2004/10/01 19:59:57  sueh
 * <p> bug# 520 Standardized getting the metadata file name.
 * <p>
 * <p> Revision 3.12.2.7  2004/09/15 22:40:52  sueh
 * <p> bug# 520 making openMessageDialog public
 * <p>
 * <p> Revision 3.12.2.6  2004/09/13 20:24:20  sueh
 * <p> bug# 520 Change MRUlist action to open a tomogram in a new window
 * <p> using EtomoDirector.
 * <p>
 * <p> Revision 3.12.2.5  2004/09/13 17:19:12  sueh
 * <p> bug# 520 Changed file menu:  changed New to New Tomogram, added
 * <p> New Join, added Close to close individual managers.  Changed Window
 * <p> menu storeage to a HashedArray with the same keys as
 * <p> EtomoDirectory.managerList.  This makes it easier to build the menu and
 * <p> to access it when a menu item is selected.  Added JPanel.repaint() call
 * <p> to fix a problem where the Setup dialog is not repainting which it is
 * <p> selected in the Window menu.  Renamed setCurrentWindowLabels() to
 * <p> setWindowMenuLabels().  Added numbering to tomogram datasets and
 * <p> joins listed under the window menu, because they may not have unique
 * <p> names.  Changed the menu items under window to JCheckBoxMenuItem
 * <p> and created a function to check one menu item.  Handling most of the
 * <p> file menu items with EtomoDirector.
 * <p>
 * <p> Revision 3.12.2.4  2004/09/09 22:12:55  sueh
 * <p> bug# 520 make etomo switch between datasets by removing and adding
 * <p> MainPanel
 * <p>
 * <p> Revision 3.12.2.3  2004/09/09 17:39:03  sueh
 * <p> bug# 520 Add the Window menu.  Add current window list menu item.
 * <p> Move call to createMenus out of MainFrame() because the list of managers
 * <p> hasn't been built when MainFrame is created.  Create
 * <p> setCurrentWindowLabels() which created a new current window menu list.
 * <p>
 * <p> Revision 3.12.2.2  2004/09/08 22:37:34  sueh
 * <p> bug# 520 removed fields that only belong in MainPanel
 * <p>
 * <p> Revision 3.12.2.1  2004/09/07 17:59:47  sueh
 * <p> bug# 520 encapsulated mainPanel
 * <p>
 * <p> Revision 3.12  2004/07/24 01:53:52  sueh
 * <p> bug# 513 make sure that packAxis() won't fail if it is called
 * <p> when the Setup dialog is running.
 * <p>
 * <p> Revision 3.11  2004/07/24 01:48:04  sueh
 * <p> bug# 513 making fitWindow() public so that app manager can
 * <p> call it
 * <p>
 * <p> Revision 3.10  2004/07/23 23:00:00  sueh
 * <p> bug# 517, bug# 513 comments, renamed functions for clarity,
 * <p> removed code in setDividerPostion() which was making
 * <p> fitWindow() fail when the frame was taller then the screen,
 * <p> moved tooSmall() from AxisPanelPRocess and renamed it
 * <p> isFitScreenError.
 * <p>
 * <p> Revision 3.9  2004/07/23 00:08:16  sueh
 * <p> bug# 517 Don't use setSize() anymore because the layout
 * <p> manager doesn't reliably fix the layout after this call.
 * <p> To get the layout manager to limit the size of the dialogs
 * <p> to the viewable area, and a new rootPanel with a BoxLayout,
 * <p> make it the root of the mainPanel.
 * <p>
 * <p> Revision 3.8  2004/07/21 21:08:33  sueh
 * <p> bug# 512 added options menu items Axis A, Axis B, and Both
 * <p> Axes.  Removed options menu item Advanced
 * <p>
 * <p> Revision 3.7  2004/07/16 23:01:03  sueh
 * <p> bug# 501 sending System.out prints only when debug is set
 * <p>
 * <p> Revision 3.6  2004/07/16 22:05:13  sueh
 * <p> bug# 501 adjusting divider to fix problem with
 * <p> JsplitPane.resetToPreferedSizes() that happens when
 * <p> etomo is too wide for the screen
 * <p>
 * <p> Revision 3.5  2004/05/19 23:17:14  sueh
 * <p> bug# 425 fixing single axis bug
 * <p>
 * <p> Revision 3.4  2004/05/19 23:11:47  sueh
 * <p> bug# 425 if the window is too tall for the screen, resize it
 * <p>
 * <p> Revision 3.3  2004/05/15 01:42:12  sueh
 * <p> bug# 415 MainFrame is already calling System.exit(), so don't
 * <p> do EXIT_ON_CLOSE when X button is pressed.
 * <p>
 * <p> Revision 3.2  2004/04/28 22:38:14  sueh
 * <p> bug# 268 if a panel was hidden, set the divider location to
 * <p> continue to hide it
 * <p>
 * <p> Revision 3.1  2004/02/13 17:39:03  sueh
 * <p> bug# 268 during fit window, if the split bar is all the way to
 * <p> one side, hide the hidden side, resize as usual, then show it
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.21  2003/11/04 22:02:17  sueh
 * <p> bug329 MainFrame: setMRUFileLabels(): Don't display blank
 * <p> MRU entries
 * <p>
 * <p> Revision 2.20  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p> Revision 2.19  2003/11/03 19:36:09  sueh
 * <p> bug266 EtomoFileFilter:  added an implements clause to the class def to allow the use
 * <p> of this file with File.listFiles(FileFilter)
 * <p>
 * <p> Revision 2.18  2003/10/30 20:29:55  rickg
 * <p> Bug# 341 3dmod and eTomo users guide
 * <p>
 * <p> Revision 2.17  2003/10/15 17:27:05  sueh
 * <p> Bug266 added a file filter to getTestParamFilename()
 * <p>
 * <p> Revision 2.16  2003/09/30 03:13:38  rickg
 * <p> bug248 File / open now calls the correct function in the
 * <p> AppManager (as does the MRU list opens)
 * <p>
 * <p> Revision 2.15  2003/09/30 02:15:09  rickg
 * <p> Added message dialogs that were originally in the
 * <p> ApplicationManager
 * <p>
 * <p> Revision 2.14  2003/06/10 05:14:53  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.13  2003/05/27 08:54:18  rickg
 * <p> Determinant progress bar now takes a string
 * <p>
 * <p> Revision 2.12  2003/05/23 14:22:38  rickg
 * <p> Progress bar determinant delegate methods
 * <p> axisPanel mapping method
 * <p>
 * <p> Revision 2.11  2003/05/19 22:10:03  rickg
 * <p> Added new to file menu
 * <p> Added tomography guide and imod guide to help menu
 * <p> Restructured action handlers
 * <p>
 * <p> Revision 2.10  2003/05/19 04:54:18  rickg
 * <p> Added mnemonics for menus
 * <p>
 * <p> Revision 2.9  2003/05/15 22:25:14  rickg
 * <p> created separate setDividerLocation method to correctly update
 * <p> panel
 * <p>
 * <p> Revision 2.8  2003/05/15 20:21:22  rickg
 * <p> Added extra validation call hopefully to make sure divider gets rendered
 * <p> correctly
 * <p>
 * <p> Revision 2.7  2003/05/08 19:58:25  rickg
 * <p> Addd post processing state update on an updateAll... call
 * <p>
 * <p> Revision 2.6  2003/05/07 17:49:12  rickg
 * <p> System property user.dir now defines the working directory
 * <p> Updated status bar
 * <p>
 * <p> Revision 2.5  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.4  2003/03/20 17:42:18  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.3  2003/01/29 15:18:19  rickg
 * <p> Added combine state setter
 * <p>
 * <p> Revision 2.2  2003/01/28 00:15:43  rickg
 * <p> Main window now remembers its size
 * <p>
 * <p> Revision 2.1  2003/01/27 23:51:23  rickg
 * <p> Added a split pane manager to the mane window for dual
 * <p> axis layout
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.9.2.2  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.9.2.1  2003/01/11 00:45:27  rickg
 * <p> Added app icon
 * <p>
 * <p> Revision 1.9  2002/12/18 20:45:32  rickg
 * <p> Advanced menu implemented
 * <p>
 * <p> Revision 1.8  2002/12/11 21:28:29  rickg
 * <p> Implemented repaint method, doesn't work well
 * <p>
 * <p> Revision 1.7  2002/12/11 00:37:26  rickg
 * <p> Added handler for options/settings menu
 * <p>
 * <p> Revision 1.6  2002/12/09 04:42:29  rickg
 * <p> Automatically add .edf extenstion when doing save as or
 * <p> save with not existing filename
 * <p>
 * <p> Revision 1.5  2002/12/09 04:16:11  rickg
 * <p> Added EDF file filter to open dialog
 * <p>
 * <p> Revision 1.4  2002/11/19 05:32:55  rickg
 * <p> Label spelling correction
 * <p>
 * <p> Revision 1.3  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public final class MainFrame extends EtomoFrame implements ContextMenu {
  public static final String rcsid = "$Id$";

  static final int EXTRA_SCREEN_WIDTH_MULTIPLIER = 2;
  static final Dimension FRAME_BORDER = FixedDim.frameBorder;
  public static final String ETOMO_TITLE = "Etomo";
  public static final String NAME = "main-frame";

  private static final String aAxisTitle = "A Axis - ";
  private static final String bAxisTitle = "B Axis - ";

  // private JPanel contentPane;
  private final JPanel rootPanel;

  GenericMouseAdapter mouseAdapter = null;
  WindowSwitch windowSwitch = new WindowSwitch();
  private String title;
  private String[] mRUList;
  private boolean registered = false;

  /**
   * Main window constructor.  This sets up the menus and status line.
   */
  MainFrame() {
    register();
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    Dimension screenSize = UIUtilities.getScreenSize();
    screenSize.width *= EXTRA_SCREEN_WIDTH_MULTIPLIER;
    Dimension rootPanelSize = new Dimension(screenSize);
    rootPanelSize.height -= FRAME_BORDER.height;
    rootPanelSize.width -= FRAME_BORDER.width;

    rootPanel = (JPanel) getContentPane();
    rootPanel.setLayout(new BorderLayout());
    rootPanel.setMaximumSize(rootPanelSize);
    // set name
    String name = Utilities.convertLabelToName(NAME);
    rootPanel.setName(UITestFieldType.PANEL.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(UITestFieldType.PANEL.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }

    // rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.PAGE_AXIS));

    // add the context menu to all of the main window objects
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    super.initialize();
  }

  final synchronized void register() {
    if (registered) {
      throw new IllegalStateException("Only one instance of MainFrame is allowed.");
    }
    registered = true;
    mainFrame = this;
    main = true;
  }

  final FrameType getFrameType() {
    return FrameType.Main;
  }

  void setCurrentManager(BaseManager currentManager, UniqueKey managerKey,
      boolean newWindow) {
    // Hide log window from previous manager
    if (this.currentManager != null) {
      this.currentManager.msgCurrentManagerChanged(false);
    }
    setEnabled(currentManager);
    this.currentManager = currentManager;
    if (currentManager == null) {
      EtomoDirector.INSTANCE.makeOriginalDirLocal();
    }
    else {
      currentManager.msgCurrentManagerChanged(true);
      Utilities.managerStamp(currentManager.getPropertyUserDir(),
          currentManager.getName());
    }
    // Remove everything from rootPanel if the main panel has been set from the
    // previous manager.
    if (mainPanel != null) {
      rootPanel.removeAll();
    }
    if (currentManager == null) {
      title = ETOMO_TITLE;
      hideAxisB();
    }
    else {
      mainPanel = currentManager.getMainPanel();
      title = currentManager.getName() + " - " + ETOMO_TITLE;
      rootPanel.add(windowSwitch.getPanel(managerKey));
      toFront();
      mainPanel.addMouseListener(mouseAdapter);
      mainPanel.repaint();

      if (subFrame != null) {
        ((SubFrame) subFrame).setMainPanel(bAxisTitle + title + " ", currentManager);
      }
      if (newWindow) {
        showAxisA();
      }
      else {
        if (mainPanel.isShowingBothAxis()) {
          showBothAxis();
        }
        else if (mainPanel.isShowingAxisA()) {
          showAxisA();
        }
        else {
          showAxisB();
        }
      }
    }
  }

  void showHideLog() {
    if (currentManager != null) {
      currentManager.showHideLog();
    }
  }

  MainPanel getMainPanel() {
    return mainPanel;
  }

  void setCurrentManager(BaseManager currentManager, UniqueKey managerKey) {
    setCurrentManager(currentManager, managerKey, false);
  }

  void setMRUFileLabels(String[] mRUList) {
    this.mRUList = mRUList;
    super.setMRUFileLabels(mRUList);
  }

  // Right mouse button context menu
  public void popUpContextMenu(MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(mainPanel, mouseEvent, "",
        currentManager, getAxisID());
  }

  void addWindow(BaseManager manager, UniqueKey managerKey) {
    windowSwitch.add(manager, managerKey);
  }

  void removeWindow(UniqueKey managerKey) {
    windowSwitch.remove(managerKey);
  }

  void renameWindow(UniqueKey oldKey, UniqueKey newManagerKey) {
    windowSwitch.rename(oldKey, newManagerKey);
  }

  void selectWindowMenuItem(UniqueKey currentManagerKey) {
    selectWindowMenuItem(currentManagerKey, false);
  }

  void selectWindowMenuItem(UniqueKey currentManagerKey, boolean newWindow) {
    windowSwitch.selectWindow(currentManagerKey, newWindow);
  }

  /**
   * Handle the view menu events
   * @param event
   */
  public void menuViewAction(ActionEvent event) {
    if (menu.equalsAxisA(event)) {
      showAxisA();
    }
    else if (menu.equalsAxisB(event)) {
      showAxisB();
    }
    else if (menu.equalsAxisBoth(event)) {
      showBothAxis();
    }
    else if (menu.equalsLogWindow(event)) {
      showHideLog();
    }
    else {
      super.menuViewAction(event);
    }
  }

  private void setTitle(AxisID axisID) {
    if (mainPanel != null && mainPanel.getAxisType() == AxisType.DUAL_AXIS) {
      if (axisID == AxisID.FIRST) {
        setTitle(aAxisTitle + title);
      }
      else if (axisID == AxisID.SECOND) {
        setTitle(bAxisTitle + title);
      }
      else {
        setTitle(title);
      }
    }
    else {
      setTitle(title);
    }
  }

  void hideAxisB() {
    setTitle(title);
    if (subFrame != null) {
      subFrame.setVisible(false);
    }
    pack();
  }

  void showAxisA() {
    setTitle(AxisID.FIRST);
    if (subFrame != null) {
      subFrame.setVisible(false);
    }
    mainPanel.showAxisA();
    UIHarness.INSTANCE.pack(AxisID.FIRST, currentManager);
  }

  void showAxisB() {
    setTitle(AxisID.SECOND);
    if (subFrame != null) {
      subFrame.setVisible(false);
    }
    mainPanel.showAxisB();
    UIHarness.INSTANCE.pack(AxisID.SECOND, currentManager);
  }

  void showBothAxis() {
    setTitle(aAxisTitle + title);
    mainPanel.showAxisA();
    if (subFrame == null || !subFrame.isDisplayable()) {
      subFrame = new SubFrame(this);
      ((SubFrame) subFrame).initialize(bAxisTitle + title + " ", currentManager, mRUList);
    }
    else {
      subFrame.setVisible(true);
    }
    UIHarness.INSTANCE.pack(currentManager);
    subFrame.pack();
  }

  /**Overridden so we can exit when window is closed*/
  protected void processWindowEvent(WindowEvent event) {
    super.processWindowEvent(event);
    if (event.getID() == WindowEvent.WINDOW_CLOSING
        && !EtomoDirector.INSTANCE.getArguments().isTest()) {
      menu.doClickFileExit();
    }
  }
}