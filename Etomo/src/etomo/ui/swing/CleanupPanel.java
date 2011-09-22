package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.storage.FileFilterCollection;
import etomo.storage.IntermediateFileFilter;
import etomo.storage.SirtOutputFileFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.util.Utilities;

/**
 * <p>
 * Description:
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 1.2  2011/02/22 18:06:06  sueh
 * bug# 1437 Reformatting.
 *
 * Revision 1.1  2010/11/13 16:07:34  sueh
 * bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 *
 * Revision 3.19  2010/02/17 05:03:12  sueh
 * bug# 1301 Using manager instead of manager key for popping up messages.
 *
 * Revision 3.18  2009/10/01 18:49:58  sueh
 * bug# 1233 In setDirSize checking for null to make sure that etomo can't
 * get stuck when leaving a dialog.
 *
 * Revision 3.17  2009/03/17 00:46:24  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 3.16  2009/01/20 19:50:37  sueh
 * bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 *
 * Revision 3.15  2007/12/28 21:15:27  sueh
 * bug# 726 Added directory size to clean up panel.
 *
 * Revision 3.14  2007/02/09 00:48:16  sueh
 * bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * classes.
 *
 * Revision 3.13  2006/07/05 23:25:45  sueh
 * Get rid of multiple error messages when files are deleted.
 *
 * Revision 3.12  2005/11/14 21:40:04  sueh
 * bug# 762 Made buttonAction() protected.
 *
 * Revision 3.11  2005/08/10 20:41:37  sueh
 * bug# 711 Moved button sizing to MultiLineButton.  Made clean up buttons
 * the standard size.
 *
 * Revision 3.10  2005/08/09 20:20:15  sueh
 * bug# 711  No longer inheriting JButton in MultiLineButton.  This allows
 * MultiLineButton to treate toggling as an attribute.  Then we can get rid of
 * MultiLineToggleButton.  Then we can have one Run3dmodButton which
 * can be toggle or non-toggle.
 *
 * Revision 3.9  2005/06/16 22:08:49  sueh
 * bug# 625 Changed deleteSelected() to not remove the file names from the
 * File Name field if the delete failed.
 *
 * Revision 3.8  2005/06/16 22:05:39  sueh
 * bug# 625 Changed deleteSelected() to remove the file names from the
 * File Name field after the files have been deleted.
 *
 * Revision 3.7  2005/05/18 22:39:13  sueh
 * bug# 662 Moved the beveled border to the outer panel.
 *
 * Revision 3.6  2005/04/25 20:54:42  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.  Move the interface for
 * popping up message dialogs to UIHarness.  It prevents headless
 * exceptions during a test execution.  It also allows logging of dialog
 * messages during a test.  It also centralizes the dialog interface and
 * allows the dialog functions to be synchronized to prevent dialogs popping
 * up in both windows at once.  All Frame functions will use UIHarness as a
 * public interface.
 *
 * Revision 3.5  2005/03/30 21:05:24  sueh
 * bug# 621 corrected panel title.
 *
 * Revision 3.4  2005/03/29 23:52:19  sueh
 * bug# 618 To prevent the user from deleting their tomogram when they
 * decide not to trim, setting
 * IntermediateFileFilter.acceptPretrimmedTomograms to true only if a
 * trimmed tomogram exists.
 *
 * Revision 3.3  2004/11/19 23:49:43  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.2.4.3  2004/10/11 02:10:41  sueh
 * bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * property.  This property would need a different value for each manager.
 * This variable can be retrieved from the manager if the object knows its
 * manager.  Otherwise it can retrieve it from the current manager using the
 * EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * gets the value from the "user.dir" property.
 *
 * Revision 3.2.4.2  2004/09/15 22:39:14  sueh
 * bug# 520 call openMessageDialog in mainPanel instead of mainFrame
 *
 * Revision 3.2.4.1  2004/09/07 17:58:53  sueh
 * bug# 520 getting dataset name from metadata
 *
 * Revision 3.2  2004/01/30 22:44:54  sueh
 * bug# 356 Changing buttons with html labels to
 * MultiLineButton and MultiLineToggleButton
 *
 * Revision 3.1  2003/11/10 07:38:21  rickg
 * Task tags moved to bugzilla
 *
 * Revision 3.0  2003/11/07 23:19:01  rickg
 * Version 1.0.0
 *
 * Revision 1.5  2003/10/29 20:49:11  rickg
 * Bug# 308 Tooltips
 *
 * Revision 1.4  2003/10/16 17:07:44  rickg
 * Bug# 305 Label changes, backup file filter
 *
 * <p>
 * Revision 1.3 2003/05/07 17:51:45 rickg
 * <p>
 * System property user.dir now defines the working directory
 * <p>
 * <p>
 * Revision 1.2 2003/04/24 17:46:54 rickg
 * <p>
 * Changed fileset name to dataset name
 * <p>
 * <p>
 * Revision 1.1 2003/04/17 23:12:00 rickg
 * <p>
 * Initial revision
 * <p>
 * </p>
 */
/**
 * @author rickg
 * 
 * To change this generated comment go to Window>Preferences>Java>Code
 * Generation>Code Template
 */
final class CleanupPanel {
  public static final String rcsid = "$Id$";

  private final EtomoPanel pnlCleanup = new EtomoPanel();
  private final JLabel instructions = new JLabel(
      "Select files to be deleted then press the \"Delete Selected\" button. Ctrl-A selects all displayed files.");
  private final JPanel pnlButton = new JPanel();
  private final MultiLineButton btnDelete = new MultiLineButton("Delete Selected");
  private final MultiLineButton btnRescanDir = new MultiLineButton("Rescan Directory");
  private final JFileChooser fileChooser = new JFileChooser();
  private final JLabel lDirSize = new JLabel();
  private final ApplicationManager applicationManager;

  private CleanupPanel(final ApplicationManager appMgr) {
    applicationManager = appMgr;
    // Set the button sizes
    btnDelete.setSize();
    btnRescanDir.setSize();

    // Create the filechooser
    String datasetName = applicationManager.getMetaData().getDatasetName();
    // Collect the file filters
    FileFilterCollection fileFilterCollection = new FileFilterCollection();
    IntermediateFileFilter intermediateFileFilter = new IntermediateFileFilter(
        datasetName);
    File trimmedTomogram = new File(applicationManager.getPropertyUserDir(), datasetName
        + ".rec");
    intermediateFileFilter.setAcceptPretrimmedTomograms(trimmedTomogram.exists());
    fileFilterCollection.addFileFilter(intermediateFileFilter);
    if (appMgr.getMetaData().getAxisType() == AxisType.DUAL_AXIS) {
      fileFilterCollection.addFileFilter(new SirtOutputFileFilter(appMgr, AxisID.FIRST,
          true, true, true));
      fileFilterCollection.addFileFilter(new SirtOutputFileFilter(appMgr, AxisID.SECOND,
          true, true, true));
    }
    else {
      fileFilterCollection.addFileFilter(new SirtOutputFileFilter(appMgr, AxisID.ONLY,
          true, true, true));
    }
    // Setup the file chooser
    fileChooser.setDialogType(JFileChooser.CUSTOM_DIALOG);
    fileChooser.setFileFilter(fileFilterCollection);
    fileChooser.setMultiSelectionEnabled(true);
    fileChooser.setControlButtonsAreShown(false);
    fileChooser.setCurrentDirectory(new File(applicationManager.getPropertyUserDir()));

    pnlCleanup.setLayout(new BoxLayout(pnlCleanup, BoxLayout.Y_AXIS));
    pnlCleanup.setBorder(new EtchedBorder("Intermediate File Cleanup").getBorder());
    instructions.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlCleanup.add(instructions);
    pnlCleanup.add(Box.createRigidArea(FixedDim.x0_y5));
    lDirSize.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlCleanup.add(lDirSize);
    pnlCleanup.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlCleanup.add(fileChooser);

    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnDelete.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnRescanDir.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlCleanup.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlCleanup.add(pnlButton);
    pnlCleanup.add(Box.createRigidArea(FixedDim.x0_y10));
    setToolTipText();
  }

  static CleanupPanel getInstance(final ApplicationManager manager) {
    CleanupPanel instance = new CleanupPanel(manager);
    instance.setDirSize();
    instance.addListeners();
    return instance;
  }

  /**
   * @return
   */
  Container getContainer() {
    return pnlCleanup;
  }

  private void addListeners() {
    ButtonActonListener buttonActionListener = new ButtonActonListener(this);
    btnDelete.addActionListener(buttonActionListener);
    btnRescanDir.addActionListener(buttonActionListener);
  }

  private void setDirSize() {
    File[] fileList = new File(applicationManager.getPropertyUserDir()).listFiles();
    long dirSize = 0;
    if (fileList != null) {
      for (int i = 0; i < fileList.length; i++) {
        if (fileList[i].isFile()) {
          dirSize += fileList[i].length();
        }
      }
    }
    lDirSize.setText("Directory size (MB): " + dirSize / 1000000);
  }

  /**
   * 
   *  
   */
  private void deleteSelected() {
    boolean deletedAll = true;
    File[] deleteList = fileChooser.getSelectedFiles();
    for (int i = 0; i < deleteList.length; i++) {
      if (!deleteList[i].delete()) {
        deletedAll = false;
      }
    }
    // if (deletedAll) {
    fileChooser.rescanCurrentDirectory();
    fileChooser.setSelectedFile(new File(""));
    if (!deletedAll) {
      StringBuffer message = new StringBuffer("Unable to delete file(s).  "
          + "Check file permissions.");
      if (Utilities.isWindowsOS()) {
        message.append("\nIf the files are open in 3dmod, close 3dmod.");
      }
      UIHarness.INSTANCE.openMessageDialog(applicationManager, message.toString(),
          "Unable to delete intermediate file", AxisID.ONLY);
    }
    setDirSize();
  }

  /**
   * @param event
   */
  private void buttonAction(final ActionEvent event) {
    if (event.getActionCommand() == btnDelete.getActionCommand()) {
      deleteSelected();
    }
    if (event.getActionCommand() == btnRescanDir.getActionCommand()) {
      fileChooser.rescanCurrentDirectory();
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    fileChooser.setToolTipText(TooltipFormatter.INSTANCE
        .format("The list of files in this text box will be deleted."));
    btnDelete.setToolTipText("Delete the files listed in the \"File name\" text box.");
    btnRescanDir
        .setToolTipText("Read the directory again to update the list in the file selection box.");
  }

  /*
   *  
   */
  private static final class ButtonActonListener implements ActionListener {
    private final CleanupPanel listenee;

    ButtonActonListener(final CleanupPanel cleanupPanel) {
      listenee = cleanupPanel;
    }

    public void actionPerformed(final ActionEvent event) {
      listenee.buttonAction(event);
    }
  }
}
