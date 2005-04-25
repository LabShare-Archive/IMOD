package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.storage.BackupFileFilter;
import etomo.storage.IntermediateFileFilter;
import etomo.type.AxisID;

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
public class CleanupPanel {
  public static final String rcsid =
    "$Id$";
  ApplicationManager applicationManager;

  private JPanel pnlCleanup = new JPanel();
  private JLabel instructions =
    new JLabel("Select files to be deleted then press the \"Delete Selected\" button. Ctrl-A selects all displayed files.");

  JFileChooser fileChooser;
  IntermediateFileFilter intermediateFileFilter;
  BackupFileFilter backupFileFilter;
  private JPanel pnlButton = new JPanel();
  MultiLineButton btnDelete = new MultiLineButton("<html><b>Delete Selected</b>");
  MultiLineButton btnRescanDir = new MultiLineButton("<html><b>Rescan Directory</b>");

  public CleanupPanel(ApplicationManager appMgr) {
    applicationManager = appMgr;

    double height = instructions.getPreferredSize().getHeight();

    //  Set the button sizes
    Dimension dimButton = new Dimension();
    dimButton.setSize(12 * height, 3 * height);
    btnDelete.setPreferredSize(dimButton);
    btnDelete.setMaximumSize(dimButton);
    btnRescanDir.setPreferredSize(dimButton);
    btnRescanDir.setMaximumSize(dimButton);

    //  Create the filechooser
    String datasetName = applicationManager.getMetaData().getDatasetName();
    intermediateFileFilter =
      new IntermediateFileFilter(datasetName);
    File trimmedTomogram = new File(applicationManager.getPropertyUserDir(), datasetName + ".rec");
    intermediateFileFilter.setAcceptPretrimmedTomograms(trimmedTomogram.exists());
    backupFileFilter = new BackupFileFilter();

    fileChooser = new JFileChooser();
    fileChooser.setDialogType(JFileChooser.CUSTOM_DIALOG);
    fileChooser.setFileFilter(backupFileFilter);
    fileChooser.setFileFilter(intermediateFileFilter);
    fileChooser.setMultiSelectionEnabled(true);
    fileChooser.setControlButtonsAreShown(false);
    fileChooser.setCurrentDirectory(new File(applicationManager.getPropertyUserDir()));

    pnlCleanup.setLayout(new BoxLayout(pnlCleanup, BoxLayout.Y_AXIS));
    pnlCleanup.setBorder(
      new BeveledBorder("Intermediate File Cleanup").getBorder());
    instructions.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlCleanup.add(instructions);
    pnlCleanup.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlCleanup.add(fileChooser);

    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnDelete);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnRescanDir);
    pnlButton.add(Box.createHorizontalGlue());
    pnlCleanup.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlCleanup.add(pnlButton);
    pnlCleanup.add(Box.createRigidArea(FixedDim.x0_y10));

    //  Bind the buttons to the action listener
    ButtonActonListener buttonActionListener = new ButtonActonListener(this);
    btnDelete.addActionListener(buttonActionListener);
    btnRescanDir.addActionListener(buttonActionListener);
    setToolTipText();
  }

  /**
   * @return
   */
  public Container getContainer() {
    return pnlCleanup;
  }

  /**
   * 
   *  
   */
  private void deleteSelected() {
    File[] deleteList = fileChooser.getSelectedFiles();
    for (int i = 0; i < deleteList.length; i++) {
      if (!deleteList[i].delete()) {
        String message[] = new String[2];
        message[0] = "Unable to delete " + deleteList[i].getName();
        message[1] = "Check file permissions";
        UIHarness.INSTANCE.openMessageDialog(message,
            "Unable to delete intermediate file", AxisID.ONLY);
      }
    }
    fileChooser.rescanCurrentDirectory();
  }

  /**
   * @param event
   */
  private void buttonAction(ActionEvent event) {
    if (event.getActionCommand() == btnDelete.getActionCommand()) {
      deleteSelected();
    }
    if (event.getActionCommand() == btnRescanDir.getActionCommand()) {
      fileChooser.rescanCurrentDirectory();
    }
  }

  /*
   *  
   */
  class ButtonActonListener implements ActionListener {
    CleanupPanel listenee;

    ButtonActonListener(CleanupPanel cleanupPanel) {
      listenee = cleanupPanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.buttonAction(event);
    }
  }
  /**
  * Initialize the tooltip text
  */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "The list of files in this text box will be deleted.";
    fileChooser.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Select the type of files to show in the file selection box.";

    text = "Delete the files listed in the \"File name\" text box.";
    btnDelete.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Read the directory again to update the list in the file selection box.";
    btnRescanDir.setToolTipText(tooltipFormatter.setText(text).format());
  }
}
