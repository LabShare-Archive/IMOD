package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.storage.BackupFileFilter;
import etomo.storage.IntermediateFileFilter;

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
  JButton btnDelete = new JButton("<html><b>Delete Selected</b>");
  JButton btnRescanDir = new JButton("<html><b>Rescan Directory</b>");

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
    intermediateFileFilter =
      new IntermediateFileFilter(applicationManager.getDatasetName());
    backupFileFilter = new BackupFileFilter();

    fileChooser = new JFileChooser();
    fileChooser.setDialogType(JFileChooser.CUSTOM_DIALOG);
    fileChooser.setFileFilter(backupFileFilter);
    fileChooser.setFileFilter(intermediateFileFilter);
    fileChooser.setMultiSelectionEnabled(true);
    fileChooser.setControlButtonsAreShown(false);
    fileChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));

    pnlCleanup.setLayout(new BoxLayout(pnlCleanup, BoxLayout.Y_AXIS));
    pnlCleanup.setBorder(
      new BeveledBorder("Intermediate file cleanup").getBorder());
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
        applicationManager.openMessageDialog(
          message,
          "Unable to delete intermediate file");
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
