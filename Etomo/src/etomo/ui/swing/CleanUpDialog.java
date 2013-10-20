package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JLabel;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DialogType;

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
public class CleanUpDialog extends ProcessDialog implements ContextMenu {
  public static final String rcsid = "$Id$";

  private CleanupPanel cleanupPanel;
  private MultiLineButton btnArchiveStack = new MultiLineButton();
  private JLabel archiveInfoA = new JLabel();
  private JLabel archiveInfoB = new JLabel();

  private AxisType axisType;

  public CleanUpDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.ONLY, DialogType.CLEAN_UP);
    this.axisType = appMgr.getBaseMetaData().getAxisType();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("Clean Up").getBorder());
    String stackFileName;
    //Add archive original stack button
    if (axisType == AxisType.DUAL_AXIS) {
      btnArchiveStack.setText("Archive Original Stacks");
    }
    else {
      btnArchiveStack.setText("Archive Original Stack");
      archiveInfoB.setVisible(false);
    }
    setArchiveFields();
    ButtonActionListener listener = new ButtonActionListener(this);
    btnArchiveStack.addActionListener(listener);
    btnArchiveStack.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnArchiveStack.setSize();
    archiveInfoA.setAlignmentX(Component.CENTER_ALIGNMENT);
    archiveInfoB.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(btnArchiveStack.getComponent());
    rootPanel.add(archiveInfoA);
    rootPanel.add(archiveInfoB);

    cleanupPanel = CleanupPanel.getInstance(applicationManager);
    rootPanel.add(cleanupPanel.getContainer());
    addExitButtons();
    btnAdvanced.setVisible(false);
    btnExecute.setText("Done");

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    setToolTipText();
  }

  public final void updateArchiveDisplay(boolean originalStacksExist) {
    if (btnArchiveStack == null) {
      return;
    }
    btnArchiveStack.setEnabled(originalStacksExist);
  }

  /**
   * Set the status of the archive information labels.
   */
  public void setArchiveFields() {
    String archiveInfoText = "To restore original stack, run:  archiveorig -r ";
    String stackFileName;
    //if archiveorig has been run, put information about restoring the
    //originals on the screen.
    if (axisType == AxisType.DUAL_AXIS) {
      stackFileName = applicationManager.getArchiveInfo(AxisID.FIRST);
      if (stackFileName != null) {
        archiveInfoA.setText(archiveInfoText + stackFileName);
        archiveInfoA.setVisible(true);
      }
      else {
        archiveInfoA.setVisible(false);
      }
      stackFileName = applicationManager.getArchiveInfo(AxisID.SECOND);
      if (stackFileName != null) {
        archiveInfoB.setText(archiveInfoText + stackFileName);
        archiveInfoB.setVisible(true);
      }
      else {
        archiveInfoB.setVisible(false);
      }
    }
    else {
      stackFileName = applicationManager.getArchiveInfo(AxisID.ONLY);
      if (stackFileName != null) {
        archiveInfoA.setText("To restore original stack run:  archiveorig -r "
            + stackFileName);
        archiveInfoA.setVisible(true);
      }
      else {
        archiveInfoA.setVisible(false);
      }
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    new ContextPopup(rootPanel, mouseEvent, "Cleaning Up", ContextPopup.TOMO_GUIDE,
        applicationManager, axisID);
  }

  void done() {
    applicationManager.doneCleanUp();
    setDisplayed(false);
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  protected void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnArchiveStack.getText())) {
      applicationManager.archiveOriginalStack(null, dialogType);
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    btnArchiveStack
        .setToolTipText("Run archiveorig.  Archiveorig creates a _xray.st.gz file, which"
            + " contains the difference between the .st file and the _orig.st "
            + " file.  If archiveorig succeeds, then you can delete the _orig.st "
            + "file.  To restore _orig.st, go to the directory containing the "
            + "_xray.st.gz file and run \"archiveorig -r\" on the .st file.");
  }

  /*
   *  
   */
  private class ButtonActionListener implements ActionListener {
    CleanUpDialog adaptee;

    ButtonActionListener(CleanUpDialog cleanUpDialog) {
      adaptee = cleanUpDialog;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

}
/**
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.17  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.16  2008/05/28 02:49:38  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 1.15  2008/05/03 00:48:59  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.14  2007/12/28 21:15:13  sueh
 * <p> bug# 726 Added directory size to clean up panel.
 * <p>
 * <p> Revision 1.13  2007/12/26 22:23:05  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 1.12  2007/02/09 00:48:07  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.11  2006/06/30 20:00:37  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.
 * <p>
 * <p> Revision 1.10  2006/06/21 15:50:25  sueh
 * <p> bug# 581 Passing manager and axis to ContextPopup, so that imodqtassist can
 * <p> be run.
 * <p>
 * <p> Revision 1.9  2005/11/14 21:39:38  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 1.8  2005/10/19 00:18:15  sueh
 * <p> bug# 673 Added updateArchiveDisplay() to enable/disable the archive
 * <p> button.
 * <p>
 * <p> Revision 1.7  2005/08/10 20:40:31  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 1.6  2005/08/09 20:19:48  sueh
 * <p> bug# 711 Moving button sizing from UIUtilities to the multi line button
 * <p> classes.
 * <p>
 * <p> Revision 1.5  2005/08/04 20:08:10  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 1.4  2005/05/18 22:38:15  sueh
 * <p> bug# 662 Added btnArchiveStack, archiveInfoA, and archiveInfoB.
 * <p> Added setArchiveFields() to set text and display archiveInfoA and B.
 * <p> Added setToolTipText() and ButtonActionListener.
 * <p>
 * <p> Revision 1.3  2005/04/21 20:31:35  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 1.2  2005/04/16 01:53:59  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 1.1  2005/03/24 17:49:51  sueh
 * <p> bug# 621 Moved the clean up panel in post processing to a separate
 * <p> dialog.
 * <p> </p>
 */
