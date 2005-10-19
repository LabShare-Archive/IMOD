package etomo.ui;

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
  public static  final String  rcsid =  "$Id$";
  
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

    cleanupPanel = new CleanupPanel(applicationManager);
    rootPanel.add(cleanupPanel.getContainer());
    addExitButtons();
    btnAdvanced.setVisible(false);
    btnExecute.setText("Done");


    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced dialog state
    setToolTipText();
    updateAdvanced();
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
    new ContextPopup(rootPanel, mouseEvent, "Cleaning Up", ContextPopup.TOMO_GUIDE);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }
  
  //
  //  Action function overides for buttons
  //
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneCleanUp();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneCleanUp();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneCleanUp();
  }
  
  private void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnArchiveStack.getText())) {
      applicationManager.archiveOriginalStack();
    }
  }
  
  /**
   * Initialize the tooltip text
   */
   private void setToolTipText() {
     String text;
     TooltipFormatter tooltipFormatter = new TooltipFormatter();

     text = "Run archiveorig.  Archiveorig creates a _xray.st.gz file, which" +
         " contains the difference between the .st file and the _orig.st " +
         " file.  If archiveorig succeeds, then you can delete the _orig.st " +
         "file.  To restore _orig.st, go to the directory containing the " +
         "_xray.st.gz file and run \"archiveorig -r\" on the .st file.";
     btnArchiveStack.setToolTipText(tooltipFormatter.setText(text).format());
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
