/**
 * <p>Description:A generic process dialog box with a set of exit buttons
 * and the action adapter to handle their processing.  The action functions
 * can be overriden to implement the required functionality. </p>
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
 * <p> Revision 3.4  2005/01/14 03:10:48  sueh
 * <p> bug# 511 Added a DialogType member variable to identify the dialog the
 * <p> ProcessDialog the parent of.  Changed default exitState to SAVE. added
 * <p> isAdvanced() and getDialogType().
 * <p>
 * <p> Revision 3.3  2004/11/20 00:02:01  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.2.4.1  2004/10/08 16:34:54  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 3.2  2004/04/26 03:18:54  rickg
 * <p> Normalized button size
 * <p>
 * <p> Revision 3.1  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/10/13 20:27:21  sueh
 * <p> bug270
 * <p> changed tooltips
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.DialogExitState;
import etomo.type.DialogType;

public class ProcessDialog implements ExitButtons {
  public static final String rcsid = "$Id$";

  protected ApplicationManager applicationManager;

  protected AxisID axisID;

  protected boolean isAdvanced;

  protected DialogExitState exitState = DialogExitState.SAVE;

  protected Dimension rootSize = new Dimension(620, 680);

  protected JPanel rootPanel = new JPanel();

  //  Exit buttons
  protected JPanel pnlExitButtons = new JPanel();

  protected MultiLineButton btnCancel = new MultiLineButton("Cancel");
  
  protected MultiLineButton btnPostpone = new MultiLineButton("Postpone");

  protected MultiLineButton btnExecute = new MultiLineButton("Execute");

  protected MultiLineButton btnAdvanced = new MultiLineButton("Advanced");
  
  protected DialogType dialogType;

  /**
   * Create a new process dialog with a set of exit buttons (cancel, postpone
   * execute, and advanced) available for use.  The action adapters for the
   * buttons are already implemented.
   */
  public ProcessDialog(ApplicationManager appManager, AxisID axisID, DialogType dialogType) {
    applicationManager = appManager;
    this.axisID = axisID;
    this.dialogType = dialogType;

    EtomoDirector.getInstance().getMainFrame().setEnabledFit(true);
    //  Get the default initial advanced state
    isAdvanced = appManager.isAdvanced(dialogType, axisID);
    setAdvanced(isAdvanced);
    setToolTipText();

    //  Layout the buttons
    pnlExitButtons.setLayout(new BoxLayout(pnlExitButtons, BoxLayout.X_AXIS));

    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnCancel);
    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnPostpone);
    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnExecute);
    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnAdvanced);
    pnlExitButtons.add(Box.createHorizontalGlue());

    UIUtilities.setButtonSizeAll(pnlExitButtons, UIParameters
      .getNarrowButtonDimension());

    //  Exit action listeners
    btnCancel.addActionListener(new buttonCancelActionAdapter(this));
    btnPostpone.addActionListener(new buttonPostponeActionAdapter(this));
    btnExecute.addActionListener(new buttonExecuteActionAdapter(this));
    btnAdvanced.addActionListener(new buttonAdvancedActionAdapter(this));
  }

  public Container getContainer() {
    return rootPanel;
  }
  
  public DialogType getDialogType() {
    return dialogType;
  }
  
  public boolean isAdvanced() {
    return isAdvanced;
  }

  public void fixRootPanel(Dimension size) {
    //    rootPanel.setMinimumSize(rootSize);
    //    rootPanel.setPreferredSize(rootSize);
    //    rootPanel.setMaximumSize(rootSize);
  }

  /**
   * Action to take when the cancel button is pressed, the default action is
   * to set the exitState attribute to CANCEL.
   */
  public void buttonCancelAction(ActionEvent event) {
    exitState = DialogExitState.CANCEL;
  }

  /**
   * Action to take when the postpone button is pressed, the default action is
   * to set the exitState attribute to POSTPONE.
   */
  public void buttonPostponeAction(ActionEvent event) {
    exitState = DialogExitState.POSTPONE;
  }

  /**
   * Action to take when the execute button is pressed, the default action is
   * to set the exitState attribute to EXECUTE.
   */
  public void buttonExecuteAction(ActionEvent event) {
    if (validateInput()) {
      exitState = DialogExitState.EXECUTE;
    }
  }

  /**
   * Action to take when the advanced button is pressed, this method
   * toggles the the isAdvanced attribute as well as the state of the advanced
   * button.  Call this method first before checking the state of isAdvanced.
   */
  public void buttonAdvancedAction(ActionEvent event) {
    if (btnAdvanced.isVisible()) {
      EtomoDirector.getInstance().getMainFrame().setEnabledFit(true);
    }
    setAdvanced(!isAdvanced);
  }

  /**
   * Set the advanced state variable and update the button text
   */
  void setAdvanced(boolean state) {
    isAdvanced = state;
    if (isAdvanced) {
      btnAdvanced.setText("Basic");
    }
    else {
      btnAdvanced.setText("Advanced");
    }
  }

  public boolean validateInput() {
    return true;
  }

  public DialogExitState getExitState() {
    return exitState;
  }

  //
  //  Default tool tip text for the buttons.
  //
  private void setToolTipText() {
    String line1, line2, line3, line4;

    line1 = "<html>This button will abort any changes to the parameters<br>";
    line2 = "in this dialog box and return you to the main window.";
    btnCancel.setToolTipText(line1 + line2);

    line1 = "<html>This button will save any changes to the parameters<br>";
    line2 = "in this dialog box and return you to the main window<br>";
    line3 = "without executing any of the processing.  Any parameter<br>";
    line4 = "changes will also be written to the com scripts.";
    btnPostpone.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>This button will save any changes to the parameters<br>";
    line2 = "in this dialog box and execute the specified operation<br>";
    line3 = "on the data.  Any parameter changes will also be written<br>";
    line4 = "to the com scripts.";
    btnExecute.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>This button will present a more detailed set of<br>";
    line2 = "options for each of the underlying processes.";
    btnAdvanced.setToolTipText(line1 + line2);
  }
}

/**
 * Defines four button events for the cancel, postpose, execute and advanced
 * buttons
 */
interface ExitButtons {
  void buttonCancelAction(ActionEvent event);

  void buttonPostponeAction(ActionEvent event);

  void buttonExecuteAction(ActionEvent event);

  void buttonAdvancedAction(ActionEvent event);
}

/**
 *  Action adapters to bind the buttons their action functions
 */
class buttonCancelActionAdapter implements java.awt.event.ActionListener {

  ProcessDialog adaptee;

  buttonCancelActionAdapter(ProcessDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent e) {
    adaptee.buttonCancelAction(e);
  }
}

class buttonPostponeActionAdapter implements java.awt.event.ActionListener {

  ProcessDialog adaptee;

  buttonPostponeActionAdapter(ProcessDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent e) {
    adaptee.buttonPostponeAction(e);
  }
}

class buttonExecuteActionAdapter implements java.awt.event.ActionListener {

  ProcessDialog adaptee;

  buttonExecuteActionAdapter(ProcessDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent e) {
    adaptee.buttonExecuteAction(e);
  }
}

class buttonAdvancedActionAdapter implements java.awt.event.ActionListener {

  ProcessDialog adaptee;

  buttonAdvancedActionAdapter(ProcessDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent e) {
    adaptee.buttonAdvancedAction(e);
  }
}