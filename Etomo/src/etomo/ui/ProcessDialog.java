package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.DialogExitState;

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
public class ProcessDialog implements ExitButtons {
  public static final String rcsid =
    "$Id$";

  protected ApplicationManager applicationManager;
  protected AxisID axisID;
  protected boolean isAdvanced;
  protected DialogExitState exitState = DialogExitState.CANCEL;

  protected Dimension rootSize = new Dimension(620, 680);
  protected JPanel rootPanel = new JPanel();

  //  Exit buttons
  protected JPanel panelExitButtons = new JPanel();
  protected JButton buttonCancel = new JButton("Cancel");
  protected JButton buttonPostpone = new JButton("Postpone");
  protected JButton buttonExecute = new JButton("Execute");
  protected JButton buttonAdvanced = new JButton("Advanced");

  /**
   * Create a new process dialog with a set of exit buttons (cancel, postpone
   * execute, and advanced) available for use.  The action adapters for the
   * buttons are already implemented.
   */
  public ProcessDialog(ApplicationManager appManager, AxisID axisID) {
    applicationManager = appManager;
    this.axisID = axisID;

    //  Get the default initial advanced state
    isAdvanced = applicationManager.getAdvanced();
    setAdvanced(isAdvanced);
    setToolTipText();

    //  Layout the buttons
    panelExitButtons.setLayout(
      new BoxLayout(panelExitButtons, BoxLayout.X_AXIS));

    panelExitButtons.add(Box.createHorizontalGlue());
    panelExitButtons.add(buttonCancel);
    panelExitButtons.add(Box.createHorizontalGlue());
    panelExitButtons.add(buttonPostpone);
    panelExitButtons.add(Box.createHorizontalGlue());
    panelExitButtons.add(buttonExecute);
    panelExitButtons.add(Box.createHorizontalGlue());
    panelExitButtons.add(buttonAdvanced);
    panelExitButtons.add(Box.createHorizontalGlue());

    //  Exit action listeners
    buttonCancel.addActionListener(new buttonCancelActionAdapter(this));
    buttonPostpone.addActionListener(new buttonPostponeActionAdapter(this));
    buttonExecute.addActionListener(new buttonExecuteActionAdapter(this));
    buttonAdvanced.addActionListener(new buttonAdvancedActionAdapter(this));
  }

  public Container getContainer() {
    return rootPanel;
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
    setAdvanced(!isAdvanced);
  }

  /**
   * Set the advanced state variable and update the button text
   */
  void setAdvanced(boolean state) {
    isAdvanced = state;
    if (isAdvanced) {
      buttonAdvanced.setText("Basic");
    }
    else {
      buttonAdvanced.setText("Advanced");
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
    buttonCancel.setToolTipText(line1 + line2);

    line1 = "<html>This button will save any changes to the parameters<br>";
    line2 = "in this dialog box and return you to the main window<br>";
    line3 = "without executing any of the processing.  Any parameter<br>";
    line4 = "changes will also be written to the com scripts.";
    buttonPostpone.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>This button will save any changes to the parameters<br>";
    line2 = "in this dialog box and execute the specified operation<br>";
    line3 = "on the data.  Any parameter changes will also be written<br>";
    line4 = "to the com scripts.";
    buttonExecute.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>This button will present a more detailed set of<br>";
    line2 = "options for each of the underlying processes.";
    buttonAdvanced.setToolTipText(line1 + line2);
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
