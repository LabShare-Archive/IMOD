package etomo.ui;

import java.awt.event.*;
import javax.swing.*;

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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ProcessDialog extends JDialog implements ExitButtons {
  public static final String rcsid =
    "$Id$";

  protected boolean isAdvanced = false;
  protected DialogExitState exitState = DialogExitState.CANCEL;

  //
  //  Exit buttons
  //
  protected JPanel panelExitButtons = new JPanel();
  protected JButton buttonCancel = new JButton("Cancel");
  protected JButton buttonPostpone = new JButton("Postpone");
  protected JButton buttonExecute = new JButton("Execute");
  protected JToggleButton buttonAdvanced = new JToggleButton("Advanced");

  /**
   * Create a new process dialog with a set of exit buttons (cancel, postpone
   * execute, and advanced) available for use.  The action adapters for the
   * buttons are already implemented.
   */
  public ProcessDialog() {
    setToolTipText();

    //
    //  Layout the buttons
    //
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

    //
    //  Exit action listeners
    //
    buttonCancel.addActionListener(new buttonCancelActionAdapter(this));
    buttonPostpone.addActionListener(new buttonPostponeActionAdapter(this));
    buttonExecute.addActionListener(new buttonExecuteActionAdapter(this));
    buttonAdvanced.addActionListener(new buttonAdvancedActionAdapter(this));
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
    if (isAdvanced) {
      isAdvanced = false;
      buttonAdvanced.setSelected(false);
    }
    else {
      isAdvanced = true;
      buttonAdvanced.setSelected(true);
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
    line3 = "without exectuing any of the processing.  Any parameter<br>";
    line4 = "changes will also be written in to the com scripts.";
    buttonPostpone.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "<html>This button will save any changes to the parameters<br>";
    line2 = "in this dialog box and execute the specified operation<br>";
    line3 = "on the data.  Any parameter changes will also be written<br>";
    line4 = "in to the com scripts.";
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
