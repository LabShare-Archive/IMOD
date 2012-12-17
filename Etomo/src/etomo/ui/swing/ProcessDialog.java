/**
 * <p>Description:A generic process dialog box with a set of exit buttons
 * and the action adapter to handle their processing.  The action functions
 * can be overriden to implement the required functionality.</p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.37  2009/09/20 21:33:48  sueh
 * <p> bug# 1268 Added timestamp and dialog identification to log.
 * <p>
 * <p> Revision 3.36  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.35  2009/01/20 20:21:29  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.34  2008/10/16 22:31:14  sueh
 * <p> bug# 1141 Removed fixRootPanel because it doesn't do anything.
 * <p>
 * <p> Revision 3.33  2008/05/30 22:32:28  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 3.32  2008/05/30 21:33:41  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 3.31  2007/12/26 22:26:06  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 3.30  2007/09/07 00:28:01  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.29  2007/03/21 19:46:27  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.28  2007/03/07 21:12:21  sueh
 * <p> bug# 981 Reduce the visibility of members to package private because the
 * <p> inheritance of this class is in-package only.
 * <p>
 * <p> Revision 3.27  2007/03/03 01:22:22  sueh
 * <p> bug# 977 Stop formating the Cancel, Postpone, Execute, and Advanced buttons
 * <p> twice.
 * <p>
 * <p> Revision 3.26  2006/07/28 19:57:24  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing because isParallel is too similar to a standard get
 * <p> function.
 * <p>
 * <p> Revision 3.25  2006/07/20 17:21:06  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 3.24  2006/06/30 20:53:28  sueh
 * <p> Fixed timestamp label for saveAction.
 * <p>
 * <p> Revision 3.23  2006/06/30 20:03:10  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog done() functions,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.  Added displayed.
 * <p>
 * <p> Revision 3.22  2006/06/14 00:36:03  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.
 * <p>
 * <p> Revision 3.21  2006/06/09 19:50:48  sueh
 * <p> bug# 870 Added setExitState().
 * <p>
 * <p> Revision 3.20  2006/04/28 21:03:51  sueh
 * <p> bug# 787 Removed UITestConstants.  Moved constants to
 * <p> implementations of AdocCommand.
 * <p>
 * <p> Revision 3.19  2006/03/20 18:06:56  sueh
 * <p> bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
 * <p>
 * <p> Revision 3.18  2006/01/31 20:32:52  sueh
 * <p> bug# 521 Added overrideable function done() to do clean up before the
 * <p> reference to a dialog is set to null.  This is important for removing
 * <p> action listeners from buttons that are managed by
 * <p> ProcessResultDisplayFactory, sinces these buttons outlast the dialog.
 * <p>
 * <p> Revision 3.17  2006/01/12 17:18:42  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.16  2006/01/11 22:32:05  sueh
 * <p> bug# 675 fixed print names functionality
 * <p>
 * <p> Revision 3.15  2006/01/04 20:27:51  sueh
 * <p> bug# 675 Moved constants that must be shared by non-test objects to an
 * <p> object which doesn't know about junit.  Overwise junit would have to be in
 * <p> the path for compiling and running EtomoDirector.  Removed the second
 * <p> name/value pair because the panel doesn't have to be addressed directly.
 * <p>
 * <p> Revision 3.14  2006/01/03 23:44:41  sueh
 * <p> bug# 675 Getting the test section name from JfcUnitTests
 * <p>
 * <p> Revision 3.13  2005/12/29 18:16:55  sueh
 * <p> bug# 675 temporarily deleting classes using jfcunit
 * <p>
 * <p> Revision 3.12  2005/12/23 02:17:17  sueh
 * <p> bug# 675 Named the root panel so it can be found by JfcUnit.
 * <p>
 * <p> Revision 3.11  2005/10/15 00:36:21  sueh
 * <p> bug# 532 Implementing ParallelDialog.  IsParallel() always returns false.
 * <p>
 * <p> Revision 3.10  2005/08/09 20:28:38  sueh
 * <p> bug# 711  No longer inheriting JButton in MultiLineButton.
 * <p>
 * <p> Revision 3.9  2005/06/17 19:18:13  sueh
 * <p> bug# 685 Put all timestamp functionality into one function.  Added
 * <p> buttonTimestamp to provide an interface to the main timestamp function.
 * <p>
 * <p> Revision 3.8  2005/06/17 00:34:21  sueh
 * <p> bug# 685 Timestamped cancel, execute, and postpone button presses.
 * <p>
 * <p> Revision 3.7  2005/04/16 02:03:18  sueh
 * <p> bug# 615 Moved the adding of exit buttons from the child classes to this
 * <p> class.  Remove the rigid area at the bottom so the color would wrap the
 * <p> dialog correctly.
 * <p>
 * <p> Revision 3.6  2005/04/12 19:39:15  sueh
 * <p> bug# 615 Do not disable fit window menu option.
 * <p>
 * <p> Revision 3.5  2005/04/01 02:55:21  sueh
 * <p> bug# 622 newstuff:  Turned on fit menu command when dialog is created
 * <p> and when Advanced is pressed.
 * <p>
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
package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.ActionEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ParallelParam;
import etomo.type.AxisID;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.util.Utilities;

public abstract class ProcessDialog implements AbstractParallelDialog {
  public static final String rcsid = "$Id$";

  final ApplicationManager applicationManager;
  final AxisID axisID;
  final DialogType dialogType;
  final EtomoPanel rootPanel = new EtomoPanel();
  // Exit buttons
  final JPanel pnlExitButtons = new JPanel();
  final MultiLineButton btnCancel = new MultiLineButton("Cancel");
  final MultiLineButton btnPostpone = new MultiLineButton("Postpone");
  final MultiLineButton btnExecute = new MultiLineButton("Execute");
  final GlobalExpandButton btnAdvanced = GlobalExpandButton.getInstance("Advanced",
      "Basic");

  private DialogExitState exitState = DialogExitState.SAVE;
  private boolean displayed = false;

  abstract void done();

  /**
   * Create a new process dialog with a set of exit buttons (cancel, postpone
   * execute, and advanced) available for use.  The action adapters for the
   * buttons are already implemented.
   */
  public ProcessDialog(final ApplicationManager appManager, final AxisID axisID,
      DialogType dialogType) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: " + dialogType);
    displayed = true;
    applicationManager = appManager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    // Get the default initial advanced state - dialog must set themselves up
    // according to this state.
    btnAdvanced.changeState(appManager.isAdvanced(dialogType, axisID));
    setToolTipText();

    // Layout the buttons
    pnlExitButtons.setLayout(new BoxLayout(pnlExitButtons, BoxLayout.X_AXIS));
    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnCancel.getComponent());
    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnPostpone.getComponent());
    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnExecute.getComponent());
    pnlExitButtons.add(Box.createHorizontalGlue());
    pnlExitButtons.add(btnAdvanced.getComponent());
    pnlExitButtons.add(Box.createHorizontalGlue());

    UIUtilities.setButtonSizeAll(pnlExitButtons,
        UIParameters.INSTANCE.getNarrowButtonDimension());

    // Exit action listeners
    btnCancel.addActionListener(new buttonCancelActionAdapter(this));
    btnPostpone.addActionListener(new buttonPostponeActionAdapter(this));
    btnExecute.addActionListener(new buttonExecuteActionAdapter(this));
  }

  void setExitState(DialogExitState exitState) {
    this.exitState = exitState;
  }

  public Container getContainer() {
    return rootPanel;
  }

  public void getParameters(final ParallelParam param) {
  }

  public void addExitButtons() {
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlExitButtons);
  }

  public DialogType getDialogType() {
    return dialogType;
  }

  public boolean isAdvanced() {
    return btnAdvanced.isExpanded();
  }

  void setDisplayed(final boolean displayed) {
    this.displayed = displayed;
  }

  public boolean isDisplayed() {
    return displayed;
  }

  /**
   * Action to take when the cancel button is pressed, the default action is
   * to set the exitState attribute to CANCEL.
   */
  public void buttonCancelAction(final ActionEvent event) {
    Utilities.buttonTimestamp("cancel", dialogType.toString());
    exitState = DialogExitState.CANCEL;
    done();
  }

  /**
   * Action to take when the postpone button is pressed, the default action is
   * to set the exitState attribute to POSTPONE.
   */
  public void buttonPostponeAction(final ActionEvent event) {
    Utilities.buttonTimestamp("postpone", dialogType.toString());
    exitState = DialogExitState.POSTPONE;
    done();
  }

  /**
   * Action to take when the execute button is pressed, the default action is
   * to set the exitState attribute to EXECUTE.
   */
  public boolean buttonExecuteAction() {
    Utilities.buttonTimestamp("done", dialogType.toString());
    exitState = DialogExitState.EXECUTE;
    done();
    return true;
  }

  public void saveAction() {
    Utilities.timestamp("save", dialogType.toString());
    exitState = DialogExitState.SAVE;
    done();
  }

  public DialogExitState getExitState() {
    return exitState;
  }

  //
  // Default tool tip text for the buttons.
  //
  private void setToolTipText() {
    String line1, line2, line3, line4;

    line1 = "This button will abort any changes to the parameters ";
    line2 = "in this dialog box and return you to the main window.";
    btnCancel.setToolTipText(line1 + line2);

    line1 = "This button will save any changes to the parameters ";
    line2 = "in this dialog box and return you to the main window ";
    line3 = "without executing any of the processing.  Any parameter ";
    line4 = "changes will also be written to the com scripts.";
    btnPostpone.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "This button will save any changes to the parameters ";
    line2 = "in this dialog box and execute the specified operation ";
    line3 = "on the data.  Any parameter changes will also be written ";
    line4 = "to the com scripts.";
    btnExecute.setToolTipText(line1 + line2 + line3 + line4);

    line1 = "This button will present a more detailed set of ";
    line2 = "options for each of the underlying processes.";
    btnAdvanced.setToolTipText(line1 + line2);
  }
}

/**
 *  Action adapters to bind the buttons their action functions
 */

final class buttonCancelActionAdapter implements java.awt.event.ActionListener {

  ProcessDialog adaptee;

  buttonCancelActionAdapter(final ProcessDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(final ActionEvent e) {
    adaptee.buttonCancelAction(e);
  }
}

final class buttonPostponeActionAdapter implements java.awt.event.ActionListener {

  ProcessDialog adaptee;

  buttonPostponeActionAdapter(ProcessDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(final ActionEvent e) {
    adaptee.buttonPostponeAction(e);
  }
}

final class buttonExecuteActionAdapter implements java.awt.event.ActionListener {

  ProcessDialog adaptee;

  buttonExecuteActionAdapter(final ProcessDialog adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent e) {
    adaptee.buttonExecuteAction();
  }
}