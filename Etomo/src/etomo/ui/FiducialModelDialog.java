package etomo.ui;

import java.awt.Component;
import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.DialogType;
import etomo.type.InvalidEtomoNumberException;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.TransferfidParam;
import etomo.comscript.FortranInputSyntaxException;

/**
 * <p>Description: The dialog box for creating the fiducial model(s).</p>
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
 * <p> Revision 3.35  2007/02/09 00:48:55  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.34  2006/07/31 16:36:32  sueh
 * <p> bug# 902 changed "View Fiducial Model" label to "View Seed Model"
 * <p>
 * <p> Revision 3.33  2006/07/19 20:12:47  sueh
 * <p> bug# 902 Added updateDisplay() to change the label of btnSeed when seeding is
 * <p> done.
 * <p>
 * <p> Revision 3.32  2006/07/05 23:26:02  sueh
 * <p> Get fine alignment fix fiducials to set the right mode.
 * <p>
 * <p> Revision 3.31  2006/07/04 20:41:42  sueh
 * <p> bug# 898 Don't remove action listeners unless the done dialog function
 * <p> succeeds.
 * <p>
 * <p> Revision 3.30  2006/07/04 18:47:37  sueh
 * <p> bug# 893 Calling updateAdvanced(boolean) in panels to change the
 * <p> headers when the advanced button is pressed.
 * <p>
 * <p> Revision 3.29  2006/07/04 05:18:49  mast
 * <p> Fix typo transferid in popup menu
 * <p>
 * <p> Revision 3.28  2006/06/30 20:01:47  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.
 * <p>
 * <p> Revision 3.27  2006/06/28 18:44:50  sueh
 * <p> bug# 889 done():  Calling beadtrack done.
 * <p>
 * <p> Revision 3.26  2006/06/27 23:12:16  sueh
 * <p> bug# 887 getParameters(BaseScreenState):  fixed null pointer exception.
 * <p>
 * <p> Revision 3.25  2006/06/21 15:52:51  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.24  2006/06/16 15:25:10  sueh
 * <p> bug# 734 Moved track and use buttons from fiducial model dialog to beadtracker
 * <p> dialog.
 * <p>
 * <p> Revision 3.23  2006/06/07 22:25:47  sueh
 * <p> bug# 766 ApplicationManager.imodFixFiducials():  turning off auto center when
 * <p> fix fiducials is first run.
 * <p>
 * <p> Revision 3.22  2006/05/23 21:07:28  sueh
 * <p> bug# 617 Sharing button label text, so it can be used in messages.
 * <p>
 * <p> Revision 3.21  2006/03/30 21:25:38  sueh
 * <p> bug# 809 Do fix fiducials with auto center on.
 * <p>
 * <p> Revision 3.20  2006/02/06 21:20:54  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.19  2006/01/26 22:04:33  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.18  2005/11/14 22:03:06  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 3.17  2005/08/30 19:05:28  sueh
 * <p> bug# 718 fixed a null pointer bug that happended when
 * <p> btnTransferFiducials is not set.
 * <p>
 * <p> Revision 3.16  2005/08/11 23:50:31  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 3.15  2005/08/10 20:42:49  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.14  2005/08/09 20:22:13  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * <p> MultiLineButton from JButton.
 * <p>
 * <p> Revision 3.13  2005/08/04 20:09:47  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.12  2005/07/29 00:54:05  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.11  2005/05/10 03:26:54  sueh
 * <p> bug# 658 Using BeadtrackParam in place of ConstBeadtrackParam in
 * <p> setBeadtrackParams().  Throwing InvalidEtomoNumberException in
 * <p> getBeadtrackParams().
 * <p>
 * <p> Revision 3.10  2005/04/21 20:33:55  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.9  2005/04/16 01:55:06  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.8  2005/01/21 23:43:06  sueh
 * <p> bug# 509 bug# 591  Passing axisID to TransferfidPanel contructor so it
 * <p> can set a value for center view when the .rawtlt file is not in use.
 * <p>
 * <p> Revision 3.7  2005/01/14 03:07:26  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.6  2004/12/02 20:39:29  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.5  2004/11/19 23:53:35  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.4.4.1  2004/10/11 02:12:56  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.4  2004/05/05 21:21:40  sueh
 * <p> bug# 430 moving Use fid as seed button
 * <p>
 * <p> Revision 3.3  2004/03/15 23:13:16  sueh
 * <p> progress button names changed to "btn"
 * <p>
 * <p> Revision 3.2  2004/03/15 20:23:06  sueh
 * <p> bug# 276 Moved Use Model as Seed to be next to the Seed button.  Placed Use
 * <p> Model as Seed in Advanced.
 * <p>
 * <p> Revision 3.1  2004/02/16 18:52:01  sueh
 * <p> bug# 276 Added Use Fiducial Model as Seed button with
 * <p> action = call makeFiducialModelSeedModel() and untoggle
 * <p> Track button.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.11  2003/10/30 21:09:41  rickg
 * <p> Bug# 340 Added context menu entry for transferfid man page
 * <p> JToggleButton -> MultilineToggleButton
 * <p>
 * <p> Revision 2.10  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.9  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.8  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.7  2003/10/15 01:34:20  sueh
 * <p> Bug277 added tooltips
 * <p>
 * <p> Revision 2.6  2003/10/10 23:17:01  sueh
 * <p> bug251 removing marks
 * <p>
 * <p> Revision 2.5  2003/10/09 22:49:42  sueh
 * <p> bug251 fixed some null reference problems with transferfid
 * <p> panel in single axis mode
 * <p>
 * <p> Revision 2.4  2003/10/07 22:43:13  sueh
 * <p> bug251 moved transferfid from fine alignment dialog
 * <p> to fiducial model dialog
 * <p>
 * <p> Revision 2.3  2003/05/19 04:31:36  rickg
 * <p> Toggle button for Fix Model
 * <p>
 * <p> Revision 2.2  2003/05/07 17:50:37  rickg
 * <p> Added beadtrack and track.log to context menu
 * <p>
 * <p> Revision 2.1  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.7.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.7  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.6  2002/12/19 06:02:57  rickg
 * <p> Implementing advanced parameters handling
 * <p>
 * <p> Revision 1.5  2002/12/19 00:30:05  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/10/17 22:39:42  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class FiducialModelDialog extends ProcessDialog implements ContextMenu,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private static final String SEEDING_NOT_DONE_LABEL = "Seed Fiducial Model";
  private static final String SEEDING_DONE_LABEL = "View Seed Model";

  private JPanel pnlFiducialModel = new JPanel();

  private BeveledBorder border = new BeveledBorder("Fiducial Model Generation");
  private MultiLineButton btnTransferFiducials = null;
  private final Run3dmodButton btnSeed;

  private TransferfidPanel pnlTransferfid = null;
  private BeadtrackPanel pnlBeadtrack;

  private boolean transferfidEnabled = false;
  private final FiducialModelActionListener actionListener;

  private FiducialModelDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID, DialogType.FIDUCIAL_MODEL);
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnSeed = (Run3dmodButton) displayFactory.getSeedFiducialModel();
    btnSeed.setRun3dmodButtonContainer(this);
    setToolTipText();
    fixRootPanel(rootSize);
    pnlBeadtrack = BeadtrackPanel.getInstance(appMgr, axisID, dialogType);

    btnExecute.setText("Done");

    btnSeed.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnSeed.setSize();

    pnlFiducialModel
        .setLayout(new BoxLayout(pnlFiducialModel, BoxLayout.Y_AXIS));

    pnlFiducialModel.setBorder(border.getBorder());

    if (applicationManager.isDualAxis()) {
      pnlTransferfid = new TransferfidPanel(applicationManager, axisID, true,
          dialogType);
      pnlFiducialModel.add(pnlTransferfid.getContainer());
      pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));
    }
    pnlFiducialModel.add(btnSeed.getComponent());
    pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlFiducialModel.add(pnlBeadtrack.getContainer());
    pnlFiducialModel.add(Box.createRigidArea(FixedDim.x0_y5));
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(pnlFiducialModel);
    addExitButtons();

    //  Set the advanced state to the default
    updateAdvanced(isAdvanced);
    updateEnabled();
    updateDisplay();
    actionListener = new FiducialModelActionListener(this);
  }

  public static FiducialModelDialog getInstance(ApplicationManager appMgr,
      AxisID axisID) {
    FiducialModelDialog instance = new FiducialModelDialog(appMgr, axisID);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    //
    //  Action listener assignments for the buttons
    //
    btnSeed.addActionListener(actionListener);

    if (applicationManager.isDualAxis()) {
      btnTransferFiducials = pnlTransferfid.getButton();
      if (btnTransferFiducials != null) {
        btnTransferFiducials.addActionListener(actionListener);
      }
    }

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlFiducialModel.addMouseListener(mouseAdapter);
  }

  public void updateDisplay() {
    if (applicationManager.getState().isSeedingDone(axisID)) {
      btnSeed.setText(SEEDING_DONE_LABEL);
    }
    else {
      btnSeed.setText(SEEDING_NOT_DONE_LABEL);
    }
  }

  public static ProcessResultDisplay getTransferFiducialsDisplay() {
    return TransferfidPanel
        .getTransferFiducialsDisplay(DialogType.FIDUCIAL_MODEL);
  }

  public static ProcessResultDisplay getSeedFiducialModelDisplay() {
    return Run3dmodButton.getToggle3dmodButtonInstance(SEEDING_NOT_DONE_LABEL,
        DialogType.FIDUCIAL_MODEL);
  }

  public static ProcessResultDisplay getTrackFiducialsDisplay() {
    return BeadtrackPanel.getTrackFiducialsDisplay(DialogType.FIDUCIAL_MODEL);
  }

  public static ProcessResultDisplay getFixFiducialModelDisplay() {
    return Run3dmodButton.getToggle3dmodButtonInstance("Fix Fiducial Model",
        DialogType.FIDUCIAL_MODEL);
  }

  /**
   * Set the advanced state for the dialog box
   */
  public void updateAdvanced(boolean state) {
    pnlBeadtrack.updateAdvanced(state);
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.updateAdvanced(state);
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public void updateEnabled() {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.setEnabled(transferfidEnabled);
    }
  }

  /**
   * Set the parameters for the specified beadtrack panel
   */
  public void setBeadtrackParams(/*Const*/BeadtrackParam beadtrackParams) {
    pnlBeadtrack.setParameters(beadtrackParams);
  }

  public void setTransferFidParams() {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.setParameters();
    }
  }

  /**
   * Get the parameters for the specified beadtrack command
   */
  public void getBeadtrackParams(BeadtrackParam beadtrackParams)
      throws FortranInputSyntaxException, InvalidEtomoNumberException {
    pnlBeadtrack.getParameters(beadtrackParams);
  }

  public void getParameters(BaseScreenState screenState) {
    pnlBeadtrack.getParameters(screenState);
    if (pnlTransferfid != null) {
      pnlTransferfid.getParameters(screenState);
    }
  }

  public final void setParameters(ReconScreenState screenState) {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.setParameters(screenState);
    }
    btnSeed.setButtonState(screenState.getButtonState(btnSeed
        .getButtonStateKey()));
    pnlBeadtrack.setParameters(screenState);
  }

  public void getTransferFidParams() {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.getParameters();
    }
  }

  public void getTransferFidParams(TransferfidParam transferFidParam) {
    if (applicationManager.isDualAxis()) {
      pnlTransferfid.getParameters(transferFidParam);
    }
  }

  public void setTransferfidEnabled(boolean fileExists) {
    transferfidEnabled = fileExists;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Transferfid", "Beadtrack", "3dmod" };
    String[] manPage = { "transferfid.html", "beadtrack.html", "3dmod.html" };

    String[] logFileLabel = { "Track", "Transferfid" };
    String[] logFile = new String[2];
    logFile[0] = "track" + axisID.getExtension() + ".log";
    logFile[1] = "transferfid.log";

    //    ContextPopup contextPopup =
    new ContextPopup(pnlFiducialModel, mouseEvent, "GETTING FIDUCIAL",
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile,
        applicationManager, axisID);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    btnSeed.setToolTipText("Open new or existing seed model in 3dmod.");
  }

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }

  private void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnSeed.getActionCommand())) {
      applicationManager.imodSeedFiducials(axisID, menuOptions, btnSeed);
    }
  }

  //  Action function for buttons
  protected void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (btnTransferFiducials != null
        && command.equals(btnTransferFiducials.getActionCommand())) {
      applicationManager.transferfid(axisID, btnTransferFiducials);
    }
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  protected void done() {
    if (applicationManager.doneFiducialModelDialog(axisID)) {
      if (btnTransferFiducials != null) {
        btnTransferFiducials.removeActionListener(actionListener);
      }
      btnSeed.removeActionListener(actionListener);
      pnlBeadtrack.done();
      setDisplayed(false);
    }
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced(isAdvanced);
  }

  //
  //	Action listener adapters
  //
  class FiducialModelActionListener implements ActionListener {

    FiducialModelDialog adaptee;

    FiducialModelActionListener(FiducialModelDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }
}
