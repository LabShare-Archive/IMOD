package etomo.ui;

import java.awt.Component;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;

import etomo.ApplicationManager;
import etomo.process.ImodManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseScreenState;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EnumeratedType;
import etomo.type.EtomoNumber;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.util.DatasetFiles;
import etomo.util.EnvironmentVariable;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.RunraptorParam;
import etomo.comscript.TransferfidParam;

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
 * <p> Revision 3.54  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.53  2009/09/01 03:18:24  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.52  2009/06/15 20:21:10  sueh
 * <p> bug# 1221 Added getBeadTrackDisplay.
 * <p>
 * <p> Revision 3.51  2009/06/10 22:16:26  sueh
 * <p> bug# 1221 Factoring RAPTOR into RaptorPanel.
 * <p>
 * <p> Revision 3.50  2009/06/08 20:45:47  sueh
 * <p> bug# 1216 Fixed the code that chooses whether to display RAPTOR as an
 * <p> option.  Add code to chooses whether to display a choice of input files.
 * <p>
 * <p> Revision 3.49  2009/05/28 19:18:03  sueh
 * <p> bug# 1216 Checking for the RAPTOR bin directory and the RAPTOR_BIN
 * <p> environment variable.
 * <p>
 * <p> Revision 3.48  2009/05/22 21:28:47  sueh
 * <p> bug# 1216 Spaced raptor panel, added tooltips, improved button titles.
 * <p>
 * <p> Revision 3.47  2009/05/04 16:46:44  sueh
 * <p> bug# 1216 In FiducialModelDialog using AxisType to prevent the displaying
 * <p> of the RAPTOR interface unless the dataset is dual axis.  Make .preali the
 * <p> default file to use with RAPTOR.
 * <p>
 * <p> Revision 3.46  2009/05/02 01:26:07  sueh
 * <p> bug# 1216 For B axis, hiding the pick panel and not setting/getting raptor
 * <p> data from metadata.
 * <p>
 * <p> Revision 3.45  2009/05/02 01:13:28  sueh
 * <p> bug# 1216 Added the raptor panel.
 * <p>
 * <p> Revision 3.44  2009/01/20 20:00:49  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.43  2008/11/20 01:41:16  sueh
 * <p> bug# 1147 Changed ApplicationManager.imodSeedFiducials to
 * <p> imodSeedModel.
 * <p>
 * <p> Revision 3.42  2008/10/16 21:20:51  sueh
 * <p> bug# 1141 Removed fixRootPanel because it doesn't do anything.
 * <p>
 * <p> Revision 3.41  2008/05/13 23:01:50  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.40  2008/05/07 00:01:32  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.  Make transfer
 * <p> fid panel responsible for its own actions.
 * <p>
 * <p> Revision 3.39  2008/05/03 00:49:48  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.38  2007/12/26 22:23:54  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 3.37  2007/09/10 20:42:35  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.36  2007/07/27 16:54:50  sueh
 * <p> bug# 979 Moved "Fix Fiducial Model" to BeadtrackPanel.  Using getInstance to
 * <p> construct FiducialModelDialog because it uses action listeners, which shouldn't
 * <p> be created during construction.
 * <p>
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
public final class FiducialModelDialog extends ProcessDialog implements
    ContextMenu, Run3dmodButtonContainer, RaptorPanelParent {
  public static final String rcsid = "$Id$";

  private static final String SEEDING_NOT_DONE_LABEL = "Seed Fiducial Model";
  private static final String SEEDING_DONE_LABEL = "View Seed Model";

  private final SpacedPanel pnlFiducialModel = SpacedPanel.getInstance();
  private final FiducialModelActionListener actionListener = new FiducialModelActionListener(
      this);

  final Run3dmodButton btnSeed;
  private final BeadtrackPanel pnlBeadtrack;
  private final TransferfidPanel pnlTransferfid;

  private final JPanel pnlMethod = new JPanel();
  private final ButtonGroup bgMethod = new ButtonGroup();
  private final RadioButton rbMethodSeed = new RadioButton(
      "Make seed and track", MethodEnumeratedType.SEED, bgMethod);
  private final RadioButton rbMethodPatchTracking = new RadioButton(
      "Use patching tracking to make fiducial model",
      MethodEnumeratedType.PATCH_TRACKING, bgMethod);
  private final RadioButton rbMethodRaptor = new RadioButton(
      "Run RAPTOR and fix", MethodEnumeratedType.RAPTOR, bgMethod);
  private final TiltxcorrPanel tiltxcorrPanel;

  private final RaptorPanel raptorPanel;

  private boolean transferfidEnabled = false;

  private FiducialModelDialog(final ApplicationManager appMgr,
      final AxisID axisID, final AxisType axisType) {
    super(appMgr, axisID, DialogType.FIDUCIAL_MODEL);
    //initialize final member variables
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnSeed = (Run3dmodButton) displayFactory.getSeedFiducialModel();
    raptorPanel = RaptorPanel.getInstance(appMgr, axisID, dialogType, this);
    pnlBeadtrack = BeadtrackPanel.getInstance(appMgr, axisID, dialogType,
        btnAdvanced);
    tiltxcorrPanel = TiltxcorrPanel.getPatchTrackingInstance(appMgr, axisID,
        DialogType.FIDUCIAL_MODEL, btnAdvanced);
    //root panel
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    pnlFiducialModel.add(pnlMethod);
    rootPanel.add(pnlFiducialModel.getContainer());
    //fiducial model panel
    pnlFiducialModel.setBoxLayout(BoxLayout.Y_AXIS);
    pnlFiducialModel.setBorder(new BeveledBorder("Fiducial Model Generation")
        .getBorder());
    if (applicationManager.isDualAxis()) {
      pnlTransferfid = TransferfidPanel.getInstance(applicationManager, axisID,
          dialogType, this, btnAdvanced);
      pnlFiducialModel.add(pnlTransferfid.getContainer());
    }
    else {
      pnlTransferfid = null;
    }
    pnlFiducialModel.add(tiltxcorrPanel.getPanel());
    pnlFiducialModel.add(raptorPanel.getComponent());
    pnlFiducialModel.add(btnSeed.getComponent());
    pnlFiducialModel.add(pnlBeadtrack.getContainer());
    //transfer fiducials panel
    if (pnlTransferfid != null) {
      pnlTransferfid.setDeferred3dmodButtons();
    }
    //Method panel
    pnlMethod.setLayout(new BoxLayout(pnlMethod, BoxLayout.Y_AXIS));
    pnlMethod.setBorder(BorderFactory.createEtchedBorder());
    pnlMethod.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlMethod.add(rbMethodSeed.getComponent());
    pnlMethod.add(rbMethodPatchTracking.getComponent());
    pnlMethod.add(rbMethodRaptor.getComponent());
    //RAPTOR panel

    //exit button panel
    addExitButtons();
    //set initial values
    rbMethodSeed.setSelected(true);
    //seed button
    btnSeed.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnSeed.setSize();
    btnSeed.setContainer(this);
    //
    btnExecute.setText("Done");
    //tool tips
    setToolTipText();
    //set dialog display state
    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.SECOND) {
      turnOffRaptor();
    }
    else {
      File raptorBin = new File("/usr/local/RAPTOR/bin");
      //RAPTOR_BIN environment variable overrides the default location of RAPTOR.
      String envVar = "RAPTOR_BIN";
      String raptorBinEnvVar = EnvironmentVariable.INSTANCE.getValue(appMgr,
          appMgr.getPropertyUserDir(), envVar, axisID);
      if (raptorBinEnvVar != null && !raptorBinEnvVar.matches("\\s*")) {
        raptorBin = new File(raptorBinEnvVar);
      }
      if (!raptorBin.exists() || !raptorBin.isDirectory()) {
        System.err.println("WARNING:  " + raptorBin.getAbsolutePath()
            + " cannot be found.  The environment variable " + envVar
            + " may be incorrect.");
        turnOffRaptor();
      }
    }
    updateAdvanced();
    updateEnabled();
    updateDisplay();
  }

  private void turnOffRaptor() {
    rbMethodRaptor.setVisible(false);
    if (rbMethodRaptor.isSelected()) {
      rbMethodSeed.setSelected(true);
    }
    updatePick();
  }

  public static FiducialModelDialog getInstance(
      final ApplicationManager appMgr, final AxisID axisID,
      final AxisType axisType) {
    FiducialModelDialog instance = new FiducialModelDialog(appMgr, axisID,
        axisType);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    pnlFiducialModel.addMouseListener(new GenericMouseAdapter(this));
    rbMethodSeed.addActionListener(actionListener);
    rbMethodPatchTracking.addActionListener(actionListener);
    rbMethodRaptor.addActionListener(actionListener);
    btnSeed.addActionListener(actionListener);
  }

  public static ProcessResultDisplay getRaptorDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Run RAPTOR",
        DialogType.FIDUCIAL_MODEL);
  }

  public static ProcessResultDisplay getPatchTrackingButton() {
    return TiltxcorrPanel.getPatchTrackingButton(DialogType.FIDUCIAL_MODEL);
  }

  public static ProcessResultDisplay getUseRaptorDisplay() {
    return MultiLineButton.getToggleButtonInstance(
        "Use RAPTOR Result as Fiducial Model", DialogType.FIDUCIAL_MODEL);
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
    return Run3dmodButton.getToggle3dmodInstance(SEEDING_NOT_DONE_LABEL,
        DialogType.FIDUCIAL_MODEL);
  }

  public static ProcessResultDisplay getTrackFiducialsDisplay() {
    return BeadtrackPanel.getTrackFiducialsDisplay(DialogType.FIDUCIAL_MODEL);
  }

  public static ProcessResultDisplay getFixFiducialModelDisplay() {
    return Run3dmodButton.getToggle3dmodInstance("Fix Fiducial Model",
        DialogType.FIDUCIAL_MODEL);
  }

  /**
   * Set the advanced state for the dialog box
   */
  private void updateAdvanced() {
    pnlBeadtrack.updateAdvanced(isAdvanced());
    if (pnlTransferfid != null) {
      pnlTransferfid.updateAdvanced(isAdvanced());
    }
    tiltxcorrPanel.updateAdvanced(isAdvanced());
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public void updateEnabled() {
    if (pnlTransferfid != null) {
      pnlTransferfid.setEnabled(transferfidEnabled);
    }
  }

  /**
   * Set the parameters for the specified beadtrack panel
   */
  public void setBeadtrackParams(final/*Const*/BeadtrackParam beadtrackParams) {
    raptorPanel.setBeadtrackParams(beadtrackParams);
    pnlBeadtrack.setParameters(beadtrackParams);
  }

  public void setTransferFidParams() {
    if (pnlTransferfid != null) {
      pnlTransferfid.setParameters();
    }
  }

  public BeadTrackDisplay getBeadTrackDisplay() {
    return pnlBeadtrack;
  }

  public TiltXcorrDisplay getTiltxcorrDisplay() {
    return tiltxcorrPanel;
  }

  public void getParameters(final BaseScreenState screenState) {
    pnlBeadtrack.getParameters(screenState);
    if (pnlTransferfid != null) {
      pnlTransferfid.getParameters(screenState);
    }
  }

  public boolean getParameters(final RunraptorParam param) {
    return raptorPanel.getParameters(param);
  }

  public void getParameters(final MetaData metaData) {
    if (axisID != AxisID.SECOND) {
      raptorPanel.getParameters(metaData);
    }
    metaData.setTrackMethod(axisID, ((RadioButton.RadioButtonModel) bgMethod
        .getSelection()).getEnumeratedType().toString());
    tiltxcorrPanel.getParameters(metaData);
  }

  public void setParameters(final ConstMetaData metaData) {
    if (axisID != AxisID.SECOND) {
      MethodEnumeratedType method = MethodEnumeratedType.getInstance(metaData
          .getTrackMethod(axisID));
      if (method == MethodEnumeratedType.SEED) {
        rbMethodSeed.setSelected(true);
      }
      else if (method == MethodEnumeratedType.PATCH_TRACKING) {
        rbMethodPatchTracking.setSelected(true);
      }
      else if (method == MethodEnumeratedType.RAPTOR) {
        rbMethodRaptor.setSelected(true);
      }
      raptorPanel.setParameters(metaData);
    }
    tiltxcorrPanel.setParameters(metaData);
    updatePick();
  }

  public void setParameters(final ConstTiltxcorrParam tiltXcorrParams) {
    tiltxcorrPanel.setParameters(tiltXcorrParams);
  }

  public final void setParameters(final ReconScreenState screenState) {
    if (pnlTransferfid != null) {
      pnlTransferfid.setParameters(screenState);
    }
    //btnSeed.setButtonState(screenState.getButtonState(btnSeed
    //   .getButtonStateKey()));
    pnlBeadtrack.setParameters(screenState);
  }

  public void getTransferFidParams() {
    if (pnlTransferfid != null) {
      pnlTransferfid.getParameters();
    }
  }

  public void getTransferFidParams(final TransferfidParam transferFidParam) {
    if (pnlTransferfid != null) {
      pnlTransferfid.getParameters(transferFidParam);
    }
  }

  public void setTransferfidEnabled(final boolean fileExists) {
    transferfidEnabled = fileExists;
  }

  public boolean isPickVisible() {
    return pnlMethod.isVisible();
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String[] manPagelabel = { "Transferfid", "Beadtrack", "Tiltxcorr", "3dmod" };
    String[] manPage = { "transferfid.html", "beadtrack.html",
        "tiltxcorr.html", "3dmod.html" };

    String[] logFileLabel = { "Track", "Transferfid", "Xcorr_pt" };
    String[] logFile = new String[3];
    logFile[0] = "track" + axisID.getExtension() + ".log";
    logFile[1] = "transferfid.log";
    logFile[2] = "xcorr_pt" + axisID.getExtension() + ".log";

    //    ContextPopup contextPopup =
    new ContextPopup(pnlFiducialModel.getContainer(), mouseEvent,
        "GETTING FIDUCIAL", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    rbMethodSeed
        .setToolTipText("Create a seed model and use beadtracker to generate the "
            + "fiducial model.");
    rbMethodPatchTracking
        .setToolTipText("Create the fiducial model with patch tracking.");
    rbMethodRaptor.setToolTipText("Use RAPTOR to create the fiducial model.");
    btnSeed.setToolTipText("Open new or existing seed model in 3dmod.");
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  //  Action function for buttons
  private void buttonAction(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(rbMethodSeed.getActionCommand())
        || command.equals(rbMethodPatchTracking.getActionCommand())
        || command.equals(rbMethodRaptor.getActionCommand())) {
      updatePick();
    }
    else if (command.equals(btnSeed.getActionCommand())) {
      applicationManager.imodSeedModel(axisID, run3dmodMenuOptions, btnSeed,
          ImodManager.COARSE_ALIGNED_KEY, DatasetFiles.getSeedFileName(
              applicationManager, axisID), DatasetFiles.getRawTiltFile(
              applicationManager, axisID), dialogType);
    }
  }

  private void updatePick() {
    if (rbMethodPatchTracking.isSelected()) {
      tiltxcorrPanel.setVisible(true);
      raptorPanel.setVisible(false);
      btnSeed.setVisible(false);
      pnlBeadtrack.setVisible(false);
      if (pnlTransferfid != null) {
        pnlTransferfid.setVisible(false);
      }
    }
    else if (rbMethodRaptor.isSelected()) {
      raptorPanel.setVisible(true);
      tiltxcorrPanel.setVisible(false);
      btnSeed.setVisible(false);
      pnlBeadtrack.setVisible(true);
      pnlBeadtrack.pickRaptor();
      if (pnlTransferfid != null) {
        pnlTransferfid.setVisible(true);
      }
    }
    else {
      raptorPanel.setVisible(false);
      tiltxcorrPanel.setVisible(false);
      btnSeed.setVisible(true);
      pnlBeadtrack.setVisible(true);
      pnlBeadtrack.pickSeed();
      if (pnlTransferfid != null) {
        pnlTransferfid.setVisible(true);
      }
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  void done() {
    applicationManager.doneFiducialModelDialog(axisID);
    if (pnlTransferfid != null) {
      pnlTransferfid.done();
    }
    raptorPanel.done();
    btnSeed.removeActionListener(actionListener);
    pnlBeadtrack.done();
    tiltxcorrPanel.done();
    setDisplayed(false);
  }

  //
  //	Action listener adapters
  //
  private final class FiducialModelActionListener implements ActionListener {

    private final FiducialModelDialog adaptee;

    private FiducialModelActionListener(final FiducialModelDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null, null);
    }
  }

  public static final class MethodEnumeratedType implements EnumeratedType {
    private static final MethodEnumeratedType SEED = new MethodEnumeratedType(
        true, 0, "Seed");
    private static final MethodEnumeratedType PATCH_TRACKING = new MethodEnumeratedType(
        false, 1, "PatchTracking");
    public static final MethodEnumeratedType RAPTOR = new MethodEnumeratedType(
        false, 2, "Raptor");

    private final boolean isDefault;
    private final EtomoNumber value = new EtomoNumber();
    private final String string;

    private MethodEnumeratedType(final boolean isDefault, final int value,
        final String string) {
      this.isDefault = isDefault;
      this.value.set(value);
      this.string = string;
    }

    private static MethodEnumeratedType getInstance(final String string) {
      if (string == null) {
        return null;
      }
      if (string.equals(SEED.string)) {
        return SEED;
      }
      if (string.equals(PATCH_TRACKING.string)) {
        return PATCH_TRACKING;
      }
      if (string.equals(RAPTOR.string)) {
        return RAPTOR;
      }
      return null;
    }

    public boolean isDefault() {
      return isDefault;
    }

    public ConstEtomoNumber getValue() {
      return value;
    }

    public String toString() {
      return string;
    }
  }
}
