package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.*;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.logic.ClusteredPointsAllowed;
import etomo.logic.TrackingMethod;
import etomo.process.ImodManager;
import etomo.storage.AutofidseedInitFileFilter;
import etomo.storage.AutofidseedSelectionAndSorting;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseScreenState;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EnumeratedType;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.DatasetFiles;
import etomo.comscript.AutofidseedParam;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.ImodchopcontsParam;
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
 * <p> Revision 1.4  2011/06/24 17:02:45  sueh
 * <p> Bug# 1466 Removed check for the existance of RAPTOR.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:09:04  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:03:16  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.61  2010/10/11 20:38:04  sueh
 * <p> bug# 1379 Removed patch tracking and raptor items from pop menu.
 * <p>
 * <p> Revision 3.60  2010/07/19 04:31:57  sueh
 * <p> bug# 1393 In setParameters(ConstMetaData) start setting the tracking type
 * <p> radio buttons for the second axis.
 * <p>
 * <p> Revision 3.59  2010/05/27 23:15:15  sueh
 * <p> bug# 1360 In constructor don't warn about missing RAPTOR unless RAPTOR_BIN has been
 * <p> set.
 * <p>
 * <p> Revision 3.58  2010/05/21 21:01:16  sueh
 * <p> bug# 1366 In updatePick making transfer fid invisible when raptor is in use.
 * <p>
 * <p> Revision 3.57  2010/03/12 04:13:50  sueh
 * <p> bug# 1325 Made the use raptor result button label available to the package.
 * <p>
 * <p> Revision 3.56  2010/03/08 21:03:11  sueh
 * <p> bug# 1311 Fixed typo.
 * <p>
 * <p> Revision 3.55  2010/03/03 05:04:20  sueh
 * <p> bug# 1311 Added TiltxcorrPanel and radio button for it.
 * <p>
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
public final class FiducialModelDialog extends ProcessDialog implements ContextMenu,
    Run3dmodButtonContainer, Expandable {
  public static final String rcsid = "$Id$";

  static final String SEEDING_NOT_DONE_LABEL = "Seed Fiducial Model";
  private static final String SEEDING_DONE_LABEL = "View Seed Model";
  private static final DialogType DIALOG_TYPE = DialogType.FIDUCIAL_MODEL;
  static final String AUTOFIDSEED_NEW_MODEL_LABEL = "Generate Seed Model";
  private static final String AUTOFIDSEED_APPEND_LABEL = "Add Points to Seed Model";
  private static final String AUTOFIDSEED_NEW_MODEL_TITLE = "Generate seed model automatically";
  private static final String AUTOFIDSEED_APPEND_TITLE = "Add points to seed model automatically";

  private final JPanel pnlMain = new JPanel();
  private final FiducialModelActionListener actionListener = new FiducialModelActionListener(
      this);
  private final ButtonGroup bgMethod = new ButtonGroup();
  private final RadioButton rbMethodSeed = new RadioButton("Make seed and track",
      TrackingMethod.SEED, bgMethod);
  private final RadioButton rbMethodPatchTracking = new RadioButton(
      "Use patch tracking to make fiducial model", TrackingMethod.PATCH_TRACKING,
      bgMethod);
  private final RadioButton rbMethodRaptor = new RadioButton("Run RAPTOR and fix",
      TrackingMethod.RAPTOR, bgMethod);
  private final JPanel[] pnlMethodArray = new JPanel[TrackingMethod.NUM];
  private TabbedPane tpSeedAndTrack = new TabbedPane();
  private final JPanel[] pnlSeedAndTrackArray = new JPanel[SeedAndTrackTab.NUM_TABS];
  private final JPanel[] pnlSeedAndTrackBodyArray = new JPanel[SeedAndTrackTab.NUM_TABS];
  private TabbedPane tpRunRaptor = new TabbedPane();
  private final JPanel[] pnlRunRaptorArray = new JPanel[RunRaptorTab.NUM_TABS];
  private final JPanel[] pnlRunRaptorBodyArray = new JPanel[RunRaptorTab.NUM_TABS];
  private final ButtonGroup bgSeedModel = new ButtonGroup();
  private final RadioButton rbSeedModelManual = new RadioButton(
      "Make seed model manually", SeedModelEnumeratedType.MANUAL, bgSeedModel);
  private final RadioButton rbSeedModelAuto = new RadioButton(
      AUTOFIDSEED_NEW_MODEL_TITLE, SeedModelEnumeratedType.AUTO, bgSeedModel);
  private final RadioButton rbSeedModelTransfer = new RadioButton(
      "Transfer seed model from the other axis", SeedModelEnumeratedType.TRANSFER,
      bgSeedModel);
  private final JPanel[] pnlSeedModelArray = new JPanel[SeedModelEnumeratedType.NUM];
  private final JPanel pnlAutoSeedModel = new JPanel();
  private final JPanel pnlAutofidseed = new JPanel();
  private final CheckBox cbBoundaryModel = new CheckBox("Use boundary model");
  private final Run3dmodButton btnBoundaryModel = Run3dmodButton.get3dmodInstance(
      "Create/Edit Boundary Model", this);
  private final CheckBox cbExcludeInsideAreas = new CheckBox(
      "Exclude inside boundary contours");
  private final LabeledTextField ltfBordersInXandY = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Borders in X & Y: ");
  private final LabeledTextField ltfMinGuessNumBeads = new LabeledTextField(
      FieldType.INTEGER, "Estimated number of beads in sample: ");
  private final LabeledTextField ltfMinSpacing = new LabeledTextField(
      FieldType.FLOATING_POINT, "Minimum spacing: ");
  private final LabeledTextField ltfPeakStorageFraction = new LabeledTextField(
      FieldType.FLOATING_POINT, "Fraction of peaks to store: ");
  private final ButtonGroup bgTarget = new ButtonGroup();
  private final RadioTextField rtfTargetNumberOfBeads = RadioTextField.getInstance(
      FieldType.INTEGER, "Total number:", bgTarget);
  private final RadioTextField rtfTargetDensityOfBeads = RadioTextField.getInstance(
      FieldType.FLOATING_POINT, "Density (per megapixel):", bgTarget);
  private final CheckBox cbTwoSurfaces = new CheckBox("Select beads on two surfaces");
  private final CheckBox cbAppendToSeedModel = new CheckBox("Add beads to existing model");
  private final LabeledTextField ltfIgnoreSurfaceData = new LabeledTextField(
      FieldType.INTEGER_LIST, "Ignore sorting in tracked models: ");
  private final LabeledTextField ltfDropTracks = new LabeledTextField(
      FieldType.INTEGER_LIST, "Drop tracked models: ");
  private final LabeledTextField ltfMaxMajorToMinorRatio = new LabeledTextField(
      FieldType.FLOATING_POINT, "Maximum ratio between surfaces: ");
  private final CheckBox cbClusteredPointsAllowedClustered = new CheckBox(
      "Allow clustered beads");
  private final CheckBoxSpinner cbsElongatedPointsAllowed = CheckBoxSpinner.getInstance(
      "Allow elongated beads of severity: ", 1, 1, 3);
  private final Run3dmodButton btn3dmodAutofidseed = Run3dmodButton.get3dmodInstance(
      "Open Seed Model", this);
  private final Run3dmodButton btn3dmodInitialBeadFinding = Run3dmodButton
      .get3dmodInstance("Open Initial Bead Model", this);
  private final Run3dmodButton btn3dmodBeadSelectionAndSorting = Run3dmodButton
      .get3dmodInstance("Open Sorted 3D Models", this);
  private final Run3dmodButton btn3dmodClusteredElongatedModel = Run3dmodButton
      .get3dmodInstance("Open Clustered / Elongated Model", this);
  private final MultiLineButton btnCleanup = new MultiLineButton(
      "Clean Up Temporary Files");
  private final CheckBox cbAdjustSizes = new CheckBox("Find and adjust bead size");

  private final Run3dmodButton btnSeed;
  private final BeadtrackPanel pnlBeadtrack;
  private final TransferfidPanel pnlTransferfid;
  private final TiltxcorrPanel tiltxcorrPanel;
  private final RaptorPanel raptorPanel;
  private final AxisType axisType;
  private final Run3dmodButton btnAutofidseed;

  private boolean transferfidEnabled = false;
  private int curMethodIndex = -1;
  private SeedAndTrackTab curSeedAndTrackTab = null;
  private int curSeedModelIndex = -1;
  private RunRaptorTab curRunRaptorTab = null;

  private FiducialModelDialog(final ApplicationManager appMgr, final AxisID axisID,
      final AxisType axisType) {
    super(appMgr, axisID, DIALOG_TYPE);
    this.axisType = axisType;
    ProcessResultDisplayFactory factory = appMgr.getProcessResultDisplayFactory(axisID);
    btnSeed = (Run3dmodButton) factory.getSeedFiducialModel();
    btnAutofidseed = (Run3dmodButton) factory.getAutofidseed();
    if (axisType != AxisType.DUAL_AXIS || axisID != AxisID.SECOND) {
      raptorPanel = RaptorPanel.getInstance(appMgr, axisID, dialogType);
    }
    else {
      raptorPanel = null;
    }
    pnlBeadtrack = BeadtrackPanel.getInstance(appMgr, axisID, dialogType, btnAdvanced);
    tiltxcorrPanel = TiltxcorrPanel.getPatchTrackingInstance(appMgr, axisID, DIALOG_TYPE,
        btnAdvanced);
    if (applicationManager.isDualAxis()) {
      pnlTransferfid = TransferfidPanel.getInstance(applicationManager, axisID,
          dialogType, btnAdvanced);
    }
    else {
      pnlTransferfid = null;
    }
  }

  public static FiducialModelDialog getInstance(final ApplicationManager appMgr,
      final AxisID axisID, final AxisType axisType) {
    FiducialModelDialog instance = new FiducialModelDialog(appMgr, axisID, axisType);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // Local panels
    JPanel pnlMethod = new JPanel();
    JPanel pnlMethodX = new JPanel();
    JPanel pnlSeedModel = new JPanel();
    JPanel pnlSeedModelX = new JPanel();
    JPanel pnlAutofidseedParam = new JPanel();
    JPanel pnlInitialBeadFinding = new JPanel();
    JPanel pnlBoundaryModel = new JPanel();
    JPanel pnl3dmodInitialBeadFinding = new JPanel();
    JPanel pnlBeadSearchingAndSorting = new JPanel();
    JPanel pnlExcludeInsideAreas = new JPanel();
    JPanel pnlTarget = new JPanel();
    JPanel pnlTwoSurfaces = new JPanel();
    JPanel pnlAppendToSeedModel = new JPanel();
    JPanel pnlClusteredPointsAllowed1 = new JPanel();
    JPanel pnl3dmodBeadSortingAndSearching = new JPanel();
    JPanel pnlButtons = new JPanel();
    JPanel pnl3dmodClusteredElongatedModel = new JPanel();
    JPanel pnlAdjustSizes = new JPanel();
    // Init
    btnSeed.setSize();
    btnBoundaryModel.setSize();
    btnAutofidseed.setDeferred3dmodButton(btn3dmodAutofidseed);
    btnAutofidseed.setSize();
    btn3dmodAutofidseed.setSize();
    btn3dmodInitialBeadFinding.setSize();
    btn3dmodInitialBeadFinding.setEnabled(false);
    btn3dmodBeadSelectionAndSorting.setSize();
    btn3dmodBeadSelectionAndSorting.setEnabled(false);
    ltfIgnoreSurfaceData.setEnabled(false);
    btn3dmodClusteredElongatedModel.setSize();
    btn3dmodClusteredElongatedModel.setEnabled(false);
    btnCleanup.setSize();
    btnExecute.setText("Done");
    rtfTargetNumberOfBeads.setSelected(true);
    rtfTargetNumberOfBeads.setRequired(true);
    rtfTargetDensityOfBeads.setRequired(true);
    // Root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("Fiducial Model Generation").getBorder());
    rootPanel.add(pnlMain);
    addExitButtons();
    // Main
    pnlMain.setLayout(new BoxLayout(pnlMain, BoxLayout.Y_AXIS));
    pnlMain.add(pnlMethodX);
    // Method
    // center the radio button panel
    pnlMethodX.setLayout(new BoxLayout(pnlMethodX, BoxLayout.X_AXIS));
    pnlMethodX.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlMethodX.add(Box.createHorizontalGlue());
    pnlMethodX.add(pnlMethod);
    pnlMethodX.add(Box.createHorizontalGlue());
    // radio button panel
    pnlMethod.setLayout(new BoxLayout(pnlMethod, BoxLayout.Y_AXIS));
    pnlMethod.setBorder(BorderFactory.createEtchedBorder());
    pnlMethod.add(rbMethodSeed.getComponent());
    pnlMethod.add(rbMethodPatchTracking.getComponent());
    if (raptorPanel != null) {
      pnlMethod.add(rbMethodRaptor.getComponent());
    }
    // panels to switch when the radio buttons change
    for (int i = 0; i < TrackingMethod.NUM; i++) {
      pnlMethodArray[i] = new JPanel();
    }
    int i = TrackingMethod.SEED.getValue().getInt();
    pnlMethodArray[i].add(tpSeedAndTrack);
    i = TrackingMethod.PATCH_TRACKING.getValue().getInt();
    pnlMethodArray[i].add(tiltxcorrPanel.getPanel());
    i = TrackingMethod.RAPTOR.getValue().getInt();
    pnlMethodArray[i].add(tpRunRaptor);
    // Seed and track
    // panels to switch when the tab changes
    for (i = 0; i < SeedAndTrackTab.NUM_TABS; i++) {
      pnlSeedAndTrackArray[i] = new JPanel();
      tpSeedAndTrack
          .addTab(SeedAndTrackTab.getInstance(i).title, pnlSeedAndTrackArray[i]);
      pnlSeedAndTrackBodyArray[i] = new JPanel();
      pnlSeedAndTrackBodyArray[i].setLayout(new BoxLayout(pnlSeedAndTrackBodyArray[i],
          BoxLayout.Y_AXIS));
    }
    i = SeedAndTrackTab.SEED.index;
    pnlSeedAndTrackBodyArray[i].add(pnlSeedModelX);
    // Run raptor
    // panels to switch when the tab changes
    for (i = 0; i < RunRaptorTab.NUM_TABS; i++) {
      pnlRunRaptorArray[i] = new JPanel();
      tpRunRaptor.addTab(RunRaptorTab.getInstance(i).title, pnlRunRaptorArray[i]);
      pnlRunRaptorBodyArray[i] = new JPanel();
    }
    i = RunRaptorTab.RAPTOR.index;
    if (raptorPanel != null) {
      pnlRunRaptorBodyArray[i].add(raptorPanel.getComponent());
    }
    // Seed model
    // center the radio button panel
    pnlSeedModelX.setLayout(new BoxLayout(pnlSeedModelX, BoxLayout.X_AXIS));
    pnlSeedModelX.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlSeedModelX.add(Box.createHorizontalGlue());
    pnlSeedModelX.add(pnlSeedModel);
    pnlSeedModelX.add(Box.createHorizontalGlue());
    // radio button panel
    pnlSeedModel.setLayout(new BoxLayout(pnlSeedModel, BoxLayout.Y_AXIS));
    pnlSeedModel.setBorder(BorderFactory.createEtchedBorder());
    pnlSeedModel.add(rbSeedModelManual.getComponent());
    pnlSeedModel.add(rbSeedModelAuto.getComponent());
    if (pnlTransferfid != null) {
      pnlSeedModel.add(rbSeedModelTransfer.getComponent());
    }
    // panels to switch when the radio buttons change
    for (i = 0; i < SeedModelEnumeratedType.NUM; i++) {
      pnlSeedModelArray[i] = new JPanel();
    }
    i = SeedModelEnumeratedType.MANUAL.value.getInt();
    pnlSeedModelArray[i].add(btnSeed.getComponent());
    i = SeedModelEnumeratedType.AUTO.value.getInt();
    pnlSeedModelArray[i].add(pnlAutoSeedModel);
    i = SeedModelEnumeratedType.TRANSFER.value.getInt();
    if (pnlTransferfid != null) {
      pnlSeedModelArray[i].add(pnlTransferfid.getContainer());
    }
    // Auto seed model
    pnlAutoSeedModel.setLayout(new BoxLayout(pnlAutoSeedModel, BoxLayout.Y_AXIS));
    // Autofidseed - goes on the AutoSeedModel panel under the shared beadtrack panel
    pnlAutofidseed.setLayout(new BoxLayout(pnlAutofidseed, BoxLayout.Y_AXIS));
    pnlAutofidseed.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAutofidseed.add(pnlAutofidseedParam);
    pnlAutofidseed.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAutofidseed.add(pnlButtons);
    // AutofidseedParam
    // Autofidseed - goes on the AutoSeedModel panel under the shared beadtrack panel
    pnlAutofidseedParam.setLayout(new BoxLayout(pnlAutofidseedParam, BoxLayout.X_AXIS));
    pnlAutofidseedParam.add(pnlInitialBeadFinding);
    pnlAutofidseedParam.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlAutofidseedParam.add(pnlBeadSearchingAndSorting);
    // Initial bead finding
    pnlInitialBeadFinding
        .setLayout(new BoxLayout(pnlInitialBeadFinding, BoxLayout.Y_AXIS));
    pnlInitialBeadFinding.setBorder(new EtchedBorder("Initial Bead Finding Parameters")
        .getBorder());
    pnlInitialBeadFinding.setAlignmentY(Box.TOP_ALIGNMENT);
    pnlInitialBeadFinding.add(pnlBoundaryModel);
    pnlInitialBeadFinding.add(pnlExcludeInsideAreas);
    pnlInitialBeadFinding.add(pnlAdjustSizes);
    pnlInitialBeadFinding.add(ltfBordersInXandY.getComponent());
    pnlInitialBeadFinding.add(ltfMinGuessNumBeads.getComponent());
    pnlInitialBeadFinding.add(ltfMinSpacing.getComponent());
    pnlInitialBeadFinding.add(ltfPeakStorageFraction.getComponent());
    pnlInitialBeadFinding.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlInitialBeadFinding.add(pnl3dmodInitialBeadFinding);
    // Boundary model
    pnlBoundaryModel.setLayout(new BoxLayout(pnlBoundaryModel, BoxLayout.X_AXIS));
    pnlBoundaryModel.add(cbBoundaryModel);
    pnlBoundaryModel.add(btnBoundaryModel.getComponent());
    // ExcludeInsideAreas
    pnlExcludeInsideAreas
        .setLayout(new BoxLayout(pnlExcludeInsideAreas, BoxLayout.X_AXIS));
    pnlExcludeInsideAreas.add(cbExcludeInsideAreas);
    pnlExcludeInsideAreas.add(Box.createHorizontalGlue());
    // AdjustSizes
    pnlAdjustSizes.setLayout(new BoxLayout(pnlAdjustSizes, BoxLayout.X_AXIS));
    pnlAdjustSizes.add(cbAdjustSizes);
    pnlAdjustSizes.add(Box.createHorizontalGlue());
    // 3dmodInitialBeadFinding
    pnl3dmodInitialBeadFinding.setLayout(new BoxLayout(pnl3dmodInitialBeadFinding,
        BoxLayout.X_AXIS));
    pnl3dmodInitialBeadFinding.add(Box.createHorizontalGlue());
    pnl3dmodInitialBeadFinding.add(btn3dmodInitialBeadFinding.getComponent());
    pnl3dmodInitialBeadFinding.add(Box.createHorizontalGlue());
    // Bead sorting and searching
    pnlBeadSearchingAndSorting.setLayout(new BoxLayout(pnlBeadSearchingAndSorting,
        BoxLayout.Y_AXIS));
    pnlBeadSearchingAndSorting.setBorder(new EtchedBorder(
        "Selection and Sorting Parameters").getBorder());
    pnlBeadSearchingAndSorting.setAlignmentY(Box.TOP_ALIGNMENT);
    pnlBeadSearchingAndSorting.add(pnlTarget);
    pnlBeadSearchingAndSorting.add(pnlTwoSurfaces);
    pnlBeadSearchingAndSorting.add(pnlAppendToSeedModel);
    pnlBeadSearchingAndSorting.add(ltfIgnoreSurfaceData.getComponent());
    pnlBeadSearchingAndSorting.add(ltfDropTracks.getComponent());
    pnlBeadSearchingAndSorting.add(ltfMaxMajorToMinorRatio.getComponent());
    pnlBeadSearchingAndSorting.add(pnlClusteredPointsAllowed1);
    pnlBeadSearchingAndSorting.add(cbsElongatedPointsAllowed.getContainer());
    pnlBeadSearchingAndSorting.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlBeadSearchingAndSorting.add(pnl3dmodBeadSortingAndSearching);
    pnlBeadSearchingAndSorting.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlBeadSearchingAndSorting.add(pnl3dmodClusteredElongatedModel);
    // Target
    pnlTarget.setLayout(new BoxLayout(pnlTarget, BoxLayout.Y_AXIS));
    pnlTarget.setBorder(new EtchedBorder("Seed Points to Select").getBorder());
    pnlTarget.add(rtfTargetNumberOfBeads.getContainer());
    pnlTarget.add(rtfTargetDensityOfBeads.getContainer());
    // TwoSurfaces
    pnlTwoSurfaces.setLayout(new BoxLayout(pnlTwoSurfaces, BoxLayout.X_AXIS));
    pnlTwoSurfaces.add(cbTwoSurfaces);
    pnlTwoSurfaces.add(Box.createHorizontalGlue());
    // AppendToSeedModel
    pnlAppendToSeedModel.setLayout(new BoxLayout(pnlAppendToSeedModel, BoxLayout.X_AXIS));
    pnlAppendToSeedModel.add(cbAppendToSeedModel);
    pnlAppendToSeedModel.add(Box.createHorizontalGlue());
    // ClusteredPointsAllowed1
    pnlClusteredPointsAllowed1.setLayout(new BoxLayout(pnlClusteredPointsAllowed1,
        BoxLayout.X_AXIS));
    pnlClusteredPointsAllowed1.add(cbClusteredPointsAllowedClustered);
    pnlClusteredPointsAllowed1.add(Box.createHorizontalGlue());
    // 3dmodBeadSortingAndSearching
    pnl3dmodBeadSortingAndSearching.setLayout(new BoxLayout(
        pnl3dmodBeadSortingAndSearching, BoxLayout.X_AXIS));
    pnl3dmodBeadSortingAndSearching.add(Box.createHorizontalGlue());
    pnl3dmodBeadSortingAndSearching.add(btn3dmodBeadSelectionAndSorting.getComponent());
    pnl3dmodBeadSortingAndSearching.add(Box.createHorizontalGlue());
    // 3dmodClusteredElongatedModel
    pnl3dmodClusteredElongatedModel.setLayout(new BoxLayout(
        pnl3dmodClusteredElongatedModel, BoxLayout.X_AXIS));
    pnl3dmodClusteredElongatedModel.add(Box.createHorizontalGlue());
    pnl3dmodClusteredElongatedModel.add(btn3dmodClusteredElongatedModel.getComponent());
    pnl3dmodClusteredElongatedModel.add(Box.createHorizontalGlue());
    // Buttons
    pnlButtons.add(btnAutofidseed.getComponent());
    pnlButtons.add(Box.createHorizontalGlue());
    pnlButtons.add(btn3dmodAutofidseed.getComponent());
    pnlButtons.add(Box.createHorizontalGlue());
    pnlButtons.add(btnCleanup.getComponent());
    // update
    updateAdvanced();
    updateEnabled();
    updateMethod();
    changeSeedAndTrackTab();
    changeRunRaptorTab();
    updateSeedModel();
    updateDisplay();
  }

  /**
   * Responds to the method radio buttons.  Places the panel which corresponds to the
   * currently selected radio button in the main panel.
   */
  private void updateMethod() {
    // Changed the panel
    if (curMethodIndex != -1) {
      pnlMain.remove(pnlMethodArray[curMethodIndex]);
    }
    curMethodIndex = ((RadioButton.RadioButtonModel) bgMethod.getSelection())
        .getEnumeratedType().getValue().getInt();
    pnlMain.add(pnlMethodArray[curMethodIndex]);
    // Refresh the tabs if necessary
    if (curMethodIndex == TrackingMethod.SEED.getValue().getInt()) {
      changeSeedAndTrackTab();
    }
    else if (curMethodIndex == TrackingMethod.RAPTOR.getValue().getInt()) {
      changeRunRaptorTab();
    }
    else {
      UIHarness.INSTANCE.pack(axisID, applicationManager);
    }
  }

  /**
   * Responds to the make seed and track tabs.  Places the body panel into the panel in
   * the tab which the user selected.
   */
  private void changeSeedAndTrackTab() {
    int newIndex = tpSeedAndTrack.getSelectedIndex();
    // Change the tab body panel
    if ((curSeedAndTrackTab == null && newIndex != -1)
        || !curSeedAndTrackTab.equals(newIndex)) {
      // If the tab has changed:
      // Remove the body from previous tab so that the size of the tabbed pane won't
      // reflect it.
      if (curSeedAndTrackTab != null) {
        pnlSeedAndTrackArray[curSeedAndTrackTab.index]
            .remove(pnlSeedAndTrackBodyArray[curSeedAndTrackTab.index]);
      }
      // Update the current tab and add the body to the tabbed pane
      curSeedAndTrackTab = SeedAndTrackTab.getInstance(newIndex);
      pnlSeedAndTrackArray[curSeedAndTrackTab.index]
          .add(pnlSeedAndTrackBodyArray[curSeedAndTrackTab.index]);
    }
    // Replace shared panels
    if (curSeedAndTrackTab == SeedAndTrackTab.TRACK) {
      pnlSeedAndTrackBodyArray[curSeedAndTrackTab.index].add(pnlBeadtrack.getContainer());
      pnlBeadtrack.updateAutofidseed(false);
    }
    // Refresh the auto seed panel if necessary
    if (curSeedAndTrackTab == SeedAndTrackTab.SEED
        && curSeedModelIndex == SeedModelEnumeratedType.AUTO.value.getInt()) {
      updateSeedModel();
    }
    else {
      UIHarness.INSTANCE.pack(axisID, applicationManager);
    }
  }

  /**
   * Responds to the make run raptor tabs.  Places the body panel into the panel in the
   * tab which the user selected.
   */
  private void changeRunRaptorTab() {
    int newIndex = tpRunRaptor.getSelectedIndex();
    // Change the tab body panel
    if ((curRunRaptorTab == null && newIndex != -1) || !curRunRaptorTab.equals(newIndex)) {
      // If the tab has changed:
      // Remove the body from previous tab so that the size of the tabbed pane won't
      // reflect it.
      if (curRunRaptorTab != null) {
        pnlRunRaptorArray[curRunRaptorTab.index]
            .remove(pnlRunRaptorBodyArray[curRunRaptorTab.index]);
      }
      curRunRaptorTab = RunRaptorTab.getInstance(newIndex);
      pnlRunRaptorArray[curRunRaptorTab.index]
          .add(pnlRunRaptorBodyArray[curRunRaptorTab.index]);
    }
    // Replace shared panels
    if (curRunRaptorTab == RunRaptorTab.TRACK) {
      pnlRunRaptorBodyArray[curRunRaptorTab.index].add(pnlBeadtrack.getContainer());
      pnlBeadtrack.updateAutofidseed(false);
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Responds to the seed model radio buttons.  Places the panel which corresponds to the
   * currently selected radio button in the seed tab's body panel.
   */
  private void updateSeedModel() {
    int newIndex = ((RadioButton.RadioButtonModel) bgSeedModel.getSelection())
        .getEnumeratedType().getValue().getInt();
    // Change the panel
    if ((curSeedModelIndex == -1 && newIndex != -1) || curSeedModelIndex != newIndex) {
      // If a different radio button has been selected:
      if (curSeedModelIndex != -1) {
        pnlSeedAndTrackBodyArray[SeedAndTrackTab.SEED.index]
            .remove(pnlSeedModelArray[curSeedModelIndex]);
      }
      curSeedModelIndex = newIndex;
      pnlSeedAndTrackBodyArray[SeedAndTrackTab.SEED.index]
          .add(pnlSeedModelArray[curSeedModelIndex]);
    }
    // Replace shared panels
    if (curSeedAndTrackTab == SeedAndTrackTab.SEED
        && curSeedModelIndex == SeedModelEnumeratedType.AUTO.value.getInt()) {
      pnlAutoSeedModel.remove(pnlAutofidseed);
      pnlAutoSeedModel.add(pnlBeadtrack.getContainer());
      pnlAutoSeedModel.add(pnlAutofidseed);
      pnlBeadtrack.updateAutofidseed(true);
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  private void addListeners() {
    btnSeed.setContainer(this);
    btnAutofidseed.setContainer(this);
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    for (int i = 0; i < SeedAndTrackTab.NUM_TABS; i++) {
      pnlSeedAndTrackArray[i].addMouseListener(mouseAdapter);
      pnlSeedAndTrackBodyArray[i].addMouseListener(mouseAdapter);
    }
    for (int i = 0; i < RunRaptorTab.NUM_TABS; i++) {
      pnlRunRaptorArray[i].addMouseListener(mouseAdapter);
      pnlRunRaptorBodyArray[i].addMouseListener(mouseAdapter);
    }
    tpSeedAndTrack.addChangeListener(new SeedAndTrackTabChangeListener(this));
    tpRunRaptor.addChangeListener(new RunRaptorTabChangeListener(this));
    btnSeed.addActionListener(actionListener);
    btnAutofidseed.addActionListener(actionListener);
    btn3dmodAutofidseed.addActionListener(actionListener);
    btn3dmodInitialBeadFinding.addActionListener(actionListener);
    btn3dmodBeadSelectionAndSorting.addActionListener(actionListener);
    btnCleanup.addActionListener(actionListener);
    btnAdvanced.register(this);
    rbMethodSeed.addActionListener(actionListener);
    rbMethodPatchTracking.addActionListener(actionListener);
    rbMethodRaptor.addActionListener(actionListener);
    rbSeedModelManual.addActionListener(actionListener);
    rbSeedModelAuto.addActionListener(actionListener);
    rbSeedModelTransfer.addActionListener(actionListener);
    cbBoundaryModel.addActionListener(actionListener);
    btnBoundaryModel.addActionListener(actionListener);
    btn3dmodClusteredElongatedModel.addActionListener(actionListener);
    cbAppendToSeedModel.addActionListener(actionListener);
  }

  public static String getUseRaptorResultLabel() {
    return RaptorPanel.USE_RAPTOR_RESULT_LABEL;
  }

  public void updateDisplay() {
    if (applicationManager.getState().isSeedingDone(axisID)) {
      btnSeed.setText(SEEDING_DONE_LABEL);
    }
    else {
      btnSeed.setText(SEEDING_NOT_DONE_LABEL);
    }
    boolean selected = cbBoundaryModel.isSelected();
    btnBoundaryModel.setEnabled(selected);
    cbExcludeInsideAreas.setEnabled(selected);
    if (cbAppendToSeedModel.isSelected()) {
      btnAutofidseed.setText(AUTOFIDSEED_APPEND_LABEL);
      rbSeedModelAuto.setText(AUTOFIDSEED_APPEND_TITLE);
    }
    else {
      btnAutofidseed.setText(AUTOFIDSEED_NEW_MODEL_LABEL);
      rbSeedModelAuto.setText(AUTOFIDSEED_NEW_MODEL_TITLE);
    }
  }

  public void expand(GlobalExpandButton button) {
    updateAdvanced(button.isExpanded());
  }

  public void expand(final ExpandButton button) {
  }

  /**
   * Set the advanced state for the dialog box
   */
  private void updateAdvanced() {
    updateAdvanced(isAdvanced());
  }

  private void updateAdvanced(final boolean advanced) {
    pnlBeadtrack.updateAdvanced(advanced);
    if (pnlTransferfid != null) {
      pnlTransferfid.updateAdvanced(advanced);
    }
    tiltxcorrPanel.updateAdvanced(advanced);

    cbAdjustSizes.setVisible(advanced);
    ltfBordersInXandY.setVisible(advanced);
    ltfMinGuessNumBeads.setVisible(advanced);
    ltfMinSpacing.setVisible(advanced);
    ltfPeakStorageFraction.setVisible(advanced);
    ltfIgnoreSurfaceData.setVisible(advanced);
    ltfDropTracks.setVisible(advanced);
    ltfMaxMajorToMinorRatio.setVisible(advanced);
    cbClusteredPointsAllowedClustered.setVisible(advanced);
    cbsElongatedPointsAllowed.setVisible(advanced);
    btn3dmodClusteredElongatedModel.setVisible(advanced);

    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public void updateEnabled() {
    if (pnlTransferfid != null) {
      pnlTransferfid.setEnabled(transferfidEnabled);
    }
    if (!transferfidEnabled && rbSeedModelTransfer.isEnabled()) {
      rbSeedModelTransfer.setEnabled(false);
      rbSeedModelManual.setSelected(true);
      updateMethod();
    }
    else if (transferfidEnabled) {
      rbSeedModelTransfer.setEnabled(true);
    }
    btn3dmodInitialBeadFinding.setEnabled(AutofidseedInitFileFilter.exists(
        applicationManager, axisID));
    boolean modelsExist = AutofidseedSelectionAndSorting.exists(applicationManager,
        axisID);
    btn3dmodBeadSelectionAndSorting.setEnabled(modelsExist);
    ltfIgnoreSurfaceData.setEnabled(modelsExist);
    boolean exists = FileType.CLUSTERED_ELONGATED_MODEL
        .exists(applicationManager, axisID);
    btn3dmodClusteredElongatedModel.setEnabled(exists);
  }

  /**
   * Set the parameters for the specified beadtrack panel
   */
  public void setBeadtrackParams(final BeadtrackParam beadtrackParams) {
    if (raptorPanel != null) {
      raptorPanel.setBeadtrackParams(beadtrackParams);
    }
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

  public boolean getParameters(final RunraptorParam param, final boolean doValidation) {
    if (raptorPanel != null) {
      return raptorPanel.getParameters(param, doValidation);
    }
    return true;
  }

  public void getParameters(final MetaData metaData) {
    if (raptorPanel != null) {
      raptorPanel.getParameters(metaData);
    }
    metaData.setTrackMethod(axisID, ((RadioButton.RadioButtonModel) bgMethod
        .getSelection()).getEnumeratedType().toString());
    tiltxcorrPanel.getParameters(metaData);
    metaData.setTrackSeedModelManual(rbSeedModelManual.isSelected(), axisID);
    metaData.setTrackSeedModelAuto(rbSeedModelAuto.isSelected(), axisID);
    metaData.setTrackSeedModelTransfer(rbSeedModelTransfer.isSelected(), axisID);
    metaData.setTrackExcludeInsideAreas(cbExcludeInsideAreas.isSelected(), axisID);
    metaData.setTrackTargetNumberOfBeads(rtfTargetNumberOfBeads.getText(), axisID);
    metaData.setTrackTargetDensityOfBeads(rtfTargetDensityOfBeads.getText(), axisID);
    metaData.setTrackElongatedPointsAllowed(axisID, cbsElongatedPointsAllowed.getValue());
    metaData.setTrackAdvanced(btnAdvanced.isExpanded(), axisID);
    metaData.setSeedAndTrackTab(axisID, curSeedAndTrackTab.index);
    metaData.setRaptorTab(axisID, curRunRaptorTab.index);
  }

  public void setParameters(final ConstMetaData metaData) {
    TrackingMethod method = TrackingMethod.getInstance(metaData.getTrackMethod(axisID));
    if (method == TrackingMethod.SEED) {
      rbMethodSeed.setSelected(true);
    }
    else if (method == TrackingMethod.PATCH_TRACKING) {
      rbMethodPatchTracking.setSelected(true);
    }
    else if (axisID != AxisID.SECOND && method == TrackingMethod.RAPTOR) {
      rbMethodRaptor.setSelected(true);
    }
    if (raptorPanel != null) {
      raptorPanel.setParameters(metaData);
    }
    tiltxcorrPanel.setParameters(metaData);
    if (metaData.isTrackSeedModelManual(axisID)) {
      rbSeedModelManual.setSelected(true);
    }
    if (metaData.isTrackSeedModelAuto(axisID)) {
      rbSeedModelAuto.setSelected(true);
    }
    if (metaData.isTrackSeedModelTransfer(axisID)) {
      rbSeedModelTransfer.setSelected(true);
    }
    cbExcludeInsideAreas.setSelected(metaData.isTrackExcludeInsideAreas(axisID));
    rtfTargetNumberOfBeads.setText(metaData.getTrackTargetNumberOfBeads(axisID));
    rtfTargetDensityOfBeads.setText(metaData.getTrackTargetDensityOfBeads(axisID));
    // backwards compatibility
    cbsElongatedPointsAllowed.setSelected(metaData
        .isTrackClusteredPointsAllowedElongated(axisID));
    cbsElongatedPointsAllowed.setValue(metaData
        .getTrackClusteredPointsAllowedElongatedValue(axisID));
    if (!metaData.isTrackElongatedPointsAllowedNull(axisID)) {
      cbsElongatedPointsAllowed.setValue(metaData.getTrackElongatedPointsAllowed(axisID));
    }
    btnAdvanced.changeState(metaData.isTrackAdvanced(axisID));
    SeedAndTrackTab seedAndTrackTab = SeedAndTrackTab.getInstance(metaData
        .getSeedAndTrackTab(axisID));
    if (seedAndTrackTab != null) {
      tpSeedAndTrack.setSelectedIndex(seedAndTrackTab.index);
      changeSeedAndTrackTab();
    }
    RunRaptorTab runRaptorTab = RunRaptorTab.getInstance(metaData.getRaptorTab(axisID));
    if (runRaptorTab != null) {
      tpRunRaptor.setSelectedIndex(runRaptorTab.index);
      changeRunRaptorTab();
    }
    updateMethod();
    updateSeedModel();
    updateAdvanced();
  }

  public void setParameters(final ConstTiltxcorrParam tiltXcorrParams) {
    tiltxcorrPanel.setParameters(tiltXcorrParams);
  }

  public void setParameters(final ImodchopcontsParam param) {
    tiltxcorrPanel.setParameters(param);
  }

  public void setParameters(final AutofidseedParam param) {
    ltfMinGuessNumBeads.setText(param.getMinGuessNumBeads());
    ltfMinSpacing.setText(param.getMinSpacing());
    ltfPeakStorageFraction.setText(param.getPeakStorageFraction());
    cbBoundaryModel.setSelected(param.isBoundaryModel());
    if (cbExcludeInsideAreas.isEnabled()) {
      cbExcludeInsideAreas.setSelected(param.isExcludeInsideAreas());
    }
    cbAdjustSizes.setSelected(param.isAdjustSizes());
    ltfBordersInXandY.setText(param.getBordersInXandY());
    cbTwoSurfaces.setSelected(param.isTwoSurfaces());
    cbAppendToSeedModel.setSelected(param.isAppendToSeedModel());
    if (param.isTargetNumberOfBeads()) {
      rtfTargetNumberOfBeads.setSelected(true);
      rtfTargetNumberOfBeads.setText(param.getTargetNumberOfBeads());
    }
    if (param.isTargetDensityOfBeads()) {
      rtfTargetDensityOfBeads.setSelected(true);
      rtfTargetDensityOfBeads.setText(param.getTargetDensityOfBeads());
    }
    ltfMaxMajorToMinorRatio.setText(param.getMaxMajorToMinorRatio());
    cbClusteredPointsAllowedClustered.setSelected(param.isClusteredPointsAllowed());
    if (cbClusteredPointsAllowedClustered.isSelected()) {
      ClusteredPointsAllowed cpa = param.getClusteredPointsAllowed();
      // Backwards compatibility
      if (cpa != null && cpa.isElongated()) {
        cbsElongatedPointsAllowed.setSelected(true);
        cbsElongatedPointsAllowed.setValue(cpa.convertToDisplayValue());
      }
    }
    if (param.isElongatedPointsAllowedSet()) {
      cbsElongatedPointsAllowed.setSelected(true);
      cbsElongatedPointsAllowed.setValue(param.getElongatedPointsAllowed());
    }
    else {
      cbsElongatedPointsAllowed.setSelected(false);
    }
    ltfIgnoreSurfaceData.setText(param.getIgnoreSurfaceData());
    ltfDropTracks.setText(param.getDropTracks());
    updateDisplay();
    updateEnabled();
  }

  public boolean getParameters(final AutofidseedParam param, final boolean doValidation)
      throws FortranInputSyntaxException {
    try {
      param.setMinGuessNumBeads(ltfMinGuessNumBeads.getText(doValidation));
      param.setMinSpacing(ltfMinSpacing.getText(doValidation));
      param.setPeakStorageFraction(ltfPeakStorageFraction.getText(doValidation));
      param.setBoundaryModel(cbBoundaryModel.isSelected());
      param.setExcludeInsideAreas(cbExcludeInsideAreas.isEnabled()
          && cbExcludeInsideAreas.isSelected());
      param.setAdjustSizes(cbAdjustSizes.isSelected());
      param.setBordersInXandY(ltfBordersInXandY.getText(doValidation));
      param.setTwoSurfaces(cbTwoSurfaces.isSelected());
      param.setAppendToSeedModel(cbAppendToSeedModel.isSelected());
      if (rtfTargetNumberOfBeads.isSelected()) {
        param.setTargetNumberOfBeads(rtfTargetNumberOfBeads.getText(doValidation));
      }
      else {
        param.resetTargetNumberOfBeads();
      }
      if (rtfTargetDensityOfBeads.isSelected()) {
        param.setTargetDensityOfBeads(rtfTargetDensityOfBeads.getText(doValidation));
      }
      else {
        param.resetTargetDensityOfBeads();
      }
      param.setMaxMajorToMinorRatio(ltfMaxMajorToMinorRatio.getText(doValidation));
      param.setClusteredPointsAllowed(cbClusteredPointsAllowedClustered.isSelected());
      if (cbsElongatedPointsAllowed.isSelected()) {
        param.setElongatedPointsAllowed(cbsElongatedPointsAllowed.getValue());
      }
      else {
        param.resetElongatedPointsAllowed();
      }
      param.setIgnoreSurfaceData(ltfIgnoreSurfaceData.getText(doValidation));
      param.setDropTracks(ltfDropTracks.getText(doValidation));
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public final void setParameters(final ReconScreenState screenState) {
    if (pnlTransferfid != null) {
      pnlTransferfid.setParameters(screenState);
    }
    pnlBeadtrack.setParameters(screenState);
  }

  public boolean getTransferFidParams(final boolean doValidation) {
    if (pnlTransferfid != null) {
      return pnlTransferfid.getParameters(doValidation);
    }
    return true;
  }

  public boolean getTransferFidParams(final TransferfidParam transferFidParam,
      final boolean doValidation) {
    if (pnlTransferfid != null) {
      return pnlTransferfid.getParameters(transferFidParam, doValidation);
    }
    return true;
  }

  public void setTransferfidEnabled(final boolean fileExists) {
    transferfidEnabled = fileExists;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String[] manPagelabel;
    String[] manPage;
    boolean autofidseed = curMethodIndex == TrackingMethod.SEED.getValue().getInt()
        && curSeedAndTrackTab == SeedAndTrackTab.SEED
        && curSeedModelIndex == SeedModelEnumeratedType.AUTO.value.getInt();
    if (autofidseed) {
      manPagelabel = new String[] { "Autofidseed", "Imodfindbeads", "Beadtrack",
          "Sortbeadsurfs", "Pickbestseed" };
      manPage = new String[] { "autofidseed.html", "imodfindbeads.html",
          "beadtrack.html", "sortbeadsurfs.html", "pickbestseed.html" };
    }
    else {
      manPagelabel = new String[] { "Autofidseed", "Beadtrack", "Transferfid", "3dmod" };
      manPage = new String[] { "autofidseed.html", "beadtrack.html", "transferfid.html",
          "3dmod.html" };
    }

    String[] logFileLabel = { "Autofidseed", "Track", "Transferfid" };
    String[] logFile = new String[3];
    logFile[0] = "autofidseed" + axisID.getExtension() + ".log";
    logFile[1] = "track" + axisID.getExtension() + ".log";
    logFile[2] = "transferfid.log";

    String anchor;
    if (autofidseed) {
      anchor = "AutomaticSeed";
    }
    else {
      anchor = "GETTING FIDUCIAL";
    }
    new ContextPopup(rootPanel, mouseEvent, anchor, ContextPopup.TOMO_GUIDE,
        manPagelabel, manPage, logFileLabel, logFile, applicationManager, axisID);
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
    rbSeedModelManual
        .setToolTipText("Open 3dmod to create a seed model by selecting fiducials "
            + "manually.");
    rbSeedModelAuto
        .setToolTipText("Use Autofidseed to select a fiducial seed model automatically.");
    rbSeedModelTransfer
        .setToolTipText("Create a seed mode by transferring the fiducial selection from "
            + "the other axis.");
    btn3dmodAutofidseed
        .setToolTipText("Open the model of seed points selected by Autofidseed.");
    btn3dmodInitialBeadFinding
        .setToolTipText("Open model of beads found by Imodfindbeads on all of the views "
            + "analyzed.");
    btn3dmodBeadSelectionAndSorting
        .setToolTipText("Open 3D models of beads sorted onto two surfaces in the "
            + "different Beadtrack runs.");
    btnCleanup.setToolTipText("Delete the temporary directory.");
    btnAutofidseed
        .setToolTipText("Run Autofidseed to find beads, track them through 11 views, and "
            + "select a seed model.");
    cbsElongatedPointsAllowed
        .setSpinnerToolTipText("Select 1, 2, or 3 to include beads identified as "
            + "elongated in up to 1/3, up to 2/3, or all of the Beadtrack runs, "
            + "respectively");
    btn3dmodClusteredElongatedModel
        .setToolTipText("Open a model with all beads that are candidates for selection, "
            + "color-coded by whether they are clustered or elongated");
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(applicationManager,
          AutodocFactory.AUTOFIDSEED, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    String tooltip = EtomoAutodoc
        .getTooltip(autodoc, AutofidseedParam.BOUNDARY_MODEL_KEY);
    cbBoundaryModel.setToolTipText(tooltip);
    btnBoundaryModel
        .setToolTipText("Open 3dmod to create or edit a model with contours around areas "
            + "to include or exclude");
    cbExcludeInsideAreas.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.EXCLUDE_INSIDE_AREAS_KEY));
    cbAdjustSizes.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.ADJUST_SIZES_KEY));
    ltfBordersInXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.BORDERS_IN_X_AND_Y_KEY));
    ltfMinGuessNumBeads.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.MIN_GUESS_NUM_BEADS_KEY));
    ltfMinSpacing.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.MIN_SPACING_KEY));
    ltfPeakStorageFraction.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.PEAK_STORAGE_FRACTION_KEY));
    rtfTargetNumberOfBeads.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.TARGET_NUMBER_OF_BEADS_KEY));
    rtfTargetDensityOfBeads.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.TARGET_DENSITY_OF_BEADS_KEY));
    cbTwoSurfaces.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.TWO_SURFACES_KEY));
    cbAppendToSeedModel.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.APPEND_TO_SEED_MODEL_KEY));
    ltfIgnoreSurfaceData.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.IGNORE_SURFACE_DATA_KEY));
    ltfDropTracks.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.DROP_TRACKS_KEY));
    ltfMaxMajorToMinorRatio.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.MAX_MAJOR_TO_MINOR_RATIO_KEY));
    cbClusteredPointsAllowedClustered.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.CLUSTERED_POINTS_ALLOWED_KEY));
    cbsElongatedPointsAllowed.setCheckBoxToolTipText(EtomoAutodoc.getTooltip(autodoc,
        AutofidseedParam.ELONGATED_POINTS_ALLOWED_KEY));

  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  // Action function for buttons
  private void buttonAction(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(rbMethodSeed.getActionCommand())
        || command.equals(rbMethodPatchTracking.getActionCommand())
        || command.equals(rbMethodRaptor.getActionCommand())) {
      updateMethod();
    }
    else if (command.equals(rbSeedModelManual.getActionCommand())
        || command.equals(rbSeedModelAuto.getActionCommand())
        || command.equals(rbSeedModelTransfer.getActionCommand())) {
      updateSeedModel();
    }
    else if (command.equals(btnSeed.getActionCommand())
        || command.equals(btn3dmodAutofidseed.getActionCommand())) {
      applicationManager.imodSeedModel(axisID, run3dmodMenuOptions, btnSeed,
          ImodManager.COARSE_ALIGNED_KEY,
          DatasetFiles.getSeedFileName(applicationManager, axisID),
          DatasetFiles.getRawTiltFile(applicationManager, axisID), dialogType);
    }
    else if (command.equals(cbBoundaryModel.getActionCommand())
        || command.equals(cbAppendToSeedModel.getActionCommand())) {
      updateDisplay();
    }
    else if (command.equals(btnAutofidseed.getActionCommand())) {
      applicationManager.autofidseed(axisID, btnAutofidseed, deferred3dmodButton,
          run3dmodMenuOptions, null, DIALOG_TYPE, this);
    }
    else if (command.equals(btn3dmodInitialBeadFinding.getActionCommand())) {
      String fileName = AutofidseedInitFileFilter.getFileName(applicationManager, axisID);
      if (fileName != null) {
        applicationManager.imodCoarseAlign(axisID, run3dmodMenuOptions, fileName, true);
      }
    }
    else if (command.equals(btn3dmodBeadSelectionAndSorting.getActionCommand())) {
      List<String> fileNameList = AutofidseedSelectionAndSorting.getFileNameList(
          applicationManager, axisID);
      if (fileNameList != null) {
        applicationManager.imodSortedModels(axisID, run3dmodMenuOptions, fileNameList);
      }
    }
    else if (command.equals(btn3dmodClusteredElongatedModel.getActionCommand())) {
      applicationManager.imodClusteredElongatedModel(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnCleanup.getActionCommand())) {
      applicationManager.cleanupAutofidseed(axisID);
      updateEnabled();
    }
    else if (command.equals(btnBoundaryModel.getActionCommand())) {
      applicationManager.imodModel(FileType.PREALIGNED_STACK,
          FileType.AUTOFIDSEED_BOUNDARY_MODEL, axisID, run3dmodMenuOptions, true);
    }
  }

  void done() {
    applicationManager.doneFiducialModelDialog(axisID);
    if (pnlTransferfid != null) {
      pnlTransferfid.done();
    }
    if (raptorPanel != null) {
      raptorPanel.done();
    }
    btnSeed.removeActionListener(actionListener);
    btnAutofidseed.removeActionListener(actionListener);
    pnlBeadtrack.done();
    tiltxcorrPanel.done();
    setDisplayed(false);
  }

  //
  // Action listener adapters
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

  private static final class SeedAndTrackTabChangeListener implements ChangeListener {
    private final FiducialModelDialog dialog;

    public SeedAndTrackTabChangeListener(final FiducialModelDialog dialog) {
      this.dialog = dialog;
    }

    public void stateChanged(final ChangeEvent changeEvent) {
      dialog.changeSeedAndTrackTab();
    }
  }

  private static final class RunRaptorTabChangeListener implements ChangeListener {
    private final FiducialModelDialog dialog;

    public RunRaptorTabChangeListener(final FiducialModelDialog dialog) {
      this.dialog = dialog;
    }

    public void stateChanged(final ChangeEvent changeEvent) {
      dialog.changeRunRaptorTab();
    }
  }

  public static final class SeedModelEnumeratedType implements EnumeratedType {
    private static final SeedModelEnumeratedType MANUAL = new SeedModelEnumeratedType(
        true, 0, "Manual");
    private static final SeedModelEnumeratedType AUTO = new SeedModelEnumeratedType(
        false, 1, "Auto");
    public static final SeedModelEnumeratedType TRANSFER = new SeedModelEnumeratedType(
        false, 2, "Transfer");

    private static final int NUM = 3;

    private final boolean isDefault;
    private final EtomoNumber value = new EtomoNumber();
    private final String string;

    private SeedModelEnumeratedType(final boolean isDefault, final int value,
        final String string) {
      this.isDefault = isDefault;
      this.value.set(value);
      this.string = string;
    }

    private static SeedModelEnumeratedType getInstance(final String string) {
      if (string == null) {
        return null;
      }
      if (string.equals(MANUAL.string)) {
        return MANUAL;
      }
      if (string.equals(AUTO.string)) {
        return AUTO;
      }
      if (string.equals(TRANSFER.string)) {
        return TRANSFER;
      }
      return null;
    }

    public boolean isDefault() {
      return isDefault;
    }

    public String getLabel() {
      return null;
    }

    public ConstEtomoNumber getValue() {
      return value;
    }

    public String toString() {
      return string;
    }
  }

  private static final class SeedAndTrackTab {
    private static final SeedAndTrackTab SEED = new SeedAndTrackTab(0, "Seed Model");
    private static final SeedAndTrackTab TRACK = new SeedAndTrackTab(1, "Track Beads");

    private static final SeedAndTrackTab DEFAULT = SEED;

    private static final int NUM_TABS = 2;

    private final int index;
    private final String title;

    private SeedAndTrackTab(final int index, final String title) {
      this.index = index;
      this.title = title;
    }

    private static SeedAndTrackTab getInstance(final int index) {
      if (index == SEED.index) {
        return SEED;
      }
      if (index == TRACK.index) {
        return TRACK;
      }
      return DEFAULT;
    }

    public boolean equals(final int index) {
      return index == this.index;
    }

    public String toString() {
      return "[index:" + index + ",title:" + title + "]";
    }
  }

  private static final class RunRaptorTab {
    private static final RunRaptorTab RAPTOR = new RunRaptorTab(0, "Run RAPTOR");
    private static final RunRaptorTab TRACK = new RunRaptorTab(1, "Track Beads");

    private static final RunRaptorTab DEFAULT = RAPTOR;

    private static final int NUM_TABS = 2;

    private final int index;
    private final String title;

    private RunRaptorTab(final int index, final String title) {
      this.index = index;
      this.title = title;
    }

    private static RunRaptorTab getInstance(final int index) {
      if (index == RAPTOR.index) {
        return RAPTOR;
      }
      if (index == TRACK.index) {
        return TRACK;
      }
      return DEFAULT;
    }

    public boolean equals(final int index) {
      return index == this.index;
    }
  }
}
