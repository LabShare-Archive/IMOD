/**
 * <p>Description: Panel to present the coarse alignment procedure</p>
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
 * <p> Revision 1.4  2011/02/24 23:37:04  sueh
 * <p> bug# 1452 imageRotation needs to be double everywhere.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:06:17  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 04:59:01  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.56  2010/10/11 20:34:46  sueh
 * <p> bug# 1379 In constructor, passing this to TiltxcorrPanel so can call this class's pop up menu function.
 * <p>
 * <p> Revision 3.55  2010/03/08 21:02:51  sueh
 * <p> bug# 1311 Moved the open coarse aligned stack button so that it is next to
 * <p> its associated run button.
 * <p>
 * <p> Revision 3.54  2010/03/03 05:03:22  sueh
 * <p> bug# 1311 Added TiltxcorrPanel and radio button for it.
 * <p>
 * <p> Revision 3.53  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.52  2009/06/12 19:48:13  sueh
 * <p> bug# 1221 Factored running correlation, making it independent of the
 * <p> coarse align dialog.
 * <p>
 * <p> Revision 3.51  2009/01/20 19:50:53  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.50  2008/10/16 21:09:32  sueh
 * <p> bug# 1141 Removed fixRootPanel because it doesn't do anything.
 * <p>
 * <p> Revision 3.49  2008/09/30 20:58:58  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 3.48  2008/05/13 23:00:15  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.47  2008/05/06 23:56:42  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 3.46  2008/05/03 00:49:19  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.45  2007/12/26 22:23:16  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 3.44  2007/09/10 20:42:16  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.43  2007/05/26 00:31:58  sueh
 * <p> bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * <p> Setting button size in UI.
 * <p>
 * <p> Revision 3.42  2007/02/09 00:48:30  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.41  2006/07/28 19:45:32  sueh
 * <p> bug# 868 Changed isFiduciallessAlignment to isFiducialess
 * <p>
 * <p> Revision 3.40  2006/07/20 17:19:48  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 3.39  2006/07/04 20:41:30  sueh
 * <p> bug# 898 Don't remove action listeners unless the done dialog function
 * <p> succeeds.
 * <p>
 * <p> Revision 3.38  2006/07/04 18:47:02  sueh
 * <p> bug# 893 Calling updateAdvanced(boolean) in panels to change the
 * <p> headers when the advanced button is pressed.
 * <p>
 * <p> Revision 3.37  2006/06/30 20:01:07  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.
 * <p>
 * <p> Revision 3.36  2006/06/21 15:50:42  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.35  2006/05/19 19:46:42  sueh
 * <p> bug# 866 Changed set/getTiltAxisAngle to set/getImageRotation.
 * <p>
 * <p> Revision 3.34  2006/03/30 16:47:29  sueh
 * <p> bug# 437 Moved coarse align button to PrenewstPanel.
 * <p>
 * <p> Revision 3.33  2006/03/27 21:00:37  sueh
 * <p> bug# 836 Moved btnCrossCorrelate to CrossCorrelationPanel.
 * <p>
 * <p> Revision 3.32  2006/03/22 18:00:22  sueh
 * <p> Made unchanging member variables final
 * <p>
 * <p> Revision 3.31  2006/02/06 21:20:45  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.30  2006/01/26 22:04:11  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.29  2006/01/03 23:31:30  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox
 * <p>
 * <p> Revision 3.28  2005/12/14 20:54:19  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 3.27  2005/10/27 00:34:26  sueh
 * <p> bug# 725 Calling preCrossCorrelate instead of crossCorrelate so that the
 * <p> B stack can be processed.
 * <p>
 * <p> Revision 3.26  2005/08/11 23:45:42  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 3.25  2005/08/10 20:41:54  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.24  2005/08/09 20:20:33  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.
 * <p>
 * <p> Revision 3.23  2005/08/04 20:08:25  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.22  2005/07/14 22:04:15  sueh
 * <p> bug# 626 Added getParams(BlendmontParam) and
 * <p> setParams(BlendmontParam).
 * <p>
 * <p> Revision 3.21  2005/07/06 23:32:15  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 3.20  2005/04/22 00:16:59  sueh
 * <p> bug# 615 Removed unnecessary imports.
 * <p>
 * <p> Revision 3.19  2005/04/22 00:14:25  sueh
 * <p> bug# 615 removed test raised beveled button.
 * <p>
 * <p> Revision 3.18  2005/04/21 20:32:08  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.17  2005/04/16 01:54:11  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.16  2005/04/07 22:03:22  sueh
 * <p> bug# 626 Added Make Distortion Corrected Stack button.
 * <p>
 * <p> Revision 3.15  2005/03/09 22:32:23  sueh
 * <p> bug# 533 Modify the context sensitive help so that it displays blendmont
 * <p> man pages and log files instead of newst when the view type is montage.
 * <p>
 * <p> Revision 3.14  2005/03/08 02:01:26  sueh
 * <p> bug# 533 Add btnEdgesMidas.
 * <p>
 * <p> Revision 3.13  2005/01/14 03:07:08  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.12  2004/12/02 20:37:18  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.11  2004/11/19 23:49:54  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.10.4.1  2004/10/11 02:11:10  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.10  2004/06/17 18:48:45  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.9  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.8  2004/05/25 23:24:32  rickg
 * <p> Bug #391 added fiducialess parameter interface and UI objects
 * <p>
 * <p> Revision 3.7  2004/04/23 19:38:43  rickg
 * <p> Method name change for opening 3dmod on the coarse
 * <p> aligned stack
 * <p>
 * <p> Revision 3.6  2004/04/07 21:03:10  rickg
 * <p> Fixed layout using UIUtilities
 * <p>
 * <p> Revision 3.5  2004/04/06 16:58:56  rickg
 * <p> Added fiducialess alignment methods
 * <p>
 * <p> Revision 3.4  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.3  2004/03/13 00:34:13  rickg
 * <p> Bug# 390 Add set/get prenewst parameters
 * <p>
 * <p> Revision 3.2  2004/02/05 04:35:07  rickg
 * <p> Added prenewst panel with binning
 * <p>
 * <p> Revision 3.1  2004/01/30 22:45:01  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.7  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.6  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.5  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.4  2003/10/14 21:56:05  sueh
 * <p> Bug273 add tooltips
 * <p>
 * <p> Revision 2.3  2003/05/23 22:14:55  rickg
 * <p> Added xcorr log file to context menu
 * <p>
 * <p> Revision 2.2  2003/05/19 22:06:43  rickg
 * <p> Fixed cross correlation button text
 * <p>
 * <p> Revision 2.1  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/19 00:28:41  rickg
 * <p> Advanced handling implemented
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/10/17 22:38:59  rickg
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

package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.TomodataplotsParam;
import etomo.logic.DatasetTool;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ViewType;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

public final class CoarseAlignDialog extends ProcessDialog implements ContextMenu,
    FiducialessParams, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private final EtomoPanel pnlCoarseAlign = new EtomoPanel();
  private final JPanel pnlFiducialess = new JPanel();
  private final CheckBox cbFiducialess = new CheckBox("Fiducialless alignment");
  private final LabeledTextField ltfRotation = new LabeledTextField(
      FieldType.FLOATING_POINT, "Tilt axis rotation:");

  private final ActionListener actionListener;
  private final MultiLineButton btnMidas;
  private final TiltxcorrPanel tiltxcorrPanel;
  private final PrenewstPanel pnlPrenewst;
  // Montaging
  private final MultiLineButton btnFixEdgesMidas;
  private final MultiLineButton btnDistortionCorrectedStack;

  private CoarseAlignDialog(final ApplicationManager appMgr, final AxisID axisID,
      final boolean magChangesMode) {
    super(appMgr, axisID, DialogType.COARSE_ALIGNMENT);
    ConstMetaData metaData = appMgr.getMetaData();
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnDistortionCorrectedStack = (MultiLineButton) displayFactory
        .getDistortionCorrectedStack();
    btnFixEdgesMidas = (MultiLineButton) displayFactory.getFixEdgesMidas();
    btnMidas = (MultiLineButton) displayFactory.getMidas();
    setToolTipText();
    tiltxcorrPanel = TiltxcorrPanel.getCrossCorrelationInstance(applicationManager,
        axisID, dialogType, btnAdvanced, this, magChangesMode);
    pnlPrenewst = new PrenewstPanel(applicationManager, axisID, dialogType, this,
        btnAdvanced);
    btnExecute.setText("Done");

    pnlFiducialess.setLayout(new BoxLayout(pnlFiducialess, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlFiducialess, cbFiducialess);
    UIUtilities.addWithYSpace(pnlFiducialess, ltfRotation.getContainer());
    UIUtilities.alignComponentsX(pnlFiducialess, Component.LEFT_ALIGNMENT);

    pnlCoarseAlign.setLayout(new BoxLayout(pnlCoarseAlign, BoxLayout.Y_AXIS));
    pnlCoarseAlign.setBorder(new BeveledBorder("Coarse Alignment").getBorder());
    UIUtilities.addWithSpace(pnlCoarseAlign, tiltxcorrPanel.getPanel(), FixedDim.x0_y10);
    if (metaData.getViewType() == ViewType.MONTAGE) {
      SpacedPanel pnlFixEdges = SpacedPanel.getInstance();
      pnlFixEdges.setBoxLayout(BoxLayout.Y_AXIS);
      pnlFixEdges.setBorder(new EtchedBorder("Fix Edges").getBorder());
      btnDistortionCorrectedStack.setSize();
      pnlFixEdges.add(btnDistortionCorrectedStack);
      btnFixEdgesMidas.setSize();
      pnlFixEdges.add(btnFixEdgesMidas);
      pnlCoarseAlign.add(pnlFixEdges.getContainer());
      if (!metaData.isDistortionCorrection()) {
        btnDistortionCorrectedStack.setEnabled(false);
      }
      setEnabledFixEdgesMidasButton();
    }
    UIUtilities.addWithSpace(pnlCoarseAlign, pnlPrenewst.getPanel(), FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlCoarseAlign, pnlFiducialess, FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlCoarseAlign, btnMidas.getComponent(), FixedDim.x0_y10);

    // Set the alignment and size of the UI objects
    UIUtilities.alignComponentsX(pnlCoarseAlign, Component.CENTER_ALIGNMENT);
    UIUtilities.setButtonSizeAll(pnlCoarseAlign,
        UIParameters.INSTANCE.getButtonDimension());

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    UIUtilities.addWithSpace(rootPanel, pnlCoarseAlign, FixedDim.x0_y10);
    addExitButtons();
    actionListener = new CoarseAlignActionListener(this);

    // Set the default advanced state for the window
    updateAdvanced();
  }

  public static CoarseAlignDialog getInstance(final ApplicationManager appMgr,
      final AxisID axisID, final boolean magChangesMode) {
    CoarseAlignDialog instance = new CoarseAlignDialog(appMgr, axisID, magChangesMode);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    // Action listener assignment for the buttons
    btnMidas.addActionListener(actionListener);
    btnFixEdgesMidas.addActionListener(actionListener);
    btnDistortionCorrectedStack.addActionListener(actionListener);

    // Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlCoarseAlign.addMouseListener(mouseAdapter);
  }

  /**
   * Enable Fix Edges with Midas button if there are no distortion correction files
   * in use or if the .dcst file (distortion corrected stack) has been created.
   *
   */
  public void setEnabledFixEdgesMidasButton() {
    if (btnDistortionCorrectedStack.isEnabled()
        && !BlendmontParam.getDistortionCorrectedFile(
            applicationManager.getPropertyUserDir(),
            applicationManager.getMetaData().getDatasetName(), axisID).exists()) {
      btnFixEdgesMidas.setEnabled(false);
    }
    else {
      btnFixEdgesMidas.setEnabled(true);
    }
  }

  /**
   * Set the parameters for the cross correlation panel
   */
  public void setCrossCorrelationParams(final ConstTiltxcorrParam tiltXcorrParams) {
    tiltxcorrPanel.setParameters(tiltXcorrParams);
  }

  public TiltXcorrDisplay getTiltXcorrDisplay() {
    return tiltxcorrPanel;
  }

  public NewstackDisplay getNewstackDisplay() {
    return pnlPrenewst;
  }

  /**
   * Set the prenewst params of the prenewst panel
   * @param prenewstParam
   */
  public void setPrenewstParams(final ConstNewstParam prenewstParam) {
    pnlPrenewst.setParameters(prenewstParam);
  }

  /**
   * Set the blendmont params of the prenewst panel
   * @param prenewstParam
   */
  public void setParams(final BlendmontParam blendmontParam) {
    pnlPrenewst.setParameters(blendmontParam);
  }

  public void setParameters(final ReconScreenState screenState) {
    tiltxcorrPanel.setParameters(screenState);
    pnlPrenewst.setParameters(screenState);
  }

  public void setParameters(final ConstMetaData metaData) {
    pnlPrenewst.setParameters(metaData);
  }

  public void getParameters(final BaseScreenState screenState) {
    tiltxcorrPanel.getParameters(screenState);
    pnlPrenewst.getParameters(screenState);
  }

  public void getParameters(final MetaData metaData) {
    pnlPrenewst.getParameters(metaData);
  }

  public void setFiducialessAlignment(final boolean state) {
    cbFiducialess.setSelected(state);
  }

  public boolean isFiducialess() {
    return cbFiducialess.isSelected();
  }

  public void setImageRotation(final String tiltAxisAngle) {
    ltfRotation.setText(tiltAxisAngle);
  }

  public String getImageRotation(final boolean doValidation)
      throws FieldValidationFailedException {
    return ltfRotation.getText(doValidation);
  }

  void updateAdvanced() {
    tiltxcorrPanel.updateAdvanced(isAdvanced());
    pnlPrenewst.updateAdvanced(isAdvanced());
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String alignManpageLabel;
    String alignManpage;
    String alignLogfileLabel;
    String alignLogfile;
    TomodataplotsParam.Task[] graph = null;
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      alignManpageLabel = "Blendmont";
      alignManpage = "blendmont";
      alignLogfileLabel = "Preblend";
      alignLogfile = "preblend";
      if (!DatasetTool.isOneBy(applicationManager.getPropertyUserDir(),
          FileType.RAW_STACK.getFileName(applicationManager, axisID), applicationManager,
          axisID)
          && applicationManager.getState().isXcorrBlendmontWasRun(axisID)) {
        graph = new TomodataplotsParam.Task[] { TomodataplotsParam.Task.COARSE_MEAN_MAX };
      }
    }
    else {
      alignManpageLabel = "Newstack";
      alignManpage = "newstack";
      alignLogfileLabel = "Prenewst";
      alignLogfile = "prenewst";
    }
    String[] manPagelabel = { "Tiltxcorr", "Xftoxg", alignManpageLabel, "3dmod", "Midas" };
    String[] manPage = { "tiltxcorr.html", "xftoxg.html", alignManpage + ".html",
        "3dmod.html", "midas.html" };
    String[] logFileLabel = { "Xcorr", alignLogfileLabel };
    String[] logFile = new String[2];
    logFile[0] = "xcorr" + axisID.getExtension() + ".log";
    logFile[1] = alignLogfile + axisID.getExtension() + ".log";

    ContextPopup contextPopup = new ContextPopup(pnlCoarseAlign, mouseEvent,
        "COARSE ALIGNMENT", ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel,
        logFile, graph, applicationManager, axisID);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    cbFiducialess
        .setToolTipText("Enable or disable the processing flow using cross-correlation alignment only.");
    btnMidas.setToolTipText("Use Midas to adjust bad alignments.");
    ltfRotation
        .setToolTipText("Initial rotation angle of tilt axis when viewing images in Midas.");
    btnDistortionCorrectedStack
        .setToolTipText("Create a stack to use in Midas that incorporates the corrections from the image distortion field file and/or the magnification gradients file.");
    btnFixEdgesMidas
        .setToolTipText("Use Midas to adjust the alignment of the montage frames.");
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), run3dmodMenuOptions);
  }

  /**
   * Action function for process buttons
   * @param event
   */
  void buttonAction(final String command, final Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnMidas.getActionCommand())) {
      applicationManager.midasRawStack(axisID, btnMidas);
    }
    else if (command.equals(btnFixEdgesMidas.getActionCommand())) {
      applicationManager.midasFixEdges(axisID, btnFixEdgesMidas);
    }
    else if (command.equals(btnDistortionCorrectedStack.getActionCommand())) {
      applicationManager.makeDistortionCorrectedStack(axisID,
          btnDistortionCorrectedStack, null);
    }
  }

  void done() {
    applicationManager.doneCoarseAlignDialog(axisID);
    tiltxcorrPanel.done();
    pnlPrenewst.done();
    btnDistortionCorrectedStack.removeActionListener(actionListener);
    btnFixEdgesMidas.removeActionListener(actionListener);
    btnMidas.removeActionListener(actionListener);
    setDisplayed(false);
  }

  private static final class CoarseAlignActionListener implements ActionListener {
    private final CoarseAlignDialog adaptee;

    private CoarseAlignActionListener(final CoarseAlignDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null);
    }
  }
}