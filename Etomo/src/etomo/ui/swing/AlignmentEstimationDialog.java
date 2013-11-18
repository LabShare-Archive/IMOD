package etomo.ui.swing;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import etomo.ApplicationManager;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TomodataplotsParam;
import etomo.process.ImodProcess;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
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
 * <p> Revision 1.2  2010/12/05 04:50:50  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.41  2010/03/11 06:02:45  sueh
 * <p> bug# 1311 Setting patchTracking in this class so the right residual mode
 * <p> can be passed to ApplicationManager.imodFixFiducials.
 * <p>
 * <p> Revision 3.40  2010/03/03 05:00:33  sueh
 * <p> bug# 1311 Added setPatchTracking and setSurfacesToAnalyze.
 * <p>
 * <p> Revision 3.39  2009/12/19 01:12:34  sueh
 * <p> bug# 1294 Generalized ApplicationManager.imodView3DModel so it can open the smoothing assessment model.  Renamed it imodViewModel.
 * <p>
 * <p> Revision 3.38  2009/10/19 15:24:32  sueh
 * <p> bug# 1247 Formatted.
 * <p>
 * <p> Revision 3.37  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.36  2009/01/20 19:43:25  sueh
 * <p> bug# 1102 Changed labeled panels to EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.35  2008/12/05 00:53:41  sueh
 * <p> bug# 1156 Added a skipList parameter to
 * <p> ApplicationManager.imodFixFiducials.  Passing null when in
 * <p> RESIDUAL_MODE.
 * <p>
 * <p> Revision 3.34  2008/11/21 17:11:28  sueh
 * <p> bug# 1123 Added get and setParameters(ReconScreenState).
 * <p>
 * <p> Revision 3.33  2008/10/16 21:09:12  sueh
 * <p> bug# 1141 Removed fixRootPanel because it doesn't do anything.
 * <p>
 * <p> Revision 3.32  2008/09/30 20:54:20  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 3.31  2008/05/03 00:46:32  sueh
 * <p> bug# 847 Passing null to process funtions for ProcessSeries.
 * <p>
 * <p> Revision 3.30  2007/12/26 22:22:00  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 3.29  2007/11/14 23:47:02  sueh
 * <p> bug# 1048 Added beam tilt tab to the log file display - if taBeamtilt.log is not empty.
 * <p>
 * <p> Revision 3.28  2007/09/10 20:41:39  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.27  2007/05/26 00:31:14  sueh
 * <p> bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * <p> Setting button size in UI.
 * <p>
 * <p> Revision 3.26  2007/03/07 21:10:39  sueh
 * <p> bug# 981 Added isValid.
 * <p>
 * <p> Revision 3.25  2007/03/03 01:01:16  sueh
 * <p> bug# 973 Getting/setting metadata in Fine Align dialog.
 * <p>
 * <p> Revision 3.24  2007/02/09 00:43:53  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.23  2006/07/05 23:25:15  sueh
 * <p> Get fine alignment fix fiducials to set the right mode.
 * <p>
 * <p> Revision 3.22  2006/07/04 20:41:18  sueh
 * <p> bug# 898 Don't remove action listeners unless the done dialog function
 * <p> succeeds.
 * <p>
 * <p> Revision 3.21  2006/06/30 20:00:19  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.
 * <p>
 * <p> Revision 3.20  2006/02/06 21:20:27  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.19  2006/01/26 22:03:29  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.18  2006/01/12 17:04:57  sueh
 * <p> bug# 798 Reducing the visibility and inheritability of ui classes.
 * <p>
 * <p> Revision 3.17  2005/11/14 21:27:29  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 3.16  2005/08/11 23:44:08  sueh
 * <p> bug# 711  Get rid of duplicate code by running the 3dmods from a private
 * <p> function called run3dmod(String, Run3dmodMenuOptions).  It can be
 * <p> called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and the
 * <p> action function.
 * <p>
 * <p> Revision 3.15  2005/08/10 20:40:06  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.14  2005/08/09 20:09:45  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.
 * <p>
 * <p> Revision 3.13  2005/08/04 19:54:59  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.12  2005/07/06 23:31:12  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 3.11  2005/06/11 02:47:25  sueh
 * <p> bug# 583, bug# 682, bug# 679  Moved binning calculation to
 * <p> ApplicationManager.  Upgraded align.com and tilt.com to have all
 * <p> unbinned parameters and a binning value.  Fixed potential divide by 0
 * <p> errors and incorrect binning calculation errors in Fine Align.  Removed
 * <p> function: setPrealignedBinning.
 * <p>
 * <p> Revision 3.10  2005/04/21 20:31:25  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.9  2005/04/16 01:51:25  sueh
 * <p> bug# 615 Moved the adding of exit buttons to base class to standardize.
 * <p>
 * <p> Revision 3.8  2005/01/14 03:06:57  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.7  2004/12/30 18:37:27  sueh
 * <p> bug# 567 Make space between the two rows of buttons.
 * <p>
 * <p> Revision 3.6  2004/12/30 18:00:31  sueh
 * <p> bug# 567 Changing the buttons so they in two rows of two.
 * <p>
 * <p> Revision 3.5  2004/07/02 00:34:46  sueh
 * <p> bug# 461 construct TiltalignPanel with info from fid.xyz,
 * <p> preali header, and raw stack header
 * <p>
 * <p> Revision 3.4  2004/06/21 17:16:37  rickg
 * <p> Bug #461 z shift is scaled by the prealigned binning
 * <p>
 * <p> Revision 3.3  2004/06/05 00:51:25  sueh
 * <p> bug# 433 passing applicationManager to ContextPopup so that new
 * <p> versions of the ta* align logs can be generated.
 * <p>
 * <p> Revision 3.2  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.1  2004/01/30 22:44:22  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.14  2003/10/28 21:22:50  rickg
 * <p> Bug# 280 Tooltips
 * <p>
 * <p> Revision 2.13  2003/10/28 00:22:07  rickg
 * <p> Bug# 309 Capitalized align log tabs
 * <p>
 * <p> Revision 2.12  2003/10/28 00:13:41  rickg
 * <p> Bug# 309 Capitalized align log tabs
 * <p>
 * <p> Revision 2.11  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.10  2003/10/09 23:23:19  rickg
 * <p> Fixed context menu (needed to remove transferfid entries)
 * <p>
 * <p> Revision 2.9  2003/10/07 22:43:13  sueh
 * <p> bug251 moved transferfid from fine alignment dialog
 * <p> to fiducial model dialog
 * <p>
 * <p> Revision 2.8  2003/06/05 21:08:03  rickg
 * <p> Label change for transferfid button
 * <p>
 * <p> Revision 2.7  2003/06/05 04:42:01  rickg
 * <p> Button order swap
 * <p>
 * <p> Revision 2.6  2003/06/04 23:42:29  rickg
 * <p> Added independent labels for tabs
 * <p> Log file tab name changes
 * <p>
 * <p> Revision 2.5  2003/05/27 17:24:03  rickg
 * <p> Rearranged align log order and added locals
 * <p>
 * <p> Revision 2.4  2003/05/27 08:48:24  rickg
 * <p> Tabbed window for align log
 * <p>
 * <p> Revision 2.3  2003/05/23 22:50:13  rickg
 * <p> Removed any extensions from log file labels in context menu
 * <p>
 * <p> Revision 2.2  2003/04/28 23:25:26  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.1  2003/02/24 23:23:02  rickg
 * <p> Corrected acition spelling
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.19.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.19  2003/01/10 20:46:34  rickg
 * <p> Added ability to view 3D fiducial models
 * <p>
 * <p> Revision 1.18  2003/01/08 18:47:54  rickg
 * <p> Added a preferred size for the buttons so that the
 * <p> window size is more reasonable
 * <p>
 * <p> Revision 1.17  2003/01/08 05:28:25  rickg
 * <p> Still working to fix window layout :(
 * <p>
 * <p> Revision 1.16  2003/01/08 04:00:21  rickg
 * <p> Mods in progress
 * <p>
 * <p> Revision 1.15  2003/01/07 00:33:00  rickg
 * <p> Changed button panel layout to grid and added
 * <p> button to view residuals in imod
 * <p>
 * <p> Revision 1.14  2003/01/06 17:35:43  rickg
 * <p> Check for dual axis before calling second axis panel
 * <p> or transferfid components in advanced
 * <p>
 * <p> Revision 1.13  2003/01/06 04:55:04  rickg
 * <p> Changed layout to a single transferfid panel
 * <p>
 * <p> Revision 1.12  2003/01/04 00:34:45  rickg
 * <p> bound actions listeners for transferfid buttons
 * <p> added getTransferFidParams method
 * <p> implemented button actions for transfer fiducials
 * <p>
 * <p> Revision 1.11  2002/12/31 23:13:01  rickg
 * <p> Implemented logic for transferfid buttons
 * <p>
 * <p> Revision 1.10  2002/12/20 01:25:05  rickg
 * <p> Adding fiducial transfer interface
 * <p>
 * <p> Revision 1.9  2002/12/19 00:28:34  rickg
 * <p> Advanced handling implemented
 * <p>
 * <p> Revision 1.8  2002/12/18 18:52:13  rickg
 * <p> Restructured buttons and layout
 * <p>
 * <p> Revision 1.7  2002/12/05 01:22:44  rickg
 * <p> Previous commit comment was incorrect
 * <p> Split advanced functionality from action handler
 * <p> so that it can be called at startup.
 * <p>
 * <p> Revision 1.6  2002/12/05 01:20:37  rickg
 * <p> Added isAdvanced stub
 * <p>
 * <p> Revision 1.5  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.4  2002/10/17 23:42:26  rickg
 * <p> Spaced buttons some
 * <p> Call default parameters methods in panelAlign objects
 * <p>
 * <p> Revision 1.3  2002/10/17 22:38:47  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:30:28  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public final class AlignmentEstimationDialog extends ProcessDialog implements
    ContextMenu, Run3dmodButtonContainer {

  public static final String rcsid = "$Id$";

  private EtomoPanel pnlAlignEst = new EtomoPanel();
  private BeveledBorder border = new BeveledBorder("Fine Alignment");
  private TiltalignPanel pnlTiltalign;
  private JPanel panelButton = new JPanel();
  private final MultiLineButton btnComputeAlignment;
  private Run3dmodButton btnImod = Run3dmodButton.get3dmodInstance(
      "View/Edit Fiducial Model", this);
  private MultiLineButton btnView3DModel = new MultiLineButton(
      "<html><b>View 3D Model</b>");
  private Run3dmodButton btnViewResiduals = Run3dmodButton.get3dmodInstance(
      "View Residual Vectors", this);
  private final AlignmentEstimationActionListner actionListener;

  private boolean patchTracking = false;

  public AlignmentEstimationDialog(ApplicationManager appMgr, AxisID axisID) {
    super(appMgr, axisID, DialogType.FINE_ALIGNMENT);
    btnComputeAlignment = (MultiLineButton) appMgr.getProcessResultDisplayFactory(axisID)
        .getComputeAlignment();
    pnlTiltalign = TiltalignPanel.getInstance(axisID, appMgr, btnAdvanced);
    btnExecute.setText("Done");

    // Create the first tiltalign panel
    panelButton.setLayout(new BoxLayout(panelButton, BoxLayout.Y_AXIS));

    btnComputeAlignment.setSize();
    btnImod.setSize();
    btnViewResiduals.setSize();
    btnView3DModel.setSize();

    SpacedPanel topButtonPanel = SpacedPanel.getInstance();
    topButtonPanel.setBoxLayout(BoxLayout.X_AXIS);
    btnComputeAlignment.setSize();
    topButtonPanel.add(btnComputeAlignment);
    btnImod.setSize();
    topButtonPanel.add(btnImod);
    panelButton.add(topButtonPanel.getContainer());
    panelButton.add(Box.createRigidArea(FixedDim.x0_y10));
    SpacedPanel bottomButtonPanel = SpacedPanel.getInstance();
    bottomButtonPanel.setBoxLayout(BoxLayout.X_AXIS);
    btnView3DModel.setSize();
    bottomButtonPanel.add(btnView3DModel);
    // panelButton.add(Box.createRigidArea(FixedDim.x10_y0));
    btnViewResiduals.setSize();
    bottomButtonPanel.add(btnViewResiduals);
    panelButton.add(bottomButtonPanel.getContainer());

    pnlAlignEst.setLayout(new BoxLayout(pnlAlignEst, BoxLayout.Y_AXIS));
    pnlAlignEst.setBorder(border.getBorder());

    pnlAlignEst.add(pnlTiltalign.getContainer());
    pnlAlignEst.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlAlignEst.add(panelButton);

    // Construct the main panel from the alignment panel and exist buttons
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    JScrollPane scrollPane = new JScrollPane(pnlAlignEst);
    rootPanel.add(pnlAlignEst, BorderLayout.CENTER);
    addExitButtons();

    // Bind the action listeners to the buttons
    actionListener = new AlignmentEstimationActionListner(this);

    btnComputeAlignment.addActionListener(actionListener);
    btnView3DModel.addActionListener(actionListener);
    btnViewResiduals.addActionListener(actionListener);
    btnImod.addActionListener(actionListener);

    // Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    pnlTiltalign.getContainer().addMouseListener(mouseAdapter);

    // Set the default advanced state
    updateAdvanced();
    pnlTiltalign.setFirstTab();
    setToolTipText();
  }

  public void setParameters(BaseScreenState screenState) {
    pnlTiltalign.setParameters(screenState);
  }

  public void setPatchTracking(boolean input) {
    patchTracking = input;
    pnlTiltalign.setPatchTracking(input);
  }

  public void setSurfacesToAnalyze(int surfacesToAnalyze) {
    pnlTiltalign.setSurfacesToAnalyze(surfacesToAnalyze);
  }

  public void getParameters(BaseScreenState screenState) {
    pnlTiltalign.getParameters(screenState);
  }

  public void setParameters(ConstMetaData metaData) {
    pnlTiltalign.setParameters(metaData);
  }

  public void getParameters(final MetaData metaData) {
    pnlTiltalign.getParameters(metaData);
  }

  public void setTiltalignParams(final TiltalignParam tiltalignParam) {
    pnlTiltalign.setParameters(tiltalignParam);
  }

  public boolean getTiltalignParams(final TiltalignParam tiltalignParam,
      final boolean doValidation) throws FortranInputSyntaxException {
    try {
      if (!pnlTiltalign.getParameters(tiltalignParam, doValidation)) {
        return false;
      }
    }
    catch (FortranInputSyntaxException except) {
      String message = "Axis: " + axisID.getExtension() + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }
    return true;
  }

  public boolean isValid() {
    return pnlTiltalign.isValid();
  }

  /**
   * Adds a log file to logFileList and labelList, if the log file is not empty.
   * @param logFileName
   * @param label
   * @param logFileList
   * @param alignLabels
   */
  private void addLogFileTab(final String logFileName, final String label,
      final List<String> logFileList, final List<String> labelList) {
    String name = logFileName + axisID.getExtension() + ".log";
    File log = new File(applicationManager.getPropertyUserDir(), name);
    if (log.length() > 10) {
      logFileList.add(name);
      labelList.add(label);
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String[] manPagelabel = { "Tiltalign", "Xfproduct", "3dmod" };
    String[] manPage = { "tiltalign.html", "xfproduct.html", "3dmod.html" };
    String[] logWindowLabel = { "Align" };

    if (axisID != AxisID.ONLY) {
      logWindowLabel[0] = "Align Axis:" + axisID.getExtension();
    }
    String alignCommandName = logWindowLabel[0];
    // Add tabs
    List<String> logFileList = new ArrayList<String>();
    List<String> alignLabels = new ArrayList<String>();
    addLogFileTab("taRobust", "Robust", logFileList, alignLabels);
    addLogFileTab("taError", "Errors", logFileList, alignLabels);
    addLogFileTab("taSolution", "Solution", logFileList, alignLabels);
    addLogFileTab("taAngles", "Surface Angles", logFileList, alignLabels);
    addLogFileTab("taLocals", "Locals", logFileList, alignLabels);
    addLogFileTab("taResiduals", "Large Residual", logFileList, alignLabels);
    addLogFileTab("taMappings", "Mappings", logFileList, alignLabels);
    addLogFileTab("taCoordinates", "Coordinates", logFileList, alignLabels);
    addLogFileTab("taBeamtilt", "Beam Tilt", logFileList, alignLabels);
    addLogFileTab("align", "Complete Log", logFileList, alignLabels);

    Vector logFile = new Vector(1);
    logFile.add(logFileList.toArray(new String[logFileList.size()]));
    Vector logFileLabel = new Vector(1);
    logFileLabel.add(alignLabels.toArray(new String[alignLabels.size()]));

    TomodataplotsParam.Task[] graph = { TomodataplotsParam.Task.ROTATION,
        TomodataplotsParam.Task.TILT_SKEW, TomodataplotsParam.Task.MAG,
        TomodataplotsParam.Task.XSTRETCH, TomodataplotsParam.Task.RESID,
        TomodataplotsParam.Task.AVER_RESID };

    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "FINAL ALIGNMENT", manPagelabel, manPage, logWindowLabel, logFileLabel, logFile,
        graph, applicationManager, alignCommandName, axisID);
  }

  void done() {
    applicationManager.doneAlignmentEstimationDialog(axisID);
    btnComputeAlignment.removeActionListener(actionListener);
    setDisplayed(false);
  }

  // This is a separate function so it can be called at initialization time
  // as well as from the button action above
  private void updateAdvanced() {
    pnlTiltalign.updateAdvanced(isAdvanced());
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), run3dmodMenuOptions);
  }

  // Event handler for panel buttons
  void buttonAction(String command, final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnComputeAlignment.getActionCommand())) {
      applicationManager.fineAlignment(axisID, btnComputeAlignment, null);
    }
    else if (command.equals(btnView3DModel.getActionCommand())) {
      applicationManager.imodViewModel(axisID, FileType.FIDUCIAL_3D_MODEL);
    }
    else if (command.equals(btnImod.getActionCommand())) {
      applicationManager.imodFixFiducials(axisID, run3dmodMenuOptions, null,
          patchTracking ? ImodProcess.BeadFixerMode.PATCH_TRACKING_RESIDUAL_MODE
              : ImodProcess.BeadFixerMode.RESIDUAL_MODE, null);
    }
    else if (command.equals(btnViewResiduals.getActionCommand())) {
      applicationManager.imodViewResiduals(axisID, run3dmodMenuOptions);
    }
  }

  // ActionListener class for buttons
  class AlignmentEstimationActionListner implements ActionListener {

    AlignmentEstimationDialog adaptee;

    AlignmentEstimationActionListner(AlignmentEstimationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null);
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    btnComputeAlignment.setToolTipText("Run Tiltalign with current parameters.");
    btnImod.setToolTipText("View fiducial model on the image stack in 3dmod.");
    btnView3DModel
        .setToolTipText("View model of solved 3D locations of fiducial points in 3dmodv.");
    btnViewResiduals
        .setToolTipText("Show model of residual vectors (exaggerated 10x) on the image stack.");
  }
}
