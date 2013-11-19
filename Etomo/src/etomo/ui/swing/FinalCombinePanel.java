package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.ConstSetParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.SetParam;
import etomo.process.ImodManager;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.DatasetFiles;

/**
 * <p>
 * Description:
 * 
 * Note: The Y and Z parameters are presented to the user in swapped format,
 * all other representations of those parameters are as they appear in the
 * commands. Specifically Y contains the depth dimension.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 1.4  2011/02/22 18:10:23  sueh
 * bug# 1437 Reformatting.
 *
 * Revision 1.3  2011/02/03 06:22:16  sueh
 * bug# 1422 Control of the processing method has been centralized in the
 * processing method mediator class.  Implementing ProcessInterface.
 * Supplying processes with the current processing method.
 *
 * Revision 1.2  2010/12/05 05:07:39  sueh
 * bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 *
 * Revision 1.1  2010/11/13 16:07:35  sueh
 * bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 *
 * Revision 3.74  2010/02/17 05:03:12  sueh
 * bug# 1301 Using manager instead of manager key for popping up messages.
 *
 * Revision 3.73  2009/09/01 03:18:25  sueh
 * bug# 1222
 *
 * Revision 3.72  2009/03/17 00:46:24  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 3.71  2009/02/04 23:36:48  sueh
 * bug# 1158 Changed id and exception classes in LogFile.
 *
 * Revision 3.70  2009/01/20 20:04:58  sueh
 * bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 *
 * Revision 3.69  2008/09/30 21:01:32  sueh
 * bug# 1113 Using a private constructor in SpacedPanel.
 *
 * Revision 3.68  2008/07/02 18:45:31  sueh
 * bug# 1121 opening objects windows in patch vector model
 *
 * Revision 3.67  2008/05/28 02:49:55  sueh
 * bug# 1111 Add a dialogType parameter to the ProcessSeries
 * constructor.  DialogType must be passed to any function that constructs
 * a ProcessSeries instance.
 *
 * Revision 3.66  2008/05/13 23:02:02  sueh
 * bug# 847 Adding a right click menu for deferred 3dmods to some
 * process buttons.
 *
 * Revision 3.65  2008/05/03 00:49:56  sueh
 * bug# 847 Passing null for ProcessSeries to process funtions.
 *
 * Revision 3.64  2008/03/26 17:15:23  sueh
 * bug# 1003 Deleted characters which a compiler didn't like.  These
 * characters appeared to be spaces.  Don't know what the problem is.
 *
 * Revision 3.63  2007/12/10 22:42:04  sueh
 * bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * getParameters because it is required and has been added to the
 * ProcesschunksParam constructor.  Removed getParameters
 * ProcesschunksParam) because it is empty.
 *
 * Revision 3.62  2007/05/26 00:32:09  sueh
 * bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * Setting button size in UI.
 *
 * Revision 3.61  2007/03/31 03:01:11  sueh
 * bug# 964 Changed PanelHeader.isAdvanceBasicExpanded to isAdvanced.
 *
 * Revision 3.60  2007/03/21 19:45:41  sueh
 * bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * Added AutodocFactory to create Autodoc instances.
 *
 * Revision 3.59  2007/03/01 01:34:30  sueh
 * bug# 964 Added LogFile to Autodoc.
 *
 * Revision 3.58  2007/02/09 00:49:20  sueh
 * bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * classes.
 *
 * Revision 3.57  2006/12/02 04:59:12  sueh
 * bug# 944 Added get/setProcessName ProcesschunksParam so the process
 * being run can be identified.
 *
 * Revision 3.56  2006/11/07 22:36:03  sueh
 * bug# 954 Adding tooltip to cbNoVolcombine.
 *
 * Revision 3.55  2006/10/19 23:34:03  sueh
 * bug# 927 Changed the label for and added a tooltip to ltfLowFromBothRadius.
 *
 * Revision 3.54  2006/10/13 22:30:18  sueh
 * bug# 927 Added ltfLowFromBothRadius.
 *
 * Revision 3.53  2006/09/19 22:36:49  sueh
 * bug# 928 Added btnPatchVectorCCCModel and
 * updatePatchVectorModelDisplay().
 *
 * Revision 3.52  2006/09/13 23:41:48  sueh
 * bug# 921 Added initial Shift X, Y, and Z.  Bug# 924 changed the label for
 * kernel sigma.
 *
 * Revision 3.51  2006/09/05 17:38:27  sueh
 * bug# 924 Improved the label for Kernel Sigma
 *
 * Revision 3.50  2006/08/29 20:09:01  sueh
 * bug# 924 Added kernelSigma checkbox and field.
 *
 * Revision 3.49  2006/08/25 22:52:55  sueh
 * bug# 918 Changed the right click menu for the new version of patchcorr
 *
 * Revision 3.48  2006/07/20 17:20:05  sueh
 * bug# 848 Made UIParameters a singleton.
 *
 * Revision 3.47  2006/06/21 15:53:02  sueh
 * bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 *
 * Revision 3.46  2006/05/12 17:20:47  sueh
 * bug# 861 removed unused code
 *
 * Revision 3.45  2006/04/28 21:00:51  sueh
 * bug# 787 Removed the member variable title, which was not used.
 *
 * Revision 3.44  2006/03/27 21:03:25  sueh
 * bug# 836 Added DialogType to PanelHeader get instances functions so
 * that the buttons in PanelHeader could save themselves.
 *
 * Revision 3.43  2006/03/16 01:55:37  sueh
 * bug# 828 Added isEnabled().  Returns true if the final tab is enabled.
 *
 * Revision 3.42  2006/02/07 00:11:19  sueh
 * bug# 521 Getting the splitcombine process result display from
 * ProcessResultDisplayFactory so that it is always the Restart at
 * Volcombine button.
 *
 * Revision 3.41  2006/01/31 20:58:00  sueh
 * bug# 521 Managing the restart buttons (patchcorr, matchorwarp, and
 * volcombine) in ProcessResultDisplayFactory.
 *
 * Revision 3.40  2006/01/26 22:04:45  sueh
 * bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * the buttons turned on each they are run, unless the process fails or is
 * killed.
 *
 * Revision 3.39  2006/01/12 17:10:14  sueh
 * bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 *
 * Revision 3.38  2006/01/03 23:37:50  sueh
 * bug# 675 Converted JCheckBox's to CheckBox
 *
 * Revision 3.37  2005/12/14 20:56:48  sueh
 * bug# 784 Added tool tips.
 *
 * Revision 3.36  2005/11/21 20:46:42  sueh
 * bug# 772 Disabling the parallel process checkbox when the cpu.adoc is
 * missing.  Copy parallel process checkbox's enabled setting from the
 * Setup to the Final tab.
 *
 * Revision 3.35  2005/11/14 22:05:50  sueh
 * bug# 762 Made buttonAction() protected.
 *
 * Revision 3.34  2005/10/15 00:33:34  sueh
 * bug# 532 Standardized is and set parallel processing checkbox functions
 * to setParallel() and isParallel().
 *
 * Revision 3.33  2005/10/13 22:35:17  sueh
 * bug# 532 parallel process check box and no volcombine check box are on
 * both setup and final now.  Getting the text for no volcombine from final.
 * Getting the text for parallel process from Tomo Gen dialog.
 *
 * Revision 3.32  2005/10/12 22:44:39  sueh
 * bug# 532 If parallel is not set in meta data, then the default for the parallel
 * checkboxes is based on the existance and validity of cpu.adoc.
 *
 * Revision 3.31  2005/09/29 18:52:54  sueh
 * bug# 532 Add panel headers to all of the sections in Combine.  Hide the
 * sections in the tabs that are not visible so that the visible tab can become
 * small.  Added an expand() function to each tab to handle the
 * expand/contract requests of the panel header buttons.  Added set and get
 * parameters for ReconScreenState to set and get the state of the panel
 * headers.
 *
 * Revision 3.30  2005/09/22 21:02:45  sueh
 * bug# 532 Added maxCPUs to the parallel processing checkbox.
 *
 * Revision 3.29  2005/09/21 16:37:39  sueh
 * bug# 532 Removed all resume functionality from the dialogs.
 *
 * Revision 3.28  2005/09/16 20:56:15  sueh
 * bug# 532 Moved call to resetParallelPanel() to
 * ApplicationManager.processchunks().  Added resetParallelPanel() to
 * ParallelDialog.
 *
 * Revision 3.27  2005/09/16 18:09:54  sueh
 * bug# 532 Added cbParallelProcess and resume.  Added functions:
 * resume, getParameters(MetaData),
 * getParameters(ProcesschunksParam), isParallelProcessSelected,
 * runVolcombine, and setParameters(ConstMetaData).
 *
 * Revision 3.26  2005/08/27 22:36:53  sueh
 * bug# 532 Changed Autodoc.get() to getInstance().
 *
 * Revision 3.25  2005/08/11 23:52:08  sueh
 * bug# 711  Change enum Run3dmodMenuOption to
 * Run3dmodMenuOptions, which can turn on multiple options at once.
 * This allows ImodState to combine input from the context menu and the
 * pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * the action function.
 *
 * Revision 3.24  2005/08/09 20:22:37  sueh
 * bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * MultiLineButton from JButton.
 *
 * Revision 3.23  2005/04/25 21:06:01  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.
 *
 * Revision 3.22  2005/02/11 16:45:39  sueh
 * bug# 600 Getting tooltips using EtomoAutodoc instead of TooltipFormatter.
 *
 * Revision 3.21  2005/01/26 00:02:33  sueh
 * Removing ConstEtomoNumber.displayDefault.  To get the default to
 * display, set displayValue and default the same.
 *
 * Revision 3.20  2004/12/03 20:23:18  sueh
 * bug# 556 Support older versions of volcombine.com.  Since the set param
 * may be missing or setting the wrong name, check it before loading or
 * unloading ReductionFactor text.  Added enableReductionFactor() to
 * disable ReductionFactor when the set param is missing or invalid.
 *
 * Revision 3.19  2004/12/02 20:39:41  sueh
 * bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * the join guide.  Need to specify the guide to anchor.
 *
 * Revision 3.18  2004/11/30 18:29:36  sueh
 * bug# 556 Made Reduction Factor advanced and gave it a tooltip.
 *
 * Revision 3.17  2004/11/30 00:35:49  sueh
 * bug# 556 Adding reduction factor and get and set params for volcombine.
 *
 * Revision 3.16  2004/11/19 23:54:04  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.15.2.1  2004/10/11 02:13:15  sueh
 * bug# 520 Passed the manager to the ContextPopup object in order to get
 * the propertyUserDir.
 *
 * Revision 3.15  2004/08/28 01:08:28  sueh
 * bug# 507 changed the wording of the don't run volcombine
 * checkbox
 *
 * Revision 3.14  2004/08/19 02:48:33  sueh
 * bug# 508 Added a way to set the don't run volcombine checkbox
 * Added:
 * setRunVolcombine(boolean runVolcombine)
 *
 * Revision 3.13  2004/07/21 00:19:15  sueh
 * bug# 507 added Don't run volcombine checkbox, non-persistant
 *
 * Revision 3.12  2004/06/25 23:24:25  sueh
 * bug# 485 also set patch region model checkbox based on
 * matchorwarp
 *
 * Revision 3.11  2004/06/17 21:47:17  sueh
 * bug# 474 UIUtilities.setButtonSizeAll() causes this, called
 * UIUtilities.setButtonSize(AbstractButton), since there is only
 * one button on the panel in this case.
 *
 * Revision 3.10  2004/06/17 00:31:44  sueh
 * adding call to setToolTipText()
 *
 * Revision 3.9  2004/06/14 23:39:53  rickg
 * Bug #383 Transitioned to using solvematch
 *
 * Revision 3.8  2004/06/13 17:03:23  rickg
 * Solvematch mid change
 *
 * Revision 3.7  2004/05/11 20:50:01  sueh
 * bug# 302 adding FinalCombineValues interface
 * standardizing synchronization
 *
 * Revision 3.6  2004/03/22 23:20:56  sueh
 * bug# 250 Use Patch Region Model checkbox should be set from Patchcorr, not
 * Matchorwarp
 *
 * Revision 3.5  2004/03/09 01:57:23  sueh
 * bug# 381 added Restart at Volcombine button
 *
 * Revision 3.4  2004/03/06 03:48:05  sueh
 * bug# 380 added Use linear interpolation checkbox (advanced)
 *
 * Revision 3.3  2004/03/05 18:19:29  sueh
 * bug# 250 added getCombineParameters() - retrieve parameters found in
 * Combine from final tab
 *
 * Revision 3.2  2004/03/02 21:54:24  sueh
 * bug# 250 setting useBoudaryModel() with Use Patch Region Model
 *
 * Revision 3.1  2004/01/30 22:45:07  sueh
 * bug# 356 Changing buttons with html labels to
 * MultiLineButton and MultiLineToggleButton
 *
 * Revision 3.0  2003/11/07 23:19:01  rickg
 * Version 1.0.0
 *
 * Revision 1.20  2003/10/29 18:05:21  rickg
 * Bug# 304 Tooltips
 *
 * Revision 1.19  2003/10/23 17:09:41  rickg
 * Bug# 323 Label change
 *
 * Revision 1.18  2003/10/21 23:44:30  rickg
 * Changed patch region model button ot a multiline button
 *
 * Revision 1.17  2003/10/17 15:44:24  rickg
 * Bug# 303 Re-layout of UI items
 * Get button sizes from UIParameters static object
 * Revision 1.16 2003/10/16 21:03:35 rickg
 * Bug# 303 Label changes and re-layout
 * 
 * <p>
 * Revision 1.15 2003/10/15 22:45:40 rickg
 * <p>
 * Button size change
 * <p>
 * <p>
 * Revision 1.14 2003/06/05 04:41:31 rickg
 * <p>
 * Label change
 * <p>
 * <p>
 * Revision 1.13 2003/05/15 04:28:48 rickg
 * <p>
 * Removed test button for volcombine
 * <p>
 * <p>
 * Revision 1.12 2003/05/14 14:38:08 rickg
 * <p>
 * Temporary button
 * <p>
 * <p>
 * Revision 1.11 2003/04/09 23:37:46 rickg
 * <p>
 * Moved CheckBoxTextPanel out
 * <p>
 * <p>
 * Revision 1.10 2003/03/26 00:52:56 rickg
 * <p>
 * Added button to convert patch_vector.mod to patch.out
 * <p>
 * <p>
 * Revision 1.9 2003/03/20 17:57:10 rickg
 * <p>
 * Fixed combined volume button size
 * <p>
 * <p>
 * </p>
 */
public class FinalCombinePanel implements ContextMenu, FinalCombineFields,
    Run3dmodButtonContainer, Expandable {
  public static final String rcsid = "$Id$";

  static final String NO_VOLCOMBINE_TITLE = "Stop before running volcombine";
  static final String VOLCOMBINE_PARALLEL_PROCESSING_TOOL_TIP = "Check to distribute the volcombine process across multiple computers.";
  private static final String KERNEL_SIGMA_LABEL = "Kernel filtering with sigma: ";

  private TomogramCombinationDialog tomogramCombinationDialog;
  private ApplicationManager applicationManager;

  private JPanel pnlRoot = new JPanel();

  private EtomoPanel pnlPatchcorr = new EtomoPanel();
  private SpacedPanel pnlPatchcorrBody = SpacedPanel.getInstance(true);

  private JPanel pnlPatchsize = new JPanel();
  private JPanel pnlPatchsizeEdit = new JPanel();
  private LabeledTextField ltfXPatchSize = new LabeledTextField(FieldType.INTEGER,
      "X patch size :");
  private LabeledTextField ltfYPatchSize = new LabeledTextField(FieldType.INTEGER,
      "Z patch size :");
  private LabeledTextField ltfZPatchSize = new LabeledTextField(FieldType.INTEGER,
      "Y patch size :");
  private JPanel pnlPatchsizeButtons = new JPanel();
  private MultiLineButton btnPatchsizeIncrease = new MultiLineButton(
      "<html><b>Patch Size +20%</b>");
  private MultiLineButton btnPatchsizeDecrease = new MultiLineButton(
      "<html><b>Patch Size -20%</b>");

  private LabeledTextField ltfXNPatches = new LabeledTextField(FieldType.INTEGER,
      "Number of X patches :");
  private LabeledTextField ltfYNPatches = new LabeledTextField(FieldType.INTEGER,
      "Number of Z patches :");
  private LabeledTextField ltfZNPatches = new LabeledTextField(FieldType.INTEGER,
      "Number of Y patches :");
  private CheckBox cbKernelSigma = new CheckBox(KERNEL_SIGMA_LABEL);
  private TextField tfKernelSigma = new TextField(FieldType.FLOATING_POINT,
      KERNEL_SIGMA_LABEL, null);

  private final JPanel pnlBoundary = new JPanel();
  private LabeledTextField ltfXLow = new LabeledTextField(FieldType.INTEGER, "X Low :");
  private LabeledTextField ltfXHigh = new LabeledTextField(FieldType.INTEGER, "X high :");
  private LabeledTextField ltfYLow = new LabeledTextField(FieldType.INTEGER, "Z Low :");
  private LabeledTextField ltfYHigh = new LabeledTextField(FieldType.INTEGER, "Z high :");
  private LabeledTextField ltfZLow = new LabeledTextField(FieldType.INTEGER, "Y Low :");
  private LabeledTextField ltfZHigh = new LabeledTextField(FieldType.INTEGER, "Y high :");
  private final ButtonActionListener actionListener = new ButtonActionListener(this);

  private final Run3dmodButton btnPatchcorrRestart;

  private EtomoPanel pnlMatchorwarp = new EtomoPanel();
  private JPanel pnlMatchorwarpBody = new JPanel();
  private EtomoPanel pnlPatchRegionModel = new EtomoPanel();
  private SpacedPanel pnlPatchRegionModelBody = SpacedPanel.getInstance(true);
  private CheckBox cbUsePatchRegionModel = new CheckBox("Use patch region model");
  private Run3dmodButton btnPatchRegionModel = Run3dmodButton.get3dmodInstance(
      "Create/Edit Patch Region Model", this);
  private LabeledTextField ltfWarpLimit = new LabeledTextField(FieldType.STRING,
      "Warping residual limits: ");
  private LabeledTextField ltfRefineLimit = new LabeledTextField(
      FieldType.FLOATING_POINT, "Residual limit for single transform: ");

  private LabeledTextField ltfXLowerExclude = new LabeledTextField(FieldType.INTEGER,
      "Number of columns to exclude on left (in X): ");
  private LabeledTextField ltfXUpperExclude = new LabeledTextField(FieldType.INTEGER,
      "Number of columns to exclude on right (in X): ");
  private LabeledTextField ltfZLowerExclude = new LabeledTextField(FieldType.INTEGER,
      "Number of rows to exclude on bottom (in Y): ");
  private LabeledTextField ltfZUpperExclude = new LabeledTextField(FieldType.INTEGER,
      "Number of rows to exclude on top (in Y): ");
  private CheckBox cbUseLinearInterpolation = new CheckBox("Use linear interpolation");
  private JPanel pnlMatchorwarpButtons = new JPanel();
  private final Run3dmodButton btnMatchorwarpRestart;
  private MultiLineButton btnMatchorwarpTrial = new MultiLineButton(
      "<html><b>Matchorwarp Trial Run</b>");
  private EtomoPanel pnlVolcombine = new EtomoPanel();
  private JPanel pnlVolcombineBody = new JPanel();
  private final Run3dmodButton btnVolcombineRestart;
  private JPanel pnlButton = new JPanel();
  private MultiLineButton btnPatchVectorModel = new MultiLineButton(
      "<html><b>Examine Patch Vector Model</b>");
  private MultiLineButton btnReplacePatchOut = new MultiLineButton(
      "<html><b>Replace Patch Vectors</b>");
  private Run3dmodButton btnImodMatchedTo = Run3dmodButton.get3dmodInstance(
      "Open Volume Being Matched To", this);
  private final Run3dmodButton btnImodCombined = Run3dmodButton.get3dmodInstance(
      "Open Combined Volume", this);
  private CheckBox cbNoVolcombine = new CheckBox(NO_VOLCOMBINE_TITLE);
  private LabeledTextField ltfReductionFactor = new LabeledTextField(
      FieldType.FLOATING_POINT,
      "Reduction factor for matching amplitudes in combined FFT: ");
  private LabeledTextField ltfLowFromBothRadius = new LabeledTextField(
      FieldType.FLOATING_POINT,
      "Radius below which to average components from both tomograms: ");
  private CheckBox cbParallelProcess;
  private final PanelHeader patchRegionModelHeader;
  private final PanelHeader patchcorrHeader;
  private final PanelHeader matchorwarpHeader;
  private final PanelHeader volcombineHeader;
  private final SpacedPanel pnlKernelSigma = SpacedPanel.getInstance();
  private final LabeledTextField ltfInitialShiftX = new LabeledTextField(
      FieldType.FLOATING_POINT, "Initial shift in X:");
  private final LabeledTextField ltfInitialShiftY = new LabeledTextField(
      FieldType.FLOATING_POINT, "Z:");
  private final LabeledTextField ltfInitialShiftZ = new LabeledTextField(
      FieldType.FLOATING_POINT, "Y:");
  private final SpacedPanel pnlInitialShiftXYZ = SpacedPanel.getInstance();
  private MultiLineButton btnPatchVectorCCCModel = new MultiLineButton(
      "Open Vector Model with Correlations");
  private final DialogType dialogType;

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]\n";
  }

  protected String paramString() {
    return "ltfXPatchSize=" + ltfXPatchSize + ",\nltfYPatchSize=" + ltfYPatchSize
        + ",\nltfZPatchSize=" + ltfZPatchSize + ",\nltfXNPatches=" + ltfXNPatches
        + ",\nltfYNPatches=" + ltfYNPatches + ",\nltfZNPatches=" + ltfZNPatches
        + ",\nltfXLow=" + ltfXLow + ",\nltfXHigh=" + ltfXHigh + ",\nltfYLow=" + ltfYLow
        + ",\nltfYHigh=" + ltfYHigh + ",\nltfZLow=" + ltfZLow + ",\nltfZHigh=" + ltfZHigh
        + ",\ncbUsePatchRegionModel=" + cbUsePatchRegionModel + ",\nltfWarpLimit="
        + ltfWarpLimit + ",\nltfRefineLimit=" + ltfRefineLimit + ",\nltfXLowerExclude="
        + ltfXLowerExclude + ",\nltfXUpperExclude=" + ltfXUpperExclude
        + ",\nltfZLowerExclude=" + ltfZLowerExclude + ",\nltfZUpperExclude="
        + ltfZUpperExclude + ",\ncbUseLinearInterpolation=" + cbUseLinearInterpolation
        + ",\ncbNoVolcombine=" + cbNoVolcombine + ",\nltfReductionFactor="
        + ltfReductionFactor + ",\ncbParallelProcess=" + cbParallelProcess;
  }

  /**
   * Default constructor
   * 
   * @param appMgr
   */
  public FinalCombinePanel(TomogramCombinationDialog parent, ApplicationManager appMgr,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    this.dialogType = dialogType;
    tomogramCombinationDialog = parent;
    btnPatchcorrRestart = (Run3dmodButton) appMgr.getProcessResultDisplayFactory(
        AxisID.ONLY).getRestartPatchcorr();
    btnPatchcorrRestart.setContainer(this);
    btnPatchcorrRestart.setDeferred3dmodButton(btnImodCombined);
    btnMatchorwarpRestart = (Run3dmodButton) appMgr.getProcessResultDisplayFactory(
        AxisID.ONLY).getRestartMatchorwarp();
    btnMatchorwarpRestart.setContainer(this);
    btnMatchorwarpRestart.setDeferred3dmodButton(btnImodCombined);
    btnVolcombineRestart = (Run3dmodButton) appMgr.getProcessResultDisplayFactory(
        AxisID.ONLY).getRestartVolcombine();
    btnVolcombineRestart.setContainer(this);
    btnVolcombineRestart.setDeferred3dmodButton(btnImodCombined);
    applicationManager = appMgr;
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));

    // Layout Patch region model panel
    pnlPatchRegionModelBody.setBoxLayout(BoxLayout.X_AXIS);
    pnlPatchRegionModelBody.add(cbUsePatchRegionModel);
    btnPatchRegionModel.setSize();
    pnlPatchRegionModelBody.add(btnPatchRegionModel);
    pnlPatchRegionModelBody.addHorizontalGlue();
    // btnPatchRegionModel.setSize();

    pnlPatchRegionModel.setLayout(new BoxLayout(pnlPatchRegionModel, BoxLayout.Y_AXIS));
    pnlPatchRegionModel.setBorder(BorderFactory.createEtchedBorder());
    patchRegionModelHeader = PanelHeader.getInstance("Patch Region Model", this,
        dialogType);
    pnlPatchRegionModel.add(patchRegionModelHeader);
    pnlPatchRegionModel.add(pnlPatchRegionModelBody.getContainer());

    // Layout the Patchcorr panel
    pnlPatchcorrBody.setBoxLayout(BoxLayout.Y_AXIS);

    pnlPatchsizeButtons.setLayout(new BoxLayout(pnlPatchsizeButtons, BoxLayout.Y_AXIS));
    pnlPatchsizeButtons.add(btnPatchsizeIncrease.getComponent());
    pnlPatchsizeButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeButtons.add(btnPatchsizeDecrease.getComponent());
    UIUtilities.setButtonSizeAll(pnlPatchsizeButtons,
        UIParameters.INSTANCE.getButtonDimension());

    pnlPatchsizeEdit.setLayout(new BoxLayout(pnlPatchsizeEdit, BoxLayout.Y_AXIS));

    pnlPatchsizeEdit.add(ltfXPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfZPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfYPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlPatchsize.setLayout(new BoxLayout(pnlPatchsize, BoxLayout.X_AXIS));
    pnlPatchsize.add(pnlPatchsizeEdit);
    pnlPatchsize.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPatchsize.add(pnlPatchsizeButtons);
    pnlPatchcorrBody.add(pnlPatchsize);

    pnlBoundary.setLayout(new GridLayout(3, 3, 5, 5));
    pnlBoundary.add(ltfXNPatches.getContainer());
    pnlBoundary.add(ltfXLow.getContainer());
    pnlBoundary.add(ltfXHigh.getContainer());
    pnlBoundary.add(ltfZNPatches.getContainer());
    pnlBoundary.add(ltfZLow.getContainer());
    pnlBoundary.add(ltfZHigh.getContainer());
    pnlBoundary.add(ltfYNPatches.getContainer());
    pnlBoundary.add(ltfYLow.getContainer());
    pnlBoundary.add(ltfYHigh.getContainer());
    pnlPatchcorrBody.add(pnlBoundary);

    pnlInitialShiftXYZ.setBoxLayout(BoxLayout.X_AXIS);
    pnlInitialShiftXYZ.add(ltfInitialShiftX);
    pnlInitialShiftXYZ.add(ltfInitialShiftZ);
    pnlInitialShiftXYZ.add(ltfInitialShiftY);
    pnlPatchcorrBody.add(pnlInitialShiftXYZ);

    pnlKernelSigma.setBoxLayout(BoxLayout.X_AXIS);
    pnlKernelSigma.add(cbKernelSigma);
    pnlKernelSigma.add(tfKernelSigma);
    tfKernelSigma.setEnabled(false);
    pnlPatchcorrBody.add(pnlKernelSigma);

    btnPatchcorrRestart.setAlignmentX(Component.CENTER_ALIGNMENT);
    JPanel pnlPatchcorrButtons = new JPanel();
    pnlPatchcorrButtons.setLayout(new BoxLayout(pnlPatchcorrButtons, BoxLayout.X_AXIS));
    btnPatchcorrRestart.setSize();
    btnPatchVectorCCCModel.setSize();
    pnlPatchcorrButtons.add(Box.createHorizontalGlue());
    pnlPatchcorrButtons.add(btnPatchcorrRestart.getComponent());
    pnlPatchcorrButtons.add(Box.createHorizontalGlue());
    pnlPatchcorrButtons.add(btnPatchVectorCCCModel.getComponent());
    pnlPatchcorrButtons.add(Box.createHorizontalGlue());
    pnlPatchcorrBody.add(pnlPatchcorrButtons);

    pnlPatchsizeButtons.setLayout(new BoxLayout(pnlPatchsizeButtons, BoxLayout.Y_AXIS));

    pnlPatchcorr.setLayout(new BoxLayout(pnlPatchcorr, BoxLayout.Y_AXIS));
    pnlPatchcorr.setBorder(BorderFactory.createEtchedBorder());
    patchcorrHeader = PanelHeader.getAdvancedBasicInstance("Patchcorr Parameters", this,
        dialogType, globalAdvancedButton);
    pnlPatchcorr.add(patchcorrHeader);
    pnlPatchcorr.add(pnlPatchcorrBody.getContainer());

    // Layout the Matchorwarp panel
    pnlMatchorwarpBody.setLayout(new BoxLayout(pnlMatchorwarpBody, BoxLayout.Y_AXIS));

    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlMatchorwarpBody.add(ltfRefineLimit.getContainer());
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlMatchorwarpBody.add(ltfWarpLimit.getContainer());
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlMatchorwarpBody.add(ltfXLowerExclude.getContainer());
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarpBody.add(ltfXUpperExclude.getContainer());
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarpBody.add(ltfZLowerExclude.getContainer());
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarpBody.add(ltfZUpperExclude.getContainer());
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarpBody.add(cbUseLinearInterpolation);
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlMatchorwarpButtons
        .setLayout(new BoxLayout(pnlMatchorwarpButtons, BoxLayout.X_AXIS));
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpRestart.getComponent());
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpTrial.getComponent());
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlMatchorwarpButtons,
        UIParameters.INSTANCE.getButtonDimension());

    pnlMatchorwarpBody.add(pnlMatchorwarpButtons);
    pnlMatchorwarpBody.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlMatchorwarp.setLayout(new BoxLayout(pnlMatchorwarp, BoxLayout.Y_AXIS));
    pnlMatchorwarp.setBorder(BorderFactory.createEtchedBorder());
    matchorwarpHeader = PanelHeader.getAdvancedBasicInstance("Matchorwarp Parameters",
        this, dialogType, globalAdvancedButton);
    pnlMatchorwarp.add(matchorwarpHeader);
    pnlMatchorwarp.add(pnlMatchorwarpBody);

    pnlVolcombineBody.setLayout(new BoxLayout(pnlVolcombineBody, BoxLayout.Y_AXIS));
    cbParallelProcess = new CheckBox(
        tomogramCombinationDialog.parallelProcessCheckBoxText);
    JPanel pnlParallelProcess = new JPanel();
    pnlParallelProcess.setLayout(new BoxLayout(pnlParallelProcess, BoxLayout.X_AXIS));
    pnlParallelProcess.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlParallelProcess.add(cbParallelProcess);
    pnlParallelProcess.add(Box.createHorizontalGlue());
    pnlVolcombineBody.add(pnlParallelProcess);
    JPanel pnlNoVolcombine = new JPanel();
    pnlNoVolcombine.setLayout(new BoxLayout(pnlNoVolcombine, BoxLayout.X_AXIS));
    pnlNoVolcombine.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlNoVolcombine.add(cbNoVolcombine);
    pnlNoVolcombine.add(Box.createHorizontalGlue());
    pnlVolcombineBody.add(pnlNoVolcombine);
    pnlVolcombineBody.add(ltfReductionFactor.getContainer());
    pnlVolcombineBody.add(ltfLowFromBothRadius.getContainer());
    pnlVolcombineBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlVolcombineBody.add(btnVolcombineRestart.getComponent());
    cbNoVolcombine.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnVolcombineRestart.setAlignmentX(Component.CENTER_ALIGNMENT);
    UIUtilities.setButtonSizeAll(pnlVolcombineBody,
        UIParameters.INSTANCE.getButtonDimension());
    UIUtilities.alignComponentsX(pnlVolcombineBody, Component.CENTER_ALIGNMENT);

    pnlVolcombine.setLayout(new BoxLayout(pnlVolcombine, BoxLayout.Y_AXIS));
    pnlVolcombine.setBorder(BorderFactory.createEtchedBorder());
    volcombineHeader = PanelHeader.getAdvancedBasicInstance("Volcombine Parameters",
        this, dialogType, globalAdvancedButton);
    pnlVolcombine.add(volcombineHeader);
    pnlVolcombine.add(pnlVolcombineBody);

    // Create the button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnPatchVectorModel.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnReplacePatchOut.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodMatchedTo.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodCombined.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlButton, UIParameters.INSTANCE.getButtonDimension());

    // Root panel layout
    pnlRoot.add(pnlPatchRegionModel);
    pnlRoot.add(pnlPatchcorr);
    pnlRoot.add(pnlMatchorwarp);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(pnlVolcombine);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(Box.createVerticalGlue());
    pnlRoot.add(pnlButton);

    // Bind the buttons to action listener
    btnPatchcorrRestart.addActionListener(actionListener);
    btnPatchsizeIncrease.addActionListener(actionListener);
    btnPatchsizeDecrease.addActionListener(actionListener);
    btnPatchRegionModel.addActionListener(actionListener);
    btnMatchorwarpRestart.addActionListener(actionListener);
    btnMatchorwarpTrial.addActionListener(actionListener);
    btnVolcombineRestart.addActionListener(actionListener);
    btnPatchVectorModel.addActionListener(actionListener);
    btnReplacePatchOut.addActionListener(actionListener);
    btnImodMatchedTo.addActionListener(actionListener);
    btnImodCombined.addActionListener(actionListener);
    cbParallelProcess.addActionListener(actionListener);
    cbKernelSigma.addActionListener(actionListener);
    btnPatchVectorCCCModel.addActionListener(actionListener);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
    setToolTipText();
  }

  void removeListeners() {
    btnPatchcorrRestart.removeActionListener(actionListener);
    btnMatchorwarpRestart.removeActionListener(actionListener);
    btnVolcombineRestart.removeActionListener(actionListener);
  }

  void updateAdvanced(boolean state) {
    updateAdvancedPatchcorr(state);
    updateAdvancedMatchorwarp(state);
    updateAdvancedVolcombine(state);
  }

  final void updateAdvancedPatchcorr(boolean state) {
    pnlBoundary.setVisible(state);
    pnlInitialShiftXYZ.setVisible(state);
    pnlKernelSigma.setVisible(state);
  }

  final void updateAdvancedMatchorwarp(boolean state) {
    ltfRefineLimit.setVisible(state);
    cbUseLinearInterpolation.setVisible(state);
  }

  final void updateAdvancedVolcombine(boolean state) {
    ltfReductionFactor.setVisible(state);
    ltfLowFromBothRadius.setVisible(state);
  }

  ProcessResultDisplay getPatchcorrProcessResultDisplay() {
    return btnPatchcorrRestart;
  }

  Run3dmodButton getImodCombinedButton() {
    return btnImodCombined;
  }

  ProcessResultDisplay getMatchorwarpProcessResultDisplay() {
    return btnMatchorwarpRestart;
  }

  ProcessResultDisplay getVolcombineProcessResultDisplay() {
    return btnVolcombineRestart;
  }

  /**
   * Return the pnlRoot reference
   * 
   * @return Container
   */
  public Container getContainer() {
    return pnlRoot;
  }

  public void setUsePatchRegionModel(boolean usePatchRegionModel) {
    cbUsePatchRegionModel.setSelected(usePatchRegionModel);
  }

  public boolean isUsePatchRegionModel() {
    return cbUsePatchRegionModel.isSelected();
  }

  public boolean isParallel() {
    return cbParallelProcess.isSelected();
  }

  public boolean isParallelEnabled() {
    return cbParallelProcess.isEnabled();
  }

  public void setXMin(String xMin) {
    ltfXLow.setText(xMin);
  }

  public String getXMin() {
    return ltfXLow.getText();
  }

  public void setXMax(String xMax) {
    ltfXHigh.setText(xMax);
  }

  public boolean isEnabled() {
    return tomogramCombinationDialog.isTabEnabled(TomogramCombinationDialog.lblFinal);
  }

  public String getXMax() {
    return ltfXHigh.getText();
  }

  public void setYMin(String yMin) {
    ltfZLow.setText(yMin);
  }

  public String getYMin() {
    return ltfZLow.getText();
  }

  public void setYMax(String yMax) {
    ltfZHigh.setText(yMax);
  }

  public String getYMax() {
    return ltfZHigh.getText();
  }

  public void setZMin(String zMin) {
    ltfYLow.setText(zMin);
  }

  public String getZMin() {
    return ltfYLow.getText();
  }

  public void setZMax(String zMax) {
    ltfYHigh.setText(zMax);
  }

  public String getZMax() {
    return ltfYHigh.getText();
  }

  boolean isRunVolcombine() {
    return !cbNoVolcombine.isSelected();
  }

  void setRunVolcombine(boolean runVolcombine) {
    cbNoVolcombine.setSelected(!runVolcombine);
  }

  final void setParameters(ReconScreenState screenState) {
    patchRegionModelHeader.setState(screenState.getCombineFinalPatchRegionHeaderState());
    patchcorrHeader.setState(screenState.getCombineFinalPatchcorrHeaderState());
    matchorwarpHeader.setState(screenState.getCombineFinalPatchcorrHeaderState());
    volcombineHeader.setState(screenState.getCombineFinalVolcombineHeaderState());
    btnPatchcorrRestart.setButtonState(screenState.getButtonState(btnPatchcorrRestart
        .getButtonStateKey()));
    btnMatchorwarpRestart.setButtonState(screenState.getButtonState(btnMatchorwarpRestart
        .getButtonStateKey()));
    btnVolcombineRestart.setButtonState(screenState.getButtonState(btnVolcombineRestart
        .getButtonStateKey()));
    // if the kernal sigma value isn't coming from the comscript, get it from the
    // .edf, if it exists
    if (!cbKernelSigma.isSelected()) {
      ConstEtomoNumber kernelSigma = screenState.getPatchcorrKernelSigma();
      if (kernelSigma != null) {
        tfKernelSigma.setText(kernelSigma.toString());
      }
    }
  }

  final void getParameters(ReconScreenState screenState) {
    patchRegionModelHeader.getState(screenState.getCombineFinalPatchRegionHeaderState());
    patchcorrHeader.getState(screenState.getCombineFinalPatchcorrHeaderState());
    matchorwarpHeader.getState(screenState.getCombineFinalPatchcorrHeaderState());
    volcombineHeader.getState(screenState.getCombineFinalVolcombineHeaderState());
    screenState.setPatchcorrKernelSigma(tfKernelSigma.getText());
  }

  final void setVisible(boolean visible) {
    pnlPatchRegionModel.setVisible(visible);
    pnlPatchcorr.setVisible(visible);
    pnlMatchorwarp.setVisible(visible);
    pnlVolcombine.setVisible(visible);
    updatePatchVectorModelDisplay();
  }

  public void expand(GlobalExpandButton button) {

  }

  public void expand(ExpandButton button) {
    if (patchRegionModelHeader.equalsOpenClose(button)) {
      pnlPatchRegionModelBody.setVisible(button.isExpanded());
    }
    else if (patchcorrHeader.equalsOpenClose(button)) {
      pnlPatchcorrBody.setVisible(button.isExpanded());
    }
    else if (patchcorrHeader.equalsAdvancedBasic(button)) {
      updateAdvancedPatchcorr(button.isExpanded());
    }
    else if (matchorwarpHeader.equalsOpenClose(button)) {
      pnlMatchorwarpBody.setVisible(button.isExpanded());
    }
    else if (matchorwarpHeader.equalsAdvancedBasic(button)) {
      updateAdvancedMatchorwarp(button.isExpanded());
    }
    else if (volcombineHeader.equalsOpenClose(button)) {
      pnlVolcombineBody.setVisible(button.isExpanded());
    }
    else if (volcombineHeader.equalsAdvancedBasic(button)) {
      updateAdvancedVolcombine(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(AxisID.ONLY, applicationManager);
  }

  final String getVolcombineButtonName() {
    return ProcessName.VOLCOMBINE.toString();
  }

  /**
   * Set the values of the patchcrawl3D UI objects from the
   * ConstPatchcrawl3DParam object.
   * 
   * @param patchrawlParam
   */
  public void setPatchcrawl3DParams(ConstPatchcrawl3DParam patchrawlParam) {
    cbUsePatchRegionModel.setSelected(patchrawlParam.isUseBoundaryModel());
    ltfXPatchSize.setText(patchrawlParam.getXPatchSize());
    ltfYPatchSize.setText(patchrawlParam.getYPatchSize());
    ltfZPatchSize.setText(patchrawlParam.getZPatchSize());
    ltfXNPatches.setText(patchrawlParam.getNX());
    ltfYNPatches.setText(patchrawlParam.getNY());
    ltfZNPatches.setText(patchrawlParam.getNZ());
    ltfXLow.setText(patchrawlParam.getXLow());
    ltfXHigh.setText(patchrawlParam.getXHigh());
    ltfYLow.setText(patchrawlParam.getYLow());
    ltfYHigh.setText(patchrawlParam.getYHigh());
    ltfZLow.setText(patchrawlParam.getZLow());
    ltfZHigh.setText(patchrawlParam.getZHigh());
    ltfInitialShiftX.setText(patchrawlParam.getInitialShiftX());
    ltfInitialShiftY.setText(patchrawlParam.getInitialShiftY());
    ltfInitialShiftZ.setText(patchrawlParam.getInitialShiftZ());
    cbKernelSigma.setSelected(patchrawlParam.isKernelSigmaActive());
    tfKernelSigma.setText(patchrawlParam.getKernelSigma().toString());
    updateKernelSigma();
  }

  void setReductionFactorParams(ConstSetParam setParam) {
    if (setParam == null || !setParam.isValid()) {
      return;
    }
    ltfReductionFactor.setText(setParam.getValue());
  }

  void setLowFromBothRadiusParams(ConstSetParam setParam) {
    if (setParam == null || !setParam.isValid()) {
      return;
    }
    ltfLowFromBothRadius.setText(setParam.getValue());
  }

  public final void setNoVolcombine(boolean noVolcombine) {
    cbNoVolcombine.setSelected(noVolcombine);
  }

  public final boolean isNoVolcombine() {
    return cbNoVolcombine.isSelected();
  }

  public final void setParallel(boolean parallel) {
    cbParallelProcess.setSelected(parallel);
    // Used for synchronization - don't send message to mediator
  }

  public final void setParallelEnabled(boolean parallelEnabled) {
    cbParallelProcess.setEnabled(parallelEnabled);
  }

  boolean getReductionFactorParam(SetParam param, final boolean doValidation) {
    if (param == null) {
      return false;
    }
    try {
      param.setValue(ltfReductionFactor.getText(doValidation));
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  boolean getLowFromBothRadiusParam(SetParam param, final boolean doValidation) {
    if (param == null) {
      return false;
    }
    try {
      param.setValue(ltfLowFromBothRadius.getText(doValidation));
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void enableReductionFactor(boolean enable) {
    ltfReductionFactor.setEnabled(enable);
  }

  void enableLowFromBothRadius(boolean enable) {
    ltfLowFromBothRadius.setEnabled(enable);
  }

  /**
   * Set the Patchcrawl3DParam object values from the UI values.
   * 
   * @param patchrawlParam
   * @throws NumberFormatException
   */
  public boolean getPatchcrawl3DParams(final Patchcrawl3DParam patchcrawl3DParam,
      final boolean doValidation) throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = cbUsePatchRegionModel.getText();
      patchcrawl3DParam.setUseBoundaryModel(cbUsePatchRegionModel.isSelected());
      badParameter = ltfXPatchSize.getLabel();
      patchcrawl3DParam.setXPatchSize(Integer.parseInt(ltfXPatchSize
          .getText(doValidation)));
      badParameter = ltfYPatchSize.getLabel();
      patchcrawl3DParam.setYPatchSize(Integer.parseInt(ltfYPatchSize
          .getText(doValidation)));
      badParameter = ltfZPatchSize.getLabel();
      patchcrawl3DParam.setZPatchSize(Integer.parseInt(ltfZPatchSize
          .getText(doValidation)));
      badParameter = ltfXNPatches.getLabel();
      patchcrawl3DParam.setNX(Integer.parseInt(ltfXNPatches.getText(doValidation)));
      badParameter = ltfYNPatches.getLabel();
      patchcrawl3DParam.setNY(Integer.parseInt(ltfYNPatches.getText(doValidation)));
      badParameter = ltfZNPatches.getLabel();
      patchcrawl3DParam.setNZ(Integer.parseInt(ltfZNPatches.getText(doValidation)));
      badParameter = ltfXLow.getLabel();
      patchcrawl3DParam.setXLow(Integer.parseInt(ltfXLow.getText(doValidation)));
      badParameter = ltfXHigh.getLabel();
      patchcrawl3DParam.setXHigh(Integer.parseInt(ltfXHigh.getText(doValidation)));
      badParameter = ltfYLow.getLabel();
      patchcrawl3DParam.setYLow(Integer.parseInt(ltfYLow.getText(doValidation)));
      badParameter = ltfYHigh.getLabel();
      patchcrawl3DParam.setYHigh(Integer.parseInt(ltfYHigh.getText(doValidation)));
      badParameter = ltfZLow.getLabel();
      patchcrawl3DParam.setZLow(Integer.parseInt(ltfZLow.getText(doValidation)));
      badParameter = ltfZHigh.getLabel();
      patchcrawl3DParam.setZHigh(Integer.parseInt(ltfZHigh.getText(doValidation)));
      badParameter = ltfInitialShiftX.getLabel();
      patchcrawl3DParam.setInitialShiftX(ltfInitialShiftX.getText(doValidation));
      badParameter = ltfInitialShiftY.getLabel();
      patchcrawl3DParam.setInitialShiftY(ltfInitialShiftY.getText(doValidation));
      badParameter = ltfInitialShiftZ.getLabel();
      patchcrawl3DParam.setInitialShiftZ(ltfInitialShiftZ.getText(doValidation));
      badParameter = cbKernelSigma.getText();
      patchcrawl3DParam.setKernelSigma(cbKernelSigma.isSelected(),
          tfKernelSigma.getText(doValidation));
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
    return true;
  }

  /**
   * Set the values of the matchorwarp UI objects from the
   * ConstMatchorwarpParam object.
   * 
   * @param matchorwarpParam
   */
  public void setMatchorwarpParams(ConstMatchorwarpParam matchorwarpParam) {
    ltfWarpLimit.setText(matchorwarpParam.getWarpLimit());
    ltfRefineLimit.setText(matchorwarpParam.getRefineLimit());

    if (matchorwarpParam.getXLowerExclude() > 0) {
      ltfXLowerExclude.setText(matchorwarpParam.getXLowerExclude());
    }
    if (matchorwarpParam.getXUpperExclude() > 0) {
      ltfXUpperExclude.setText(matchorwarpParam.getXUpperExclude());
    }

    if (matchorwarpParam.getZLowerExclude() > 0) {
      ltfZLowerExclude.setText(matchorwarpParam.getZLowerExclude());
    }

    if (matchorwarpParam.getZUpperExclude() > 0) {
      ltfZUpperExclude.setText(matchorwarpParam.getZUpperExclude());
    }

    cbUseLinearInterpolation.setSelected(matchorwarpParam.isUseLinearInterpolation());

    // when loading into the dialog, matchorwarp takes precidence over patchcorr
    cbUsePatchRegionModel.setSelected(matchorwarpParam.isUseModelFile());
  }

  /**
   * Set the MatchorwarpParam object values from the UI values.
   * 
   * @param matchorwarpParam
   * @throws NumberFormatException
   */
  public boolean getMatchorwarpParams(MatchorwarpParam matchorwarpParam,
      final boolean doValidation) throws NumberFormatException {
    try {
      String badParameter = "";

      try {
        badParameter = cbUsePatchRegionModel.getText();
        if (cbUsePatchRegionModel.isSelected()) {
          matchorwarpParam.setDefaultModelFile();
        }
        else {
          matchorwarpParam.setModelFile("");
        }

        badParameter = ltfWarpLimit.getLabel();
        matchorwarpParam.setWarpLimit(ltfWarpLimit.getText(doValidation));

        badParameter = ltfRefineLimit.getLabel();
        matchorwarpParam.setRefineLimit(Double.parseDouble(ltfRefineLimit
            .getText(doValidation)));

        badParameter = ltfXLowerExclude.getLabel();
        String text = ltfXLowerExclude.getText(doValidation);
        if (text.matches("\\S+")) {
          matchorwarpParam.setXLowerExclude(Integer.parseInt(text));
        }
        else {
          matchorwarpParam.setXLowerExclude(0);
        }

        badParameter = ltfXUpperExclude.getLabel();
        text = ltfXUpperExclude.getText(doValidation);
        if (text.matches("\\S+")) {
          matchorwarpParam.setXUpperExclude(Integer.parseInt(text));
        }
        else {
          matchorwarpParam.setXUpperExclude(0);
        }

        badParameter = ltfZLowerExclude.getLabel();
        text = ltfZLowerExclude.getText(doValidation);
        if (text.matches("\\S+")) {
          matchorwarpParam.setZLowerExclude(Integer.parseInt(text));
        }
        else {
          matchorwarpParam.setZLowerExclude(0);
        }

        badParameter = ltfZUpperExclude.getLabel();
        text = ltfZUpperExclude.getText(doValidation);
        if (text.matches("\\S+")) {
          matchorwarpParam.setZUpperExclude(Integer.parseInt(text));
        }
        else {
          matchorwarpParam.setZUpperExclude(0);
        }
        badParameter = cbUseLinearInterpolation.getText();
        matchorwarpParam.setUseLinearInterpolation(cbUseLinearInterpolation.isSelected());
      }
      catch (NumberFormatException except) {
        String message = badParameter + " " + except.getMessage();
        throw new NumberFormatException(message);
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  /**
   * Get the combine parameters from the UI
   * @param combineParams
   */
  /* public void getCombineParameters(CombineParams combineParams) { if
   * (cbUsePatchRegionModel.isSelected()) { combineParams.setDefaultPatchRegionModel(); }
   * else { combineParams.setPatchRegionModel(""); } } */

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { Patchcrawl3DParam.getTitle(), "Matchorwarp" };
    String[] manPage = { Patchcrawl3DParam.COMMAND + ".html", "matchorwarp.html" };
    String[] logFileLabel = { "Patchcorr", "Matchorwarp", "Volcombine" };
    String[] logFile = { "patchcorr.log", "matchorwarp.log", "volcombine.log" };
    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent,
        "Patch Problems in Combining", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, AxisID.ONLY);
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void buttonAction(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    // Synchronize this panel with the others
    tomogramCombinationDialog.synchronize(TomogramCombinationDialog.lblFinal, true);
    // Decrease patch sizes by 20%
    // and then round to ints
    // since they are in
    // pixels
    try {
      if (command.equals(btnPatchsizeDecrease.getActionCommand())) {
        ltfXPatchSize
            .setText(Math.round(Integer.parseInt(ltfXPatchSize.getText(true)) / 1.2f));
        ltfYPatchSize
            .setText(Math.round(Integer.parseInt(ltfYPatchSize.getText(true)) / 1.2f));
        ltfZPatchSize
            .setText(Math.round(Integer.parseInt(ltfZPatchSize.getText(true)) / 1.2f));
      }
      // Increase patch sizes by 20% and then round to ints since they are
      // in
      // pixels
      else if (command.equals(btnPatchsizeIncrease.getActionCommand())) {
        ltfXPatchSize
            .setText(Math.round(Integer.parseInt(ltfXPatchSize.getText(true)) * 1.2f));
        ltfYPatchSize
            .setText(Math.round(Integer.parseInt(ltfYPatchSize.getText(true)) * 1.2f));
        ltfZPatchSize
            .setText(Math.round(Integer.parseInt(ltfZPatchSize.getText(true)) * 1.2f));
      }
      else if (command.equals(btnPatchcorrRestart.getActionCommand())) {
        applicationManager.patchcorrCombine(btnPatchcorrRestart, null,
            deferred3dmodButton, run3dmodMenuOptions, dialogType,
            tomogramCombinationDialog.getRunProcessingMethod());
      }
      else if (command.equals(btnMatchorwarpRestart.getActionCommand())) {
        applicationManager.matchorwarpCombine(btnMatchorwarpRestart, null,
            deferred3dmodButton, run3dmodMenuOptions, dialogType,
            tomogramCombinationDialog.getRunProcessingMethod());
      }
      else if (command.equals(btnMatchorwarpTrial.getActionCommand())) {
        applicationManager.matchorwarpTrial(null);
      }
      else if (command.equals(btnVolcombineRestart.getActionCommand())) {
        if (cbParallelProcess.isSelected()) {
          applicationManager.splitcombine(null, deferred3dmodButton, run3dmodMenuOptions,
              dialogType, tomogramCombinationDialog.getRunProcessingMethod());
        }
        else {
          applicationManager.volcombine(btnVolcombineRestart, null, deferred3dmodButton,
              run3dmodMenuOptions, dialogType);
        }
      }
      else if (command.equals(btnPatchVectorModel.getActionCommand())) {
        applicationManager.imodPatchVectorModel(ImodManager.PATCH_VECTOR_MODEL_KEY);
      }
      else if (command.equals(btnPatchVectorCCCModel.getActionCommand())) {
        applicationManager.imodPatchVectorModel(ImodManager.PATCH_VECTOR_CCC_MODEL_KEY);
      }
      else if (command.equals(btnReplacePatchOut.getActionCommand())) {
        applicationManager.modelToPatch();
      }
      else if (command.equals(cbParallelProcess.getActionCommand())) {
        sendProcessingMethodMessage();
      }
      else if (command.equals(cbKernelSigma.getActionCommand())) {
        updateKernelSigma();
      }
      else if (command.equals(btnPatchRegionModel.getActionCommand())) {
        applicationManager.imodPatchRegionModel(run3dmodMenuOptions);
      }
      else if (command.equals(btnImodMatchedTo.getActionCommand())) {
        applicationManager.imodMatchedToTomogram(run3dmodMenuOptions);
      }
      else if (command.equals(btnImodCombined.getActionCommand())) {
        applicationManager.imodCombinedTomogram(run3dmodMenuOptions);
      }
    }
    catch (FieldValidationFailedException e) {
    }
  }

  ProcessingMethod getProcessingMethod() {
    if (cbParallelProcess.isEnabled() && cbParallelProcess.isSelected()) {
      return ProcessingMethod.PP_CPU;
    }
    return ProcessingMethod.LOCAL_CPU;
  }

  private void sendProcessingMethodMessage() {
    applicationManager.getProcessingMethodMediator(AxisID.FIRST).setMethod(
        tomogramCombinationDialog, getProcessingMethod());
  }

  private void updateKernelSigma() {
    tfKernelSigma.setEnabled(cbKernelSigma.isSelected());
  }

  void updatePatchVectorModelDisplay() {
    boolean enable = DatasetFiles.getPatchVectorModel(applicationManager).exists();
    btnPatchVectorModel.setEnabled(enable);
    btnReplacePatchOut.setEnabled(enable);
  }

  private final class ButtonActionListener implements ActionListener {
    private final FinalCombinePanel listenee;

    private ButtonActionListener(final FinalCombinePanel finalCombinePanel) {
      listenee = finalCombinePanel;
    }

    public void actionPerformed(final ActionEvent event) {
      listenee.buttonAction(event.getActionCommand(), null, null);
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    String text;
    ReadOnlyAutodoc adocCombineFft = null;
    ReadOnlyAutodoc adocCorrsearch3d = null;

    try {
      adocCombineFft = AutodocFactory.getInstance(applicationManager,
          AutodocFactory.COMBINE_FFT, AxisID.ONLY);
      adocCorrsearch3d = AutodocFactory.getInstance(applicationManager,
          AutodocFactory.CORR_SEARCH_3D, AxisID.ONLY);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return;
    }
    ltfXPatchSize.setToolTipText("Size of correlation patches in X.");
    ltfYPatchSize.setToolTipText("Size of correlation patches in Y.");
    ltfZPatchSize.setToolTipText("Size of correlation patches in Z.");
    btnPatchsizeIncrease.setToolTipText("Increase all patch dimensions by 20%.");
    btnPatchsizeDecrease.setToolTipText("Decrease all patch dimensions by 20%.");
    ltfXNPatches.setToolTipText("Number of patches to correlate in the X dimension.");
    ltfYNPatches.setToolTipText("Number of patches to correlate in the Y dimension.");
    ltfZNPatches.setToolTipText("Number of patches to correlate in the Z dimension.");
    ltfXLow.setToolTipText("Minimum X coordinate for left edge of correlation patches.");
    ltfYLow.setToolTipText("Minimum Y coordinate for upper edge of correlation patches.");
    ltfZLow.setToolTipText("Minimum Z coordinate for top edge of correlation patches.");
    ltfXHigh
        .setToolTipText("Maximum X coordinate for right edge of correlation patches.");
    ltfYHigh
        .setToolTipText("Maximum Y coordinate for lower edge of correlation patches.");
    ltfZHigh
        .setToolTipText("Maximum Z coordinate for bottom edge of correlation patches.");
    btnPatchVectorCCCModel
        .setToolTipText("Open a patch vector model containing cross-correlation coefficients.  "
            + "In 3dmodv Objects, click on Values, and select on Show stored values.");
    btnPatchcorrRestart
        .setToolTipText("Compute new displacements between patches by cross-correlation.");
    cbUsePatchRegionModel
        .setToolTipText("Use a model with contours around the areas where patches should be "
            + "correlated to prevent bad patches outside those areas.");
    btnPatchRegionModel
        .setToolTipText("Open the volume being matched to and create the patch region model.");
    ltfRefineLimit
        .setToolTipText("Enter a comma-separate series of mean residual limits to try in "
            + "succession when fitting warping transformations to the patch "
            + "displacements.");
    ltfWarpLimit
        .setToolTipText("The mean residual limit for fit all patch displacements to a single "
            + "linear transformation.");
    ltfXLowerExclude
        .setToolTipText("Exclude columns of patches on the left from the fits. Number of columns "
            + "of patches on the left to exclude from the fits.");
    ltfXUpperExclude
        .setToolTipText("Exclude columns of patches on the right from the fits. Number of columns"
            + " of patches on the right to exclude from the fits.");
    ltfZLowerExclude
        .setToolTipText("Exclude rows of patches on the bottom from the fits. Number of rows of "
            + "patches on the bottom in Y to exclude from the fits.");
    ltfZUpperExclude
        .setToolTipText("Exclude rows of patches on the top from the fits. Number of rows of "
            + "patches on the top in Y to exclude from the fits.");

    cbUseLinearInterpolation
        .setToolTipText("Uses linear instead of quadratic interpolation for transforming"
            + "the volume with Matchvol or Warpvol.");
    btnMatchorwarpRestart
        .setToolTipText("Restart the combine operation at Matchorwarp, which tries to fit "
            + "transformations to the patch displacements.");
    btnVolcombineRestart
        .setToolTipText("Restart the combine operation at Volcombine, which combines volumes.");
    btnMatchorwarpTrial
        .setToolTipText("Run Matchorwarp in trial mode; find transformations then stop.");
    btnPatchVectorModel
        .setToolTipText("View the patch displacement vectors in and possibly "
            + "delete bad vectors.  To see the residual values, click on Values in "
            + "3dmodv Objects, and select on Show stored values.");
    btnReplacePatchOut
        .setToolTipText("Replace the patch displacements with the vectors from the edited model.");
    btnImodMatchedTo.setToolTipText("View the volume being matched to in 3dmod.");
    text = "View the final combined volume.";
    btnImodCombined.setToolTipText("View the final combined volume.");
    cbNoVolcombine
        .setToolTipText("Stop after running Matchorwarp.  Use the \"Restart at Volcombine\" button to continue.");

    text = "Filter by convolving in real space with a Gaussian kernel.  The "
        + "amount of filtering is controlled by the sigma of the Gaussian, in "
        + "pixels.  Higher sigma filters more.  Kernel filtering increases "
        + "execution time ~30% for sigma under 1.5 and ~2-fold for sigma 1.5 or "
        + "higher.";
    cbKernelSigma.setToolTipText(text);
    tfKernelSigma.setToolTipText(text);

    cbParallelProcess.setToolTipText(VOLCOMBINE_PARALLEL_PROCESSING_TOOL_TIP);

    if (adocCombineFft != null) {
      ltfReductionFactor.setToolTipText(EtomoAutodoc.getTooltip(adocCombineFft,
          "ReductionFraction"));
      ltfLowFromBothRadius.setToolTipText(EtomoAutodoc.getTooltip(adocCombineFft,
          "LowFromBothRadius"));
    }

    text = EtomoAutodoc.getTooltip(adocCorrsearch3d,
        Patchcrawl3DParam.INITIAL_SHIFT_XYZ_KEY);
    ltfInitialShiftX.setToolTipText(text);
    ltfInitialShiftY.setToolTipText(text);
    ltfInitialShiftZ.setToolTipText(text);
  }
}