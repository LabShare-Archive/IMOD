package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JFileChooser;

import etomo.ParallelManager;
import etomo.ProcessingMethodMediator;
import etomo.comscript.AnisotropicDiffusionParam;
import etomo.comscript.ChunksetupParam;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.TrimvolParam;
import etomo.logic.DatasetTool;
import etomo.process.ImodManager;
import etomo.storage.TomogramFileFilter;
import etomo.type.AxisID;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.ParallelMetaData;
import etomo.type.ProcessName;
import etomo.type.ProcessingMethod;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.Utilities;

/**
 * <p>Description: Main dialog for Nonlinear anisotropic diffusion.  Appears as
 * the result of a button press on the parallel processing interface main
 * dialog.</p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.3  2011/02/22 18:01:41  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.23  2010/04/28 16:32:52  sueh
 * <p> bug# 1344 Added getSubdirectory.
 * <p>
 * <p> Revision 1.22  2010/03/27 04:55:04  sueh
 * <p> bug# 1337 Fixed the partial path display in FileTextField.
 * <p>
 * <p> Revision 1.21  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.20  2010/01/21 21:31:14  sueh
 * <p> bug# 1305 Factored filter full volume panel out of AnisotropicDiffusionDialog.
 * <p>
 * <p> Revision 1.19  2009/11/20 17:00:30  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.
 * <p>
 * <p> Revision 1.18  2009/09/20 21:31:56  sueh
 * <p> bug# 1268 Added timestamp and dialog identification to log.
 * <p>
 * <p> Revision 1.17  2009/09/05 00:38:08  sueh
 * <p> bug# 1256 Made ITERATION_LIST_LABEL public.  Moved error reporting when iteration list is set to functions called by the set... function.
 * <p>
 * <p> Revision 1.16  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.15  2009/03/06 23:37:27  sueh
 * <p> bug# 1194 Made labels public.
 * <p>
 * <p> Revision 1.14  2008/09/30 20:54:36  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.13  2008/08/26 20:36:39  sueh
 * <p> bug# 1122 Moved deleteSubdir from dialog to manager so that
 * <p> ImodManager can be queried about open 3dmods.  Check deleteSubdir
 * <p> return value before setting subdirName to null.
 * <p>
 * <p> Revision 1.12  2008/06/20 20:05:29  sueh
 * <p> bug# 1119 Added debug.
 * <p>
 * <p> Revision 1.11  2008/05/28 02:49:18  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 1.10  2008/05/13 22:59:23  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 1.9  2008/05/03 00:46:59  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.8  2008/02/26 01:39:17  sueh
 * <p> bug# 1087 Modified initSubdir to fail if ftfVolume is empty.
 * <p>
 * <p> Revision 1.7  2008/02/01 01:37:14  sueh
 * <p> bug# 1075 Easter egg.
 * <p>
 * <p> Revision 1.6  2007/12/10 22:41:01  sueh
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * <p> getParameters because it is required and has been added to the
 * <p> ProcesschunksParam constructor.
 * <p>
 * <p> Revision 1.5  2007/11/14 23:47:26  sueh
 * <p> bug# 1047 Formatted.
 * <p>
 * <p> Revision 1.4  2007/11/12 22:13:00  sueh
 * <p> bug# 1047 Implementing ContextMenu.
 * <p>
 * <p> Revision 1.3  2007/11/12 15:03:26  sueh
 * <p> bug# 1047 Added the subdirectory to the remote path when the sub-directory is
 * <p> set.  Setting the absolute path from ftfVolume rather then the text
 * <p> because ftfVolume is displaying an abbreviated path.
 * <p>
 * <p> Revision 1.2  2007/11/09 17:46:26  sueh
 * <p> bug# 1047 Added tooltips.
 * <p>
 * <p> Revision 1.1  2007/11/06 19:51:55  sueh
 * <p> bug# 1047 Dialog to choose values and run nad_eed_3d.
 * 
 * <p> </p>
 */
public final class AnisotropicDiffusionDialog implements ContextMenu,
    AbstractParallelDialog, Run3dmodButtonContainer, FilterFullVolumeParent,
    ProcessInterface {
  public static final String rcsid = "$Id$";

  public static final String CLEANUP_LABEL = FilterFullVolumePanel.CLEANUP_LABEL;
  public static final String FILTER_FULL_VOLUME_LABEL = FilterFullVolumePanel.FILTER_FULL_VOLUME_LABEL;
  public static final int MEMORY_PER_CHUNK_DEFAULT = FilterFullVolumePanel.MEMORY_PER_CHUNK_DEFAULT;
  public static final String MEMORY_PER_CHUNK_LABEL = FilterFullVolumePanel.MEMORY_PER_CHUNK_LABEL;

  private static final String K_VALUE_LIST_LABEL = "List of K values: ";
  public static final String ITERATION_LIST_LABEL = "List of iterations: ";
  private static final String K_VALUE_LABEL = "K value: ";
  private static final String ITERATION_LABEL = "Iterations: ";
  private static final DialogType DIALOG_TYPE = DialogType.ANISOTROPIC_DIFFUSION;

  public static final String TEST_VOLUME_NAME = "test.input";

  private final SpacedPanel rootPanel = SpacedPanel.getInstance();
  private final Run3dmodButton btnViewFullVolume = Run3dmodButton.get3dmodInstance(
      "View Full Volume", this);
  private final FileTextField ftfVolume = FileTextField
      .getPartialPathInstance("Pick a volume");
  private final MultiLineButton btnExtractTestVolume = new MultiLineButton(
      "Extract Test Volume");
  private final Run3dmodButton btnViewTestVolume = Run3dmodButton.get3dmodInstance(
      "View Test Volume", this);
  private final CheckBox cbLoadWithFlipping = new CheckBox("Load with flipping");
  private final LabeledTextField ltfTestKValueList = new LabeledTextField(
      FieldType.FLOATING_POINT_ARRAY, K_VALUE_LIST_LABEL);
  private final Spinner spTestIteration = Spinner.getLabeledInstance(ITERATION_LABEL, 10,
      1, 200);
  private final Run3dmodButton btnRunVaryingK = Run3dmodButton.getDeferred3dmodInstance(
      "Run with Different K Values", this);
  private final Run3dmodButton btnViewVaryingK = Run3dmodButton.get3dmodInstance(
      "View Different K Values Test Results", this);
  private final LabeledTextField ltfTestKValue = new LabeledTextField(
      FieldType.FLOATING_POINT, K_VALUE_LABEL);
  private final LabeledTextField ltfTestIterationList = new LabeledTextField(
      FieldType.INTEGER_LIST, ITERATION_LIST_LABEL);
  private final Run3dmodButton btnRunVaryingIteration = Run3dmodButton
      .getDeferred3dmodInstance("Run with Different Iterations", this);
  private final Run3dmodButton btnViewVaryingIteration = Run3dmodButton.get3dmodInstance(
      "View Different Iteration Test Results", this);
  private final FilterFullVolumePanel filterFullVolumePanel;

  private final RubberbandPanel pnlTestVolumeRubberband;
  private final ParallelManager manager;
  private final ProcessingMethodMediator mediator;

  private String subdirName = null;
  private boolean debug = false;

  private void setToolTipText() {
    cbLoadWithFlipping
        .setToolTipText("Load volumes into 3dmod with flipping of Y and Z; use "
            + "this for a tomogram that has not been flipped or rotated in "
            + "post-processing.");
    btnViewFullVolume.setToolTipText("View the full volume in 3dmod.");
    btnExtractTestVolume
        .setToolTipText("Cut out a test volume from the indicated coordinate " + "range.");
    btnViewTestVolume.setToolTipText("View the test volume in 3dmod.");
    ltfTestKValueList
        .setToolTipText("Set of K threshold values to try on the test volume "
            + "with the given number of iterations.");
    spTestIteration.setToolTipText("Number of iterations to run for each K value.");
    btnRunVaryingK.setToolTipText("Compute a set of test volumes with the different K "
        + "threshold values and a fixed number of iterations");
    btnViewVaryingK
        .setToolTipText("View the volumes computed with different K values in "
            + "3dmod.");
    ltfTestKValue.setToolTipText("Single K threshold value to use with different numbers"
        + " of iterations.");
    ltfTestIterationList
        .setToolTipText("List of number of iterations to try with the single K "
            + "value. Comma-separated ranges of numbers are allowed.");
    btnRunVaryingIteration
        .setToolTipText("Compute a set of test volumes with the different "
            + "numbers of iterations and a fixed K value");
    btnViewVaryingIteration
        .setToolTipText("View the volumes computed with different iteration "
            + "numbers in 3dmod");
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Anisotropic Diffusion", "3dmod", "Processchunks",
        "Chunksetup" };
    String[] manPage = { ProcessName.ANISOTROPIC_DIFFUSION + ".html", "3dmod.html",
        "processchunks.html", "chunksetup.html" };
    String[] logFileLabel = { "Anisotropic Diffusion" };
    String[] logFile = new String[1];
    logFile[0] = ProcessName.ANISOTROPIC_DIFFUSION + ".log";
    // ContextPopup contextPopup =
    new ContextPopup(rootPanel.getContainer(), mouseEvent, "ANISOTROPIC DIFFUSION",
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
        AxisID.ONLY, subdirName);
  }

  public void setDebug(boolean input) {
    debug = input;
  }

  private AnisotropicDiffusionDialog(final ParallelManager manager) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: "
        + DialogType.ANISOTROPIC_DIFFUSION);
    this.manager = manager;
    mediator = manager.getProcessingMethodMediator(AxisID.ONLY);
    filterFullVolumePanel = FilterFullVolumePanel.getInstance(manager, DIALOG_TYPE, this);
    // root
    rootPanel.setBoxLayout(BoxLayout.X_AXIS);
    if (Utilities.isAprilFools()) {
      rootPanel.setBorder(new BeveledBorder("Anisotropic Delusion").getBorder());
    }
    else {
      rootPanel.setBorder(new BeveledBorder("Anisotropic Diffusion").getBorder());
    }
    rootPanel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    // first column
    SpacedPanel pnlFirst = SpacedPanel.getInstance();
    pnlFirst.setBoxLayout(BoxLayout.Y_AXIS);
    // volume
    pnlFirst.add(ftfVolume.getContainer());
    SpacedPanel pnlLoadWithFlipping = SpacedPanel.getInstance();
    pnlLoadWithFlipping.setBoxLayout(BoxLayout.X_AXIS);
    pnlLoadWithFlipping.add(cbLoadWithFlipping);
    pnlLoadWithFlipping.addHorizontalGlue();
    pnlFirst.add(pnlLoadWithFlipping);
    // extract
    SpacedPanel pnlExtract = SpacedPanel.getInstance();
    pnlExtract.setBoxLayout(BoxLayout.Y_AXIS);
    pnlExtract.setBorder(new EtchedBorder("Extract Test Volume").getBorder());
    pnlExtract.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlTestVolumeRubberband = RubberbandPanel.getInstance(manager,
        ImodManager.VOLUME_KEY, "Test Volume Range:", "Get test volume range From 3dmod",
        "Minimum X coordinate on the left side for the test volume range.",
        "Maximum X coordinate on the right side for the test volume range.",
        "The lower Y coordinate for the test volume range.",
        "The upper Y coordinate for the test volume range.",
        "The starting slice for the test volume range.",
        "The ending slice for the test volume range.", btnViewFullVolume);
    pnlExtract.add(pnlTestVolumeRubberband.getContainer());
    SpacedPanel pnlExtractButtons = SpacedPanel.getInstance();
    pnlExtractButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlExtractButtons.addHorizontalGlue();
    btnExtractTestVolume.setSize();
    pnlExtractButtons.add(btnExtractTestVolume);
    pnlExtractButtons.addHorizontalGlue();
    btnViewTestVolume.setSize();
    pnlExtractButtons.add(btnViewTestVolume);
    pnlExtractButtons.addHorizontalGlue();
    pnlExtract.add(pnlExtractButtons);
    pnlFirst.add(pnlExtract);
    rootPanel.add(pnlFirst);
    // second column
    SpacedPanel pnlSecond = SpacedPanel.getInstance();
    pnlSecond.setBoxLayout(BoxLayout.Y_AXIS);
    // varying K
    SpacedPanel pnlVaryingK = SpacedPanel.getInstance();
    pnlVaryingK.setBoxLayout(BoxLayout.Y_AXIS);
    pnlVaryingK.setBorder(new EtchedBorder("Find a K Value for Test Volume").getBorder());
    pnlVaryingK.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    SpacedPanel pnlVaryingKFields = SpacedPanel.getInstance();
    pnlVaryingKFields.setBoxLayout(BoxLayout.X_AXIS);
    ltfTestKValueList.setTextPreferredWidth(UIParameters.INSTANCE.getListWidth());
    pnlVaryingKFields.add(ltfTestKValueList);
    pnlVaryingKFields.add(spTestIteration);
    pnlVaryingK.add(pnlVaryingKFields);
    SpacedPanel pnlVaryingKButtons = SpacedPanel.getInstance();
    pnlVaryingKButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingKButtons.addHorizontalGlue();
    btnRunVaryingK.setDeferred3dmodButton(btnViewVaryingK);
    btnRunVaryingK.setSize();
    pnlVaryingKButtons.add(btnRunVaryingK);
    pnlVaryingKButtons.addHorizontalGlue();
    btnViewVaryingK.setSize();
    pnlVaryingKButtons.add(btnViewVaryingK);
    pnlVaryingKButtons.addHorizontalGlue();
    pnlVaryingK.add(pnlVaryingKButtons);
    pnlSecond.add(pnlVaryingK);
    // varying iterations
    SpacedPanel pnlVaryingIteration = SpacedPanel.getInstance();
    pnlVaryingIteration.setBoxLayout(BoxLayout.Y_AXIS);
    pnlVaryingIteration.setBorder(new EtchedBorder(
        "Find an Iteration Number for Test Volume").getBorder());
    SpacedPanel pnlVaryingIterationFields = SpacedPanel.getInstance();
    pnlVaryingIterationFields.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingIterationFields.add(ltfTestKValue);
    ltfTestIterationList.setTextPreferredWidth(UIParameters.INSTANCE.getListWidth());
    pnlVaryingIterationFields.add(ltfTestIterationList);
    pnlVaryingIteration.add(pnlVaryingIterationFields);
    SpacedPanel pnlVaryingIterationButtons = SpacedPanel.getInstance();
    pnlVaryingIterationButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingIterationButtons.addHorizontalGlue();
    btnRunVaryingIteration.setDeferred3dmodButton(btnViewVaryingIteration);
    btnRunVaryingIteration.setSize();
    pnlVaryingIterationButtons.add(btnRunVaryingIteration);
    pnlVaryingIterationButtons.addHorizontalGlue();
    btnViewVaryingIteration.setSize();
    pnlVaryingIterationButtons.add(btnViewVaryingIteration);
    pnlVaryingIterationButtons.addHorizontalGlue();
    pnlVaryingIteration.add(pnlVaryingIterationButtons);
    pnlSecond.add(pnlVaryingIteration);
    pnlSecond.add(filterFullVolumePanel.getComponent());
    rootPanel.add(pnlSecond);
    setToolTipText();
    mediator.register(this);
    mediator.setMethod(this, ProcessingMethod.PP_CPU);
  }

  /**
   * Get the processing method based on the dialogs settings.  Dialogs don't
   * need to know if QUEUE is in use in the parallel panel.
   * @return
   */
  public ProcessingMethod getProcessingMethod() {
    return ProcessingMethod.PP_CPU;
  }

  public void disableGpu(final boolean disable) {
  }

  public void lockProcessingMethod(final boolean lock) {
  }

  public static AnisotropicDiffusionDialog getInstance(ParallelManager manager,
      AxisID axisID) {
    AnisotropicDiffusionDialog instance = new AnisotropicDiffusionDialog(manager);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    ftfVolume.addActionListener(new VolumeActionListener(this));
    ADDActionListener listener = new ADDActionListener(this);
    btnViewFullVolume.addActionListener(listener);
    btnExtractTestVolume.addActionListener(listener);
    btnViewTestVolume.addActionListener(listener);
    btnRunVaryingK.addActionListener(listener);
    btnViewVaryingK.addActionListener(listener);
    btnRunVaryingIteration.addActionListener(listener);
    btnViewVaryingIteration.addActionListener(listener);
    rootPanel.addMouseListener(new GenericMouseAdapter(this));
  }

  public void getParameters(ParallelParam param) {
    ProcesschunksParam processchunksParam = (ProcesschunksParam) param;
    processchunksParam.setSubdirName(subdirName);
  }

  public DialogType getDialogType() {
    return DialogType.ANISOTROPIC_DIFFUSION;
  }

  public Container getContainer() {
    return rootPanel.getContainer();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  public void getInitialParameters(final ParallelMetaData metaData) {
    metaData.setRootName(ftfVolume.getFileName());
    metaData.setVolume(ftfVolume.getFileAbsolutePath());
  }

  public void getParameters(final ParallelMetaData metaData) {
    metaData.setLoadWithFlipping(cbLoadWithFlipping.isSelected());
    pnlTestVolumeRubberband.getParameters(metaData);
    metaData.setTestKValueList(ltfTestKValueList.getText());
    metaData.setTestIteration(spTestIteration.getValue());
    metaData.setTestKValue(ltfTestKValue.getText());
    metaData.setTestIterationList(ltfTestIterationList.getText());
    filterFullVolumePanel.getParameters(metaData);
  }
  
 public void getParametersForTrimvol(final ParallelMetaData metaData) {
   pnlTestVolumeRubberband.getParametersForTrimvol(metaData);
  }

  public Number getMemoryPerChunk() {
    return filterFullVolumePanel.getMemoryPerChunk();
  }

  public void setParameters(final ParallelMetaData metaData) {
    ftfVolume.setButtonEnabled(false);
    ftfVolume.setText(metaData.getVolume());
    cbLoadWithFlipping.setSelected(metaData.isLoadWithFlipping());
    pnlTestVolumeRubberband.setParameters(metaData);
    ltfTestKValueList.setText(metaData.getTestKValueList());
    spTestIteration.setValue(metaData.getTestIteration());
    ltfTestKValue.setText(metaData.getTestKValue());
    ltfTestIterationList.setText(metaData.getTestIterationList());
    filterFullVolumePanel.setParameters(metaData);
    initSubdir();
  }

  /**
   * Get the parameter values from the panel 
   * @param trimvolParam
   */
  public boolean getParameters(TrimvolParam param, final boolean doValidation) {
    if (!pnlTestVolumeRubberband.getParameters(param, doValidation)) {
      return false;
    }
    param.setFlippedVolume(cbLoadWithFlipping.isSelected());
    param.setSwapYZ(false);
    param.setRotateX(false);
    param.setConvertToBytes(false);
    param.setInputFileName(ftfVolume.getFileName());
    param.setOutputFileName(new File(subdirName, TEST_VOLUME_NAME).getPath());
    return true;
  }

  public boolean getParametersForVaryingK(AnisotropicDiffusionParam param,
      final boolean doValidation) {
    try {
      String errorMessage = null;
      if (debug) {
        System.out.println("getParametersForVaryingK:ltfTestKValueList.getText()="
            + ltfTestKValueList.getText());
      }
      errorMessage = param.setKValueList(ltfTestKValueList.getText(doValidation));
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, K_VALUE_LIST_LABEL + errorMessage,
            "Entry Error");
        return false;
      }
      param.setIteration(spTestIteration.getValue());
      // Must use an absolute file when testing file existance. Changing the
      // working directory by changing the user.dir property doesn't work for
      // relative file paths. The path looks correct, but File.exists() returns an
      // incorrect result.
      if (!(new File(new File(manager.getPropertyUserDir(), subdirName), TEST_VOLUME_NAME)
          .exists())) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Test volume has not been created.  Please extract test volume.",
            "Entry Error");
        return false;
      }
      param.setSubdirName(subdirName);
      param.setInputFileName(TEST_VOLUME_NAME);
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public boolean getParameters(AnisotropicDiffusionParam param, final boolean doValidation) {
    param.setSubdirName(subdirName);
    return filterFullVolumePanel.getParameters(param, doValidation);
  }

  public void getParameters(ChunksetupParam param) {
    filterFullVolumePanel.getParameters(param);
    param.setCommandFile(AnisotropicDiffusionParam.getFilterFullFileName());
    param.setSubdirName(subdirName);
    param.setInputFile(ftfVolume.getFileName());
    param.setOutputFile(getOutputFileName(ftfVolume.getFileName()));
  }

  public boolean getParametersForVaryingIteration(AnisotropicDiffusionParam param,
      final boolean doValidation) {
    try {
      String errorMessage = null;
      param.setKValue(ltfTestKValue.getText(doValidation));
      if (!param.setIterationList(ltfTestIterationList.getText(doValidation))) {
        return false;
      }
      param.setSubdirName(subdirName);
      param.setInputFileName(TEST_VOLUME_NAME);
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  /**
   * Initialized subdirName if is not already initialized
   * @return false if ftfVolume is empty (subdirName is dependent on ftfVolume)
   */
  public boolean initSubdir() {
    if (ftfVolume.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Please choose a volume before running this function.", "Entry Error");
      return false;
    }
    if (subdirName == null) {
      subdirName = "naddir." + ftfVolume.getFileName();
      if (!manager.makeSubdir(subdirName)) {
        return false;
      }
    }
    return true;
  }

  public String getSubdirectory() {
    if (!initSubdir()) {
      return null;
    }
    return subdirName;
  }

  public void cleanUp() {
    if (subdirName != null && manager.deleteSubdir(subdirName)) {
      subdirName = null;
    }
  }

  public String getVolume() {
    return ftfVolume.getFileAbsolutePath();
  }

  public boolean isLoadWithFlipping() {
    return cbLoadWithFlipping.isSelected();
  }

  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnExtractTestVolume.getActionCommand())) {
      if (!initSubdir()) {
        return;
      }
      manager.trimVolume(null);
    }
    else if (command.equals(btnRunVaryingK.getActionCommand())) {
      if (!initSubdir()) {
        return;
      }
      manager.anisotropicDiffusionVaryingK(subdirName, null, deferred3dmodButton,
          run3dmodMenuOptions, DIALOG_TYPE,
          mediator.getRunMethodForProcessInterface(getProcessingMethod()));
    }
    else if (command.equals(btnRunVaryingIteration.getActionCommand())) {
      if (!initSubdir()) {
        return;
      }
      manager.anisotropicDiffusionVaryingIteration(subdirName, null, deferred3dmodButton,
          run3dmodMenuOptions, DIALOG_TYPE);
    }
    else if (command.equals(btnViewFullVolume.getActionCommand())) {
      manager.imod(ImodManager.VOLUME_KEY, ftfVolume.getFile(), run3dmodMenuOptions,
          cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnViewTestVolume.getActionCommand())) {
      manager.imod(ImodManager.TEST_VOLUME_KEY, new File(subdirName, "test.input"),
          run3dmodMenuOptions, cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnViewVaryingK.getActionCommand())) {
      manager.imodVaryingKValue(ImodManager.VARYING_K_TEST_KEY, run3dmodMenuOptions,
          subdirName, TEST_VOLUME_NAME, cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnViewVaryingIteration.getActionCommand())) {
      manager.imodVaryingIteration(ImodManager.VARYING_ITERATION_TEST_KEY,
          run3dmodMenuOptions, subdirName, TEST_VOLUME_NAME,
          cbLoadWithFlipping.isSelected());
    }
  }

  private void openVolume() {
    File volume = null;
    JFileChooser chooser = new FileChooser(new File(manager.getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    chooser.setFileFilter(new TomogramFileFilter());
    int returnVal = chooser.showOpenDialog(rootPanel.getContainer());
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    volume = chooser.getSelectedFile();
    if (volume == null || volume.isDirectory() || !volume.exists()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "Please choose a volume",
          "Entry Error");
      return;
    }
    if (!DatasetTool.validateDatasetName(manager, null, AxisID.ONLY, volume,
        DataFileType.PARALLEL, null)) {
      return;
    }
    ftfVolume.setFile(volume);
    manager.setNewParamFile(volume);
    if (!initSubdir()) {
      ftfVolume.setFile(null);
      return;
    }
    ftfVolume.setButtonEnabled(false);
  }

  private static String getOutputFileName(String fileName) {
    return fileName + ".nad";
  }

  private static final class ADDActionListener implements ActionListener {
    private final AnisotropicDiffusionDialog adaptee;

    ADDActionListener(final AnisotropicDiffusionDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  private static final class VolumeActionListener implements ActionListener {
    private final AnisotropicDiffusionDialog adaptee;

    VolumeActionListener(final AnisotropicDiffusionDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.openVolume();
    }
  }
}