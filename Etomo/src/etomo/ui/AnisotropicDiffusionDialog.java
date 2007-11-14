package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JFileChooser;

import etomo.ParallelManager;
import etomo.comscript.AnisotropicDiffusionParam;
import etomo.comscript.ChunksetupParam;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.TrimvolParam;
import etomo.process.ImodManager;
import etomo.storage.TomogramFileFilter;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ParallelMetaData;
import etomo.type.ProcessName;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
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
    AbstractParallelDialog, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public static final int MEMORY_PER_CHUNK_DEFAULT = 14 * ChunksetupParam.MEMORY_TO_VOXEL;

  private static final String K_VALUE_LIST_LABEL = "List of K values: ";
  private static final String ITERATION_LIST_LABEL = "List of iterations: ";
  private static final String K_VALUE_LABEL = "K value: ";
  private static final String ITERATION_LABEL = "Iterations: ";

  static final String TEST_VOLUME_NAME = "test.input";

  private final SpacedPanel rootPanel = new SpacedPanel();
  private final Run3dmodButton btnViewFullVolume = new Run3dmodButton(
      "View Full Volume", this);
  private final FileTextField ftfVolume = new FileTextField("Pick a volume");
  private final MultiLineButton btnExtractTestVolume = new MultiLineButton(
      "Extract Test Volume");
  private final Run3dmodButton btnViewTestVolume = new Run3dmodButton(
      "View Test Volume", this);
  private final CheckBox cbLoadWithFlipping = new CheckBox("Load with flipping");
  private final LabeledTextField ltfTestKValueList = new LabeledTextField(
      K_VALUE_LIST_LABEL);
  private final Spinner spTestIteration = Spinner.getLabeledInstance(
      ITERATION_LABEL, 10, 1, 200);
  private final MultiLineButton btnRunVaryingK = new MultiLineButton(
      "Run with Different K Values");
  private final Run3dmodButton btnViewVaryingK = new Run3dmodButton(
      "View Different K Values Test Results", this);
  private final LabeledTextField ltfTestKValue = new LabeledTextField(
      K_VALUE_LABEL);
  private final LabeledTextField ltfTestIterationList = new LabeledTextField(
      ITERATION_LIST_LABEL);
  private MultiLineButton btnRunVaryingIteration = new MultiLineButton(
      "Run with Different Iterations");
  private Run3dmodButton btnViewVaryingIteration = new Run3dmodButton(
      "View Different Iteration Test Results", this);
  private final LabeledTextField ltfKValue = new LabeledTextField(K_VALUE_LABEL);
  private final Spinner spIteration = Spinner.getLabeledInstance(
      ITERATION_LABEL, 10, 1, 200);
  private MultiLineButton btnRunFilterFullVolume = new MultiLineButton(
      "Filter Full Volume");
  private Run3dmodButton btnViewFilteredVolume = new Run3dmodButton(
      "View Filtered Volume", this);
  private final Spinner spMemoryPerChunk = Spinner.getLabeledInstance(
      "Memory per chunk (MB): ", MEMORY_PER_CHUNK_DEFAULT,
      ChunksetupParam.MEMORY_TO_VOXEL, 30 * ChunksetupParam.MEMORY_TO_VOXEL,
      ChunksetupParam.MEMORY_TO_VOXEL);
  private final MultiLineButton btnCleanup = new MultiLineButton(
      "Clean Up Subdirectory");

  private final RubberbandPanel pnlTestVolumeRubberband;
  private final ParallelManager manager;

  private String subdirName = null;

  private void setToolTipText() {
    cbLoadWithFlipping
        .setToolTipText("Load volumes into 3dmod with flipping of Y and Z; use "
            + "this for a tomogram that has not been flipped or rotated in "
            + "post-processing.");
    btnViewFullVolume.setToolTipText("View the full volume in 3dmod.");
    btnExtractTestVolume
        .setToolTipText("Cut out a test volume from the indicated coordinate "
            + "range.");
    btnViewTestVolume.setToolTipText("View the test volume in 3dmod.");
    ltfTestKValueList
        .setToolTipText("Set of K threshold values to try on the test volume "
            + "with the given number of iterations.");
    spTestIteration
        .setToolTipText("Number of iterations to run for each K value.");
    btnRunVaryingK
        .setToolTipText("Compute a set of test volumes with the different K "
            + "threshold values and a fixed number of iterations");
    btnViewVaryingK
        .setToolTipText("View the volumes computed with different K values in "
            + "3dmod.");
    ltfTestKValue
        .setToolTipText("Single K threshold value to use with different numbers"
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
    ltfKValue.setToolTipText("K threshold value for running on full volume");
    spIteration.setToolTipText("Number of iterations to run on full volume");
    spMemoryPerChunk
        .setToolTipText("Maximum memory in megabytes to use while running "
            + "diffusion on one chunk. Reduce if there is less memory per "
            + "processor or if you want to break the job into more chunks.  The"
            + " number of voxels in each chunk will be 1/36 of this memory "
            + "limit or less.");
    btnRunFilterFullVolume
        .setToolTipText("Run diffusion on the full volume in chunks, creates "
            + "filename.nad");
    btnViewFilteredVolume
        .setToolTipText("View filtered volume (filename.nad) in 3dmod");
    btnCleanup
        .setToolTipText("Remove subdirectory with all temporary and test files "
            + "(naddir.filename).");
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Anisotropic Diffusion", "3dmod",
        "Processchunks", "Chunksetup" };
    String[] manPage = { ProcessName.ANISOTROPIC_DIFFUSION + ".html",
        "3dmod.html", "processchunks.html", "chunksetup.html" };
    String[] logFileLabel = { "Anisotropic Diffusion" };
    String[] logFile = new String[1];
    logFile[0] = ProcessName.ANISOTROPIC_DIFFUSION + ".log";
    //    ContextPopup contextPopup =
    new ContextPopup(rootPanel.getComponent(), mouseEvent,
        "ANISOTROPIC DIFFUSION", ContextPopup.TOMO_GUIDE, manPagelabel,
        manPage, logFileLabel, logFile, manager, AxisID.ONLY, subdirName);
  }

  private AnisotropicDiffusionDialog(final ParallelManager manager) {
    this.manager = manager;
    //root
    rootPanel.setBoxLayout(BoxLayout.X_AXIS);
    rootPanel.setBorder(new BeveledBorder("Anisotropic Diffusion").getBorder());
    rootPanel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    //first column
    SpacedPanel pnlFirst = new SpacedPanel();
    pnlFirst.setBoxLayout(BoxLayout.Y_AXIS);
    //volume
    ftfVolume.setShowPartialPath();
    ftfVolume.setFieldEditable(false);
    pnlFirst.add(ftfVolume.getContainer());
    SpacedPanel pnlLoadWithFlipping = new SpacedPanel();
    pnlLoadWithFlipping.setBoxLayout(BoxLayout.X_AXIS);
    pnlLoadWithFlipping.add(cbLoadWithFlipping);
    pnlLoadWithFlipping.addHorizontalGlue();
    pnlFirst.add(pnlLoadWithFlipping);
    //extract
    SpacedPanel pnlExtract = new SpacedPanel();
    pnlExtract.setBoxLayout(BoxLayout.Y_AXIS);
    pnlExtract.setBorder(new EtchedBorder("Extract Test Volume").getBorder());
    pnlExtract.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlTestVolumeRubberband = RubberbandPanel.getInstance(manager,
        ImodManager.VOLUME_KEY, "Test Volume Range:",
        "Get test volume range From 3dmod",
        "Minimum X coordinate on the left side for the test volume range.",
        "Maximum X coordinate on the right side for the test volume range.",
        "The lower Y coordinate for the test volume range.",
        "The upper Y coordinate for the test volume range.",
        "The starting slice for the test volume range.",
        "The ending slice for the test volume range.", btnViewFullVolume);
    pnlExtract.add(pnlTestVolumeRubberband.getContainer());
    SpacedPanel pnlExtractButtons = new SpacedPanel();
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
    //second column
    SpacedPanel pnlSecond = new SpacedPanel();
    pnlSecond.setBoxLayout(BoxLayout.Y_AXIS);
    //varying K
    SpacedPanel pnlVaryingK = new SpacedPanel();
    pnlVaryingK.setBoxLayout(BoxLayout.Y_AXIS);
    pnlVaryingK.setBorder(new EtchedBorder("Find a K Value for Test Volume")
        .getBorder());
    pnlVaryingK.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    SpacedPanel pnlVaryingKFields = new SpacedPanel();
    pnlVaryingKFields.setBoxLayout(BoxLayout.X_AXIS);
    ltfTestKValueList.setTextPreferredWidth(UIParameters.INSTANCE
        .getListWidth());
    pnlVaryingKFields.add(ltfTestKValueList);
    pnlVaryingKFields.add(spTestIteration);
    pnlVaryingK.add(pnlVaryingKFields);
    SpacedPanel pnlVaryingKButtons = new SpacedPanel();
    pnlVaryingKButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingKButtons.addHorizontalGlue();
    btnRunVaryingK.setSize();
    pnlVaryingKButtons.add(btnRunVaryingK);
    pnlVaryingKButtons.addHorizontalGlue();
    btnViewVaryingK.setSize();
    pnlVaryingKButtons.add(btnViewVaryingK);
    pnlVaryingKButtons.addHorizontalGlue();
    pnlVaryingK.add(pnlVaryingKButtons);
    pnlSecond.add(pnlVaryingK);
    //varying iterations
    SpacedPanel pnlVaryingIteration = new SpacedPanel();
    pnlVaryingIteration.setBoxLayout(BoxLayout.Y_AXIS);
    pnlVaryingIteration.setBorder(new EtchedBorder(
        "Find an Iteration Number for Test Volume").getBorder());
    SpacedPanel pnlVaryingIterationFields = new SpacedPanel();
    pnlVaryingIterationFields.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingIterationFields.add(ltfTestKValue);
    ltfTestIterationList.setTextPreferredWidth(UIParameters.INSTANCE
        .getListWidth());
    pnlVaryingIterationFields.add(ltfTestIterationList);
    pnlVaryingIteration.add(pnlVaryingIterationFields);
    SpacedPanel pnlVaryingIterationButtons = new SpacedPanel();
    pnlVaryingIterationButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingIterationButtons.addHorizontalGlue();
    btnRunVaryingIteration.setSize();
    pnlVaryingIterationButtons.add(btnRunVaryingIteration);
    pnlVaryingIterationButtons.addHorizontalGlue();
    btnViewVaryingIteration.setSize();
    pnlVaryingIterationButtons.add(btnViewVaryingIteration);
    pnlVaryingIterationButtons.addHorizontalGlue();
    pnlVaryingIteration.add(pnlVaryingIterationButtons);
    pnlSecond.add(pnlVaryingIteration);
    //filter
    SpacedPanel pnlFilter = new SpacedPanel();
    pnlFilter.setBoxLayout(BoxLayout.Y_AXIS);
    pnlFilter.setBorder(new EtchedBorder("Filter Full Volume").getBorder());
    SpacedPanel pnlFilterFields = new SpacedPanel();
    pnlFilterFields.setBoxLayout(BoxLayout.X_AXIS);
    ltfKValue.setTextPreferredWidth(UIParameters.INSTANCE.getFourDigitWidth());
    pnlFilterFields.add(ltfKValue);
    pnlFilterFields.add(spIteration);
    pnlFilterFields.add(spMemoryPerChunk);
    pnlFilter.add(pnlFilterFields);
    SpacedPanel pnlFilterButtons = new SpacedPanel();
    pnlFilterButtons.setBoxLayout(BoxLayout.X_AXIS);
    btnRunFilterFullVolume.setSize();
    pnlFilterButtons.add(btnRunFilterFullVolume);
    btnViewFilteredVolume.setSize();
    pnlFilterButtons.add(btnViewFilteredVolume);
    btnCleanup.setSize();
    pnlFilterButtons.add(btnCleanup);
    pnlFilter.add(pnlFilterButtons);
    pnlSecond.add(pnlFilter);
    rootPanel.add(pnlSecond);
    setToolTipText();
  }

  public static AnisotropicDiffusionDialog getInstance(ParallelManager manager,
      AxisID axisID) {
    AnisotropicDiffusionDialog instance = new AnisotropicDiffusionDialog(
        manager);
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
    btnRunFilterFullVolume.addActionListener(listener);
    btnViewFilteredVolume.addActionListener(listener);
    btnCleanup.addActionListener(listener);
    rootPanel.addMouseListener(new GenericMouseAdapter(this));
  }

  public void getParameters(ParallelParam param) {
    ProcesschunksParam processchunksParam = (ProcesschunksParam) param;
    processchunksParam.setProcessName(ProcessName.ANISOTROPIC_DIFFUSION);
    processchunksParam.setSubdirName(subdirName);
  }

  public DialogType getDialogType() {
    return DialogType.ANISOTROPIC_DIFFUSION;
  }

  public boolean usingParallelProcessing() {
    return true;
  }

  public Container getContainer() {
    return rootPanel.getContainer();
  }

  public void run3dmod(final Run3dmodButton button,
      final Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
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
    metaData.setKValue(ltfKValue.getText());
    metaData.setIteration(spIteration.getValue());
    metaData.setMemoryPerChunk(spMemoryPerChunk.getValue());
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
    ltfKValue.setText(metaData.getKValue());
    spIteration.setValue(metaData.getIteration());
    spMemoryPerChunk.setValue(metaData.getMemoryPerChunk());
    initSubdir();
  }

  /**
   * Get the parameter values from the panel 
   * @param trimvolParam
   */
  public void getParameters(TrimvolParam param) {
    pnlTestVolumeRubberband.getParameters(param);
    param.setFlippedVolume(cbLoadWithFlipping.isSelected());
    param.setSwapYZ(false);
    param.setRotateX(false);
    param.setConvertToBytes(false);
    param.setInputFileName(ftfVolume.getFileName());
    param.setOutputFileName(new File(subdirName, TEST_VOLUME_NAME).getPath());
  }

  public boolean getParametersForVaryingK(AnisotropicDiffusionParam param) {
    String errorMessage = null;
    errorMessage = param.setKValueList(ltfTestKValueList.getText());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(K_VALUE_LIST_LABEL + errorMessage,
          "Entry Error");
      return false;
    }
    param.setIteration(spTestIteration.getValue());
    //Must use an absolute file when testing file existance.  Changing the
    //working directory by changing the user.dir property doesn't work for
    //relative file paths.  The path looks correct, but File.exists() returns an
    //incorrect result.
    if (!(new File(new File(manager.getPropertyUserDir(), subdirName),
        TEST_VOLUME_NAME).exists())) {
      UIHarness.INSTANCE.openMessageDialog(
          "Test volume has not been created.  Please extract test volume.",
          "Entry Error");
      return false;
    }
    param.setSubdirName(subdirName);
    param.setInputFileName(TEST_VOLUME_NAME);
    param.setCommandMode(AnisotropicDiffusionParam.Mode.VARYING_K);
    return true;
  }

  public void getParameters(AnisotropicDiffusionParam param) {
    param.setSubdirName(subdirName);
    param.setKValue(ltfKValue.getText());
    param.setIteration(spIteration.getValue());
  }

  public void getParameters(ChunksetupParam param) {
    param.setMemoryPerChunk(spMemoryPerChunk.getValue());
    param.setCommandFile(AnisotropicDiffusionParam.getFilterFullFileName());
    param.setSubdirName(subdirName);
    param.setInputFile(ftfVolume.getFileName());
    param.setOutputFile(getOutputFileName(ftfVolume.getFileName()));
  }

  public boolean getParametersForVaryingIteration(
      AnisotropicDiffusionParam param) {
    String errorMessage = null;
    param.setKValue(ltfTestKValue.getText());
    errorMessage = param.setIterationList(ltfTestIterationList.getText());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(ITERATION_LIST_LABEL + errorMessage,
          "Entry Error");
      return false;
    }
    param.setSubdirName(subdirName);
    param.setInputFileName(TEST_VOLUME_NAME);
    return true;
  }

  private void initSubdir() {
    if (subdirName == null) {
      subdirName = "naddir." + ftfVolume.getFileName();
      manager.makeSubdir(subdirName);
    }
  }

  private void deleteSubdir() {
    File subdir = new File(manager.getPropertyUserDir(), subdirName);
    File[] fileList = subdir.listFiles();
    for (int i = 0; i < fileList.length; i++) {
      fileList[i].delete();
    }
    subdir.delete();
    subdirName = null;
  }

  private void action(final ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnExtractTestVolume.getActionCommand())) {
      initSubdir();
      manager.trimVolume();
    }
    else if (command.equals(btnRunVaryingK.getActionCommand())) {
      initSubdir();
      manager.anisotropicDiffusionVaryingK(subdirName);
    }
    else if (command.equals(btnRunVaryingIteration.getActionCommand())) {
      initSubdir();
      manager.anisotropicDiffusionVaryingIteration(subdirName);
    }
    else if (command.equals(btnRunFilterFullVolume.getActionCommand())) {
      initSubdir();
      if (!manager.setupAnisotropicDiffusion()) {
        return;
      }
      manager.chunksetup();
    }
    else if (command.equals(btnCleanup.getActionCommand())) {
      if (subdirName != null) {
        deleteSubdir();
      }
    }
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  private void openVolume() {
    File volume = null;
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    chooser.setFileFilter(new TomogramFileFilter());
    int returnVal = chooser.showOpenDialog(rootPanel.getContainer());
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    volume = chooser.getSelectedFile();
    if (volume == null || volume.isDirectory() || !volume.exists()) {
      UIHarness.INSTANCE.openMessageDialog("Please choose a volume",
          "Entry Error");
      return;
    }
    ftfVolume.setButtonEnabled(false);
    ftfVolume.setFile(volume);
    manager.setNewParamFile(volume);
    initSubdir();
  }

  private void run3dmod(final String command,
      final Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnViewFullVolume.getActionCommand())) {
      manager.imod(ImodManager.VOLUME_KEY, ftfVolume.getFile(), menuOptions,
          cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnViewTestVolume.getActionCommand())) {
      manager.imod(ImodManager.TEST_VOLUME_KEY, new File(subdirName,
          "test.input"), menuOptions, cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnViewVaryingK.getActionCommand())) {
      manager.imodVaryingKValue(ImodManager.VARYING_K_TEST_KEY, menuOptions,
          subdirName, TEST_VOLUME_NAME, cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnViewVaryingIteration.getActionCommand())) {
      manager.imodVaryingIteration(ImodManager.VARYING_ITERATION_TEST_KEY,
          menuOptions, subdirName, TEST_VOLUME_NAME, cbLoadWithFlipping
              .isSelected());
    }
    else if (command.equals(btnViewFilteredVolume.getActionCommand())) {
      manager.imod(ImodManager.ANISOTROPIC_DIFFUSION_VOLUME_KEY, new File(
          getOutputFileName(ftfVolume.getFileAbsolutePath())), menuOptions,
          cbLoadWithFlipping.isSelected());
    }
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
      adaptee.action(event);
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