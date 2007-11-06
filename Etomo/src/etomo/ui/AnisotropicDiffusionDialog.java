package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
 * <p> $Log$ </p>
 */
public final class AnisotropicDiffusionDialog implements
    AbstractParallelDialog, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public static final int MEMORY_PER_CHUNK_DEFAULT = 14 * ChunksetupParam.MEMORY_TO_VOXEL;

  private static final String K_VALUE_LIST_LABEL = "List of K values: ";
  private static final String ITERATION_LIST_LABEL = "List of iterations: ";
  private static final String K_VALUE_LABEL = "K values: ";
  private static final String ITERATION_LABEL = "Iterations: ";

  static final String TEST_VOLUME_NAME = "test.input";

  private final SpacedPanel rootPanel = new SpacedPanel();
  private final Run3dmodButton btnOpenVolume = new Run3dmodButton(
      "Open Volume", this);
  private final FileTextField ftfVolume = new FileTextField("Pick a volume");
  private final MultiLineButton btnExtractTestVolume = new MultiLineButton(
      "Extract Test Volume");
  private final Run3dmodButton btnOpenTestVolume = new Run3dmodButton(
      "Open Test Volume", this);
  private final CheckBox cbLoadWithFlipping = new CheckBox("Load with flipping");
  private final LabeledTextField ltfKValueList = new LabeledTextField(
      K_VALUE_LIST_LABEL);
  private final Spinner spIteration = Spinner.getLabeledInstance(
      ITERATION_LABEL, 10, 1, 200);
  private final MultiLineButton btnRunVaryingK = new MultiLineButton(
      "Run with Different K Values");
  private final MultiLineButton btnOpenVaryingK = new MultiLineButton(
      "Open Different K Values Test Results");
  private final LabeledTextField ltfKValue = new LabeledTextField(K_VALUE_LABEL);
  private final LabeledTextField ltfIterationList = new LabeledTextField(
      ITERATION_LIST_LABEL);
  private MultiLineButton btnRunVaryingIteration = new MultiLineButton(
      "Run with Different Iterations");
  private MultiLineButton btnOpenVaryingIteration = new MultiLineButton(
      "Open Different Iteration Test Results");
  private final LabeledTextField ltfFinalKValue = new LabeledTextField(
      K_VALUE_LABEL);
  private final Spinner spFinalIteration = Spinner.getLabeledInstance(
      ITERATION_LABEL, 10, 1, 200);
  private MultiLineButton btnRunFinalVolume = new MultiLineButton(
      "Create Final Volume");
  private MultiLineButton btnOpenFinalVolume = new MultiLineButton(
      "Open Final Volume");
  private final Spinner spMemoryPerChunk = Spinner.getLabeledInstance(
      "Memory per chunk (Mb): ", MEMORY_PER_CHUNK_DEFAULT,
      ChunksetupParam.MEMORY_TO_VOXEL, 30 * ChunksetupParam.MEMORY_TO_VOXEL,
      ChunksetupParam.MEMORY_TO_VOXEL);

  private final RubberbandPanel pnlTestVolumeRubberband;
  private final ParallelManager manager;

  private String subdirName = null;

  private AnisotropicDiffusionDialog(final ParallelManager manager) {
    this.manager = manager;
    rootPanel.setBoxLayout(BoxLayout.X_AXIS);
    rootPanel.setBorder(new BeveledBorder("Anisotropic Diffusion").getBorder());
    rootPanel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    SpacedPanel pnlFirst = new SpacedPanel();
    pnlFirst.setBoxLayout(BoxLayout.Y_AXIS);
    ftfVolume.setFieldEditable(false);
    pnlFirst.add(ftfVolume.getContainer());
    pnlFirst.add(cbLoadWithFlipping);
    SpacedPanel pnlExtract = new SpacedPanel();
    pnlExtract.setBoxLayout(BoxLayout.Y_AXIS);
    pnlExtract.setBorder(new EtchedBorder("Extract Test Volume").getBorder());
    pnlExtract.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlTestVolumeRubberband = RubberbandPanel.getInstance(manager,
        ImodManager.VOLUME_KEY, "Test volume range:",
        "Get test volume range From 3dmod",
        "Minimum X corduroy on the left side for the test volume range.",
        "Maximum X coordinate on the right side for the test volume range.",
        "The lower Y coordinate for the test volume range.",
        "The upper Y coordinate for the test volume range.",
        "The starting Z coordinate for the test volume range.",
        "The ending Z coordinate for the test volume range.", btnOpenVolume);
    pnlExtract.add(pnlTestVolumeRubberband.getContainer());
    SpacedPanel pnlButtons = new SpacedPanel();
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    btnExtractTestVolume.setSize();
    pnlButtons.add(btnExtractTestVolume);
    btnOpenTestVolume.setSize();
    pnlButtons.add(btnOpenTestVolume);
    pnlExtract.add(pnlButtons);
    pnlFirst.add(pnlExtract);
    SpacedPanel pnlVaryingK = new SpacedPanel();
    pnlVaryingK.setBoxLayout(BoxLayout.Y_AXIS);
    pnlVaryingK.setBorder(new EtchedBorder("Find a K Value for Test Volume")
        .getBorder());
    pnlVaryingK.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    SpacedPanel pnlVaryingKFields = new SpacedPanel();
    pnlVaryingKFields.setBoxLayout(BoxLayout.X_AXIS);
    ltfKValueList.setTextPreferredWidth(UIParameters.INSTANCE.getListWidth());
    pnlVaryingKFields.add(ltfKValueList);
    pnlVaryingKFields.add(spIteration);
    pnlVaryingK.add(pnlVaryingKFields);
    SpacedPanel pnlVaryingKButtons = new SpacedPanel();
    pnlVaryingKButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingKButtons.addRigidArea(FixedDim.x20_y0);
    btnRunVaryingK.setSize();
    pnlVaryingKButtons.add(btnRunVaryingK);
    pnlVaryingKButtons.addRigidArea(FixedDim.x20_y0);
    btnOpenVaryingK.setSize();
    pnlVaryingKButtons.add(btnOpenVaryingK);
    pnlVaryingKButtons.addRigidArea(FixedDim.x20_y0);
    pnlVaryingK.add(pnlVaryingKButtons);
    pnlFirst.add(pnlVaryingK);
    rootPanel.add(pnlFirst);
    SpacedPanel pnlSecond = new SpacedPanel();
    pnlSecond.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel pnlVaryingIteration = new SpacedPanel();
    pnlVaryingIteration.setBoxLayout(BoxLayout.Y_AXIS);
    pnlVaryingIteration.setBorder(new EtchedBorder(
        "Find an Iteration Number for Test Volume").getBorder());
    SpacedPanel pnlVaryingIterationFields = new SpacedPanel();
    pnlVaryingIterationFields.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingIterationFields.add(ltfKValue);
    ltfIterationList
        .setTextPreferredWidth(UIParameters.INSTANCE.getListWidth());
    pnlVaryingIterationFields.add(ltfIterationList);
    pnlVaryingIteration.add(pnlVaryingIterationFields);
    SpacedPanel pnlVaryingIterationButtons = new SpacedPanel();
    pnlVaryingIterationButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlVaryingIterationButtons.addRigidArea(FixedDim.x20_y0);
    btnRunVaryingIteration.setSize();
    pnlVaryingIterationButtons.add(btnRunVaryingIteration);
    pnlVaryingIterationButtons.addRigidArea(FixedDim.x20_y0);
    btnOpenVaryingIteration.setSize();
    pnlVaryingIterationButtons.add(btnOpenVaryingIteration);
    pnlVaryingIterationButtons.addRigidArea(FixedDim.x20_y0);
    pnlVaryingIteration.add(pnlVaryingIterationButtons);
    pnlSecond.add(pnlVaryingIteration);
    SpacedPanel pnlFinal = new SpacedPanel();
    pnlFinal.setBoxLayout(BoxLayout.Y_AXIS);
    pnlFinal.setBorder(new EtchedBorder("Create Volume").getBorder());
    SpacedPanel pnlFinalFields = new SpacedPanel();
    pnlFinalFields.setBoxLayout(BoxLayout.X_AXIS);
    ltfFinalKValue.setTextPreferredWidth(UIParameters.INSTANCE
        .getFourDigitWidth());
    pnlFinalFields.add(ltfFinalKValue);
    pnlFinalFields.add(spFinalIteration);
    pnlFinalFields.add(spMemoryPerChunk);
    pnlFinal.add(pnlFinalFields);
    SpacedPanel pnlFinalButtons = new SpacedPanel();
    pnlFinalButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlFinalButtons.addRigidArea(FixedDim.x20_y0);
    btnRunFinalVolume.setSize();
    pnlFinalButtons.add(btnRunFinalVolume);
    pnlFinalButtons.addRigidArea(FixedDim.x20_y0);
    btnOpenFinalVolume.setSize();
    pnlFinalButtons.add(btnOpenFinalVolume);
    pnlFinalButtons.addRigidArea(FixedDim.x20_y0);
    pnlFinal.add(pnlFinalButtons);
    pnlSecond.add(pnlFinal);
    rootPanel.add(pnlSecond);
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
    btnOpenVolume.addActionListener(listener);
    btnExtractTestVolume.addActionListener(listener);
    btnOpenTestVolume.addActionListener(listener);
    btnRunVaryingK.addActionListener(listener);
    btnOpenVaryingK.addActionListener(listener);
    btnRunVaryingIteration.addActionListener(listener);
    btnOpenVaryingIteration.addActionListener(listener);
    btnRunFinalVolume.addActionListener(listener);
    btnOpenFinalVolume.addActionListener(listener);
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
    metaData.setVolume(ftfVolume.getText());
  }

  public void getParameters(final ParallelMetaData metaData) {
    metaData.setLoadWithFlipping(cbLoadWithFlipping.isSelected());
    pnlTestVolumeRubberband.getParameters(metaData);
    metaData.setKValueList(ltfKValueList.getText());
    metaData.setIteration(spIteration.getValue());
    metaData.setKValue(ltfKValue.getText());
    metaData.setIterationList(ltfIterationList.getText());
    metaData.setFinalKValue(ltfFinalKValue.getText());
    metaData.setFinalIteration(spFinalIteration.getValue());
    metaData.setMemoryPerChunk(spMemoryPerChunk.getValue());
  }

  public void setParameters(final ParallelMetaData metaData) {
    ftfVolume.setButtonEnabled(false);
    ftfVolume.setText(metaData.getVolume());
    cbLoadWithFlipping.setSelected(metaData.isLoadWithFlipping());
    pnlTestVolumeRubberband.setParameters(metaData);
    ltfKValueList.setText(metaData.getKValueList());
    spIteration.setValue(metaData.getIteration());
    ltfKValue.setText(metaData.getKValue());
    ltfIterationList.setText(metaData.getIterationList());
    ltfFinalKValue.setText(metaData.getFinalKValue());
    spFinalIteration.setValue(metaData.getFinalIteration());
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
    errorMessage = param.setKValueList(ltfKValueList.getText());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(K_VALUE_LIST_LABEL + errorMessage,
          "Entry Error");
      return false;
    }
    param.setIteration(spIteration.getValue());
    if (!(new File(subdirName, TEST_VOLUME_NAME).exists())) {
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
    param.setKValue(ltfFinalKValue.getText());
    param.setIteration(spFinalIteration.getValue());
  }

  public void getParameters(ChunksetupParam param) {
    param.setMemoryPerChunk(spMemoryPerChunk.getValue());
    param.setCommandFile(AnisotropicDiffusionParam.getFullnadFileName());
    param.setSubdirName(subdirName);
    param.setInputFile(ftfVolume.getFileName());
    param.setOutputFile(getOutputFileName(ftfVolume.getFileName()));
  }

  public boolean getParametersForVaryingIteration(
      AnisotropicDiffusionParam param) {
    String errorMessage = null;
    param.setKValue(ltfKValue.getText());
    errorMessage = param.setIterationList(ltfIterationList.getText());
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
    else if (command.equals(btnRunFinalVolume.getActionCommand())) {
      initSubdir();
      if (!manager.setupAnisotropicDiffusion()) {
        return;
      }
      manager.chunksetup();
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
    if (command.equals(btnOpenVolume.getActionCommand())) {
      manager.imod(ImodManager.VOLUME_KEY, ftfVolume.getFile(), menuOptions,
          cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnOpenTestVolume.getActionCommand())) {
      manager.imod(ImodManager.TEST_VOLUME_KEY, new File(subdirName,
          "test.input"), menuOptions, cbLoadWithFlipping.isSelected());
    }
    else if (command.equals(btnOpenVaryingK.getActionCommand())) {
      manager.imodVaryingKValue(ImodManager.VARYING_K_TEST_KEY, menuOptions,
          subdirName);
    }
    else if (command.equals(btnOpenVaryingIteration.getActionCommand())) {
      manager.imodVaryingIteration(ImodManager.VARYING_ITERATION_TEST_KEY,
          menuOptions, subdirName);
    }
    else if (command.equals(btnOpenFinalVolume.getActionCommand())) {
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