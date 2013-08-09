package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import etomo.ApplicationManager;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SirtsetupParam;
import etomo.process.ImodManager;
import etomo.storage.LogFile;
import etomo.storage.SirtOutputFileFilter;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.PanelId;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2010</p>
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
* <p> Revision 1.7  2011/05/03 03:36:27  sueh
* <p> bug# 1416 Added checkpointing  and listening for subarea fields.  Removed  StateChangedReporter.
* <p>
* <p> Revision 1.6  2011/04/25 23:49:22  sueh
* <p> bug# 1416 Moved SirtStartFromPanel functionality to this class.  Handling one file when opening in 3dmod and using a SIRT tomogram as the dataset tomogram.  Removed the observer/observable code because their are enough observers and the response isn't standard enough.  Talking directly to TiltPanel.
* <p>
* <p> Revision 1.5  2011/04/04 17:33:42  sueh
* <p> bug# 1416 Added/modified sirt and useSirt buttons; cleanUpPastStart, scaleToInteger, and subarea
* <p> checkboxes, dialogType, flatFilterFraction, offsetInXAndY, subareaSize, yOffsetOfSubarea fields; parent.
* <p> Standardized names to match sirtsetup.  Added an advanced/basic header for the sirtsetup parameter panel.
* <p>
* <p> Revision 1.3  2011/02/03 06:22:16  sueh
* <p> bug# 1422 Control of the processing method has been centralized in the
* <p> processing method mediator class.  Implementing ProcessInterface.
* <p> Supplying processes with the current processing method.
* <p>
* <p> Revision 1.2  2010/12/05 05:17:34  sueh
* <p> bug# 1421 Main class for running SIRT.
* <p>
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p> </p>
*/
final class SirtPanel implements Run3dmodButtonContainer, SirtsetupDisplay, Expandable,
    FieldObserver {
  public static final String rcsid = "$Id$";

  private static final String RESUME_FROM_LAST_ITERATION_LABEL = "Resume from last iteration";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox cbSubarea = new CheckBox("Reconstruct subarea");
  private final LabeledTextField ltfYOffsetOfSubarea = LabeledTextField
      .getNumericInstance(" Offset in Y: ");
  private final LabeledTextField ltfSubareaSize = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Size in X and Y" + ": ");
  private final LabeledTextField ltfLeaveIterations = new LabeledTextField(
      FieldType.INTEGER_LIST, "Iteration #'s to retain: ");
  private final CheckBox cbScaleToInteger = new CheckBox(
      "Scale retained volumes to integers");
  private final ActionListener listener = new SirtActionListener(this);
  private final Run3dmodButton btn3dmodSirt = Run3dmodButton.get3dmodInstance(
      "View Tomogram(s) In 3dmod", this);
  private final CheckBox cbCleanUpPastStart = new CheckBox(
      "Delete existing reconstructions after starting point");
  private final LabeledTextField ltfFlatFilterFraction = new LabeledTextField(
      FieldType.FLOATING_POINT, "Flat filter fraction: ");
  private final SpacedPanel pnlSirtsetupParamsBody = SpacedPanel.getInstance(true);
  private final ButtonGroup bgStartingIteration = new ButtonGroup();
  private final RadioButton rbStartFromZero = new RadioButton("Start from beginning",
      bgStartingIteration);
  private final RadioButton rbResumeFromLastIteration = new RadioButton(
      RESUME_FROM_LAST_ITERATION_LABEL, bgStartingIteration);
  private final RadioButton rbResumeFromIteration = new RadioButton(
      "Go back, resume from iteration:", bgStartingIteration);
  private final ComboBox cmbResumeFromIteration = ComboBox
      .getUnlabeledInstance(rbResumeFromIteration.getText());
  private final List<ResumeObserver> resumeObservers = new ArrayList();
  private CheckBox cbSkipVertSliceOutput = new CheckBox(
      "Do not make vertical slice output files used for resuming");

  private final AxisID axisID;
  private final ApplicationManager manager;
  private final Run3dmodButton btnSirt;
  private final MultiLineButton btnUseSirt;
  private final TomogramGenerationDialog parent;
  private final DialogType dialogType;
  private final RadialPanel radiusAndSigmaPanel;
  private final PanelHeader sirtSetupParamsHeader;

  private int numFiles = 0;
  private boolean differentFromCheckpointFlag = false;

  private SirtPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final TomogramGenerationDialog parent) {
    this.axisID = axisID;
    this.manager = manager;
    this.parent = parent;
    this.dialogType = dialogType;
    radiusAndSigmaPanel = RadialPanel.getInstance(manager, axisID, PanelId.SIRTSETUP);
    ProcessResultDisplayFactory factory = manager.getProcessResultDisplayFactory(axisID);
    btnSirt = (Run3dmodButton) factory.getSirtsetup();
    btnUseSirt = (MultiLineButton) factory.getUseSirt();
    sirtSetupParamsHeader = PanelHeader.getAdvancedBasicInstance("SIRT", this,
        dialogType, globalAdvancedButton);
  }

  static SirtPanel getInstance(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final TomogramGenerationDialog parent) {
    SirtPanel instance = new SirtPanel(manager, axisID, dialogType, globalAdvancedButton,
        parent);
    instance.createPanel();
    instance.setToolTipText();
    instance.addListeners();
    return instance;
  }

  Component getRoot() {
    return pnlRoot;
  }

  private void createPanel() {
    // initialize
    SpacedPanel pnlSubarea = SpacedPanel.getInstance();
    JPanel pnlSizeAndOffset = new JPanel();
    JPanel pnlSirtsetupParams = new JPanel();
    JPanel pnlScaleToInteger = new JPanel();
    JPanel pnlSkipVertSliceOutput = new JPanel();
    JPanel pnlCleanUpPastStart = new JPanel();
    JPanel pnlStartFrom = new JPanel();
    JPanel pnlStartFromZero = new JPanel();
    JPanel pnlResumeFromLastIteration = new JPanel();
    JPanel pnlResumeFromIteration = new JPanel();
    JPanel pnlButtons = new JPanel();
    btnSirt.setSize();
    btnSirt.setContainer(this);
    btnSirt.setDeferred3dmodButton(btn3dmodSirt);
    btn3dmodSirt.setSize();
    btnUseSirt.setSize();
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(pnlSubarea.getContainer());
    pnlRoot.add(pnlSirtsetupParams);
    pnlRoot.add(pnlButtons);
    // SIRT panel
    // Subarea panel
    pnlSubarea.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSubarea.setBorder(BorderFactory.createEtchedBorder());
    pnlSubarea.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlSubarea.add(cbSubarea);
    pnlSubarea.add(pnlSizeAndOffset);
    // Offset and size panel
    pnlSizeAndOffset.setLayout(new BoxLayout(pnlSizeAndOffset, BoxLayout.X_AXIS));
    pnlSizeAndOffset.add(ltfSubareaSize.getContainer());
    pnlSizeAndOffset.add(ltfYOffsetOfSubarea.getContainer());
    // SIRT params panel
    pnlSirtsetupParams.setLayout(new BoxLayout(pnlSirtsetupParams, BoxLayout.Y_AXIS));
    pnlSirtsetupParams.setBorder(BorderFactory.createEtchedBorder());
    pnlSirtsetupParams.add(sirtSetupParamsHeader.getContainer());
    pnlSirtsetupParams.add(pnlSirtsetupParamsBody.getContainer());
    // SIRT params body panel
    pnlSirtsetupParamsBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSirtsetupParamsBody.add(radiusAndSigmaPanel.getRoot());
    pnlSirtsetupParamsBody.add(ltfLeaveIterations);
    pnlSirtsetupParamsBody.add(pnlScaleToInteger);
    pnlSirtsetupParamsBody.add(pnlSkipVertSliceOutput);
    pnlSirtsetupParamsBody.add(pnlCleanUpPastStart);
    pnlSirtsetupParamsBody.add(ltfFlatFilterFraction);
    pnlSirtsetupParamsBody.add(pnlStartFrom);
    // ScaleToInteger panel
    pnlScaleToInteger.setLayout(new BoxLayout(pnlScaleToInteger, BoxLayout.X_AXIS));
    pnlScaleToInteger.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlScaleToInteger.add(cbScaleToInteger);
    pnlScaleToInteger.add(Box.createHorizontalGlue());
    // SkipVertSliceOutput panel
    pnlSkipVertSliceOutput.setLayout(new BoxLayout(pnlSkipVertSliceOutput,
        BoxLayout.X_AXIS));
    pnlSkipVertSliceOutput.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlSkipVertSliceOutput.add(cbSkipVertSliceOutput);
    pnlSkipVertSliceOutput.add(Box.createHorizontalGlue());
    // CleanUpPastStart panel
    pnlCleanUpPastStart.setLayout(new BoxLayout(pnlCleanUpPastStart, BoxLayout.X_AXIS));
    pnlCleanUpPastStart.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlCleanUpPastStart.add(cbCleanUpPastStart);
    pnlCleanUpPastStart.add(Box.createHorizontalGlue());
    // start from panel
    pnlStartFrom.setLayout(new BoxLayout(pnlStartFrom, BoxLayout.Y_AXIS));
    pnlStartFrom.add(pnlStartFromZero);
    pnlStartFrom.add(pnlResumeFromLastIteration);
    pnlStartFrom.add(pnlResumeFromIteration);
    // StartFromZero panel
    pnlStartFromZero.setLayout(new BoxLayout(pnlStartFromZero, BoxLayout.X_AXIS));
    pnlStartFromZero.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlStartFromZero.add(rbStartFromZero.getComponent());
    pnlStartFromZero.add(Box.createHorizontalGlue());
    // ResumeFromLastIteration panel
    pnlResumeFromLastIteration.setLayout(new BoxLayout(pnlResumeFromLastIteration,
        BoxLayout.X_AXIS));
    pnlResumeFromLastIteration.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlResumeFromLastIteration.add(rbResumeFromLastIteration.getComponent());
    pnlResumeFromLastIteration.add(Box.createHorizontalGlue());
    // Resume from iteration panel
    pnlResumeFromIteration.setLayout(new BoxLayout(pnlResumeFromIteration,
        BoxLayout.X_AXIS));
    pnlResumeFromIteration.add(rbResumeFromIteration.getComponent());
    pnlResumeFromIteration.add(cmbResumeFromIteration.getComponent());
    // Buttons panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(btnSirt.getComponent());
    pnlButtons.add(btn3dmodSirt.getComponent());
    pnlButtons.add(btnUseSirt.getComponent());
    // defaults
    rbStartFromZero.setSelected(true);
    cbCleanUpPastStart.setSelected(true);
    updateDisplay();
  }

  private void addListeners() {
    btnSirt.addActionListener(listener);
    btn3dmodSirt.addActionListener(listener);
    btnUseSirt.addActionListener(listener);
    rbStartFromZero.addActionListener(listener);
    rbResumeFromLastIteration.addActionListener(listener);
    rbResumeFromIteration.addActionListener(listener);
    cbSubarea.addActionListener(listener);
    SirtDocumentListener documentListener = new SirtDocumentListener(this);
    ltfSubareaSize.addDocumentListener(documentListener);
    ltfYOffsetOfSubarea.addDocumentListener(documentListener);
  }

  void addResumeObserver(ResumeObserver resumeObserver) {
    resumeObservers.add(resumeObserver);
    resumeChanged();
  }

  public void msgFieldChanged(final boolean differentFromCheckpoint) {
    differentFromCheckpointFlag = differentFromCheckpoint;
    updateDisplay();
  }

  private void updateDisplay() {
    // Update checkpointed fields - disabled fields are not checked for checkpoint
    // difference.
    boolean subarea = cbSubarea.isSelected();
    ltfSubareaSize.setEnabled(subarea);
    ltfYOffsetOfSubarea.setEnabled(subarea);
    // Enable resume if there are files to resume from and this class has no checkpoint
    // differences, and classes that this class is observing have no checkpoint
    // differences.
    boolean enableResume = numFiles > 0 && !isDifferentFromCheckpoint()
        && !differentFromCheckpointFlag;
    rbResumeFromLastIteration.setEnabled(enableResume);
    rbResumeFromIteration.setEnabled(enableResume);
    cmbResumeFromIteration.setComboBoxEnabled(enableResume && rbResumeFromIteration.isSelected());
    // Don't allow the resume radio buttons to be selected when they are disabled
    if (!enableResume
        && (rbResumeFromLastIteration.isSelected() || rbResumeFromIteration.isSelected())) {
      rbStartFromZero.setSelected(true);
    }
    resumeChanged();
    boolean resume = isResume();
    // Correct checkpointed fields now that resume is available
    ltfSubareaSize.setEnabled(subarea && !resume);
    ltfYOffsetOfSubarea.setEnabled(subarea && !resume);
    radiusAndSigmaPanel.setEnabled(!resume);
    cmbResumeFromIteration.setComboBoxEnabled(rbResumeFromIteration.isEnabled()
        && rbResumeFromIteration.isSelected());
  }

  private Boolean isResumeEnabled() {
    return rbResumeFromLastIteration.isEnabled();
  }

  Boolean isResume() {
    return rbResumeFromLastIteration.isSelected() || rbResumeFromIteration.isSelected();
  }

  void msgSirtSucceeded() {
    loadResumeFrom();
  }

  void msgMethodChanged() {
    pnlRoot.setVisible(parent.isSirt());
  }

  void done() {
    btnSirt.removeActionListener(listener);
    btnUseSirt.removeActionListener(listener);
  }

  void getParameters(final ReconScreenState screenState) {
    btnSirt.setButtonState(screenState.getButtonState(btnSirt.getButtonStateKey()));
    btnUseSirt.setButtonState(screenState.getButtonState(btnUseSirt.getButtonStateKey()));
    sirtSetupParamsHeader.getState(screenState.getTomoGenSirtHeaderState());
  }

  void setParameters(final ReconScreenState screenState) {
    sirtSetupParamsHeader.setState(screenState.getTomoGenSirtHeaderState());
    btnSirt.setButtonState(screenState.getButtonState(btnSirt.getButtonStateKey()));
    btnUseSirt.setButtonState(screenState.getButtonState(btnUseSirt.getButtonStateKey()));
  }

  void getParameters(final MetaData metaData) {
    metaData.setGenSubarea(axisID, cbSubarea.isSelected());
    metaData.setGenSubareaSize(axisID, ltfSubareaSize.getText());
    metaData.setGenYOffsetOfSubarea(axisID, ltfYOffsetOfSubarea.getText());
  }

  void setParameters(final ConstMetaData metaData) {
    cbSubarea.setSelected(metaData.isGenSubarea(axisID));
    ltfSubareaSize.setText(metaData.getGenSubareaSize(axisID));
    ltfYOffsetOfSubarea.setText(metaData.getGenYOffsetOfSubarea(axisID));
    loadResumeFrom();
  }

  public boolean getParameters(final SirtsetupParam param, final boolean doValidation) {
    try {
      try {
        if (ltfLeaveIterations.isEmpty()) {
          UIHarness.INSTANCE.openMessageDialog(manager, ltfLeaveIterations.getLabel()
              + " is empty.", "Entry Error", axisID);
          return false;
        }
        param.setLeaveIterations(ltfLeaveIterations.getText(doValidation));
        if (cbSubarea.isSelected()) {
          if (ltfSubareaSize.isEmpty()) {
            UIHarness.INSTANCE.openMessageDialog(manager, ltfSubareaSize.getLabel()
                + " is empty.", "Entry Error", axisID);
            return false;
          }
          param.setSubareaSize(ltfSubareaSize.getText(doValidation));
          param.setYOffsetOfSubarea(ltfYOffsetOfSubarea.getText(doValidation));
        }
        else {
          param.resetSubareaSize();
          param.resetYOffsetOfSubarea();
        }
        param.setScaleToInteger(cbScaleToInteger.isSelected());
        if (!radiusAndSigmaPanel.getParameters(param, doValidation)) {
          return false;
        }
        param.setCleanUpPastStart(cbCleanUpPastStart.isSelected());
        param.setFlatFilterFraction(ltfFlatFilterFraction.getText(doValidation));
        param.setSkipVertSliceOutput(cbSkipVertSliceOutput.isSelected());
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE.openMessageDialog(manager, ltfSubareaSize.getLabel()
            + " is an invalid list of integers.", "Entry Error", axisID);
        return false;
      }
      if (rbStartFromZero.isSelected()) {
        param.setStartFromZero(true);
        param.resetResumeFromIteration();
      }
      else if (rbResumeFromLastIteration.isEnabled()
          && rbResumeFromLastIteration.isSelected()) {
        param.setStartFromZero(false);
        param.resetResumeFromIteration();
      }
      else if (rbResumeFromIteration.isEnabled() && rbResumeFromIteration.isSelected()) {
        param.setStartFromZero(false);
        param.setResumeFromIteration((ConstEtomoNumber) cmbResumeFromIteration
            .getSelectedItem());
      }
      else {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Please select an enabled starting option.", "Entry Error", axisID);
        return false;
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void setParameters(final SirtsetupParam param) {
    ltfLeaveIterations.setText(param.getLeaveIterations());
    if (!param.isSubareaSizeNull()) {
      ltfSubareaSize.setText(param.getSubareaSize());
    }
    if (!param.isYOffsetOfSubareaNull()) {
      ltfYOffsetOfSubarea.setText(param.getYOffsetOfSubarea());
    }
    cbScaleToInteger.setSelected(!param.isScaleToIntegerNull());
    radiusAndSigmaPanel.setParameters(param);
    cbCleanUpPastStart.setSelected(param.isCleanUpPastStart());
    ltfFlatFilterFraction.setText(param.getFlatFilterFraction());
    cbSkipVertSliceOutput.setSelected(param.isSkipVertSliceOutput());
    if (param.isStartFromZero()) {
      rbStartFromZero.setSelected(true);
    }
    else if (!param.isResumeFromIterationNull()) {
      rbResumeFromIteration.setSelected(true);
    }
    else {
      rbResumeFromLastIteration.setSelected(true);
    }
    updateDisplay();
  }

  /**
   * Load all of the .srecdd files in the current directory (any number of digits).  Save
   * the digits in the ResumeFromIteration pulldown list.
   */
  private void loadResumeFrom() {
    // Clear pulldown list.
    cmbResumeFromIteration.removeAllItems();
    // Get file names.
    boolean subarea = cbSubarea.isSelected();
    SirtOutputFileFilter filter;
    if (subarea) {
      filter = SirtOutputFileFilter.getSubareaInstance(manager, axisID, false);
    }
    else {
      filter = SirtOutputFileFilter.getFullInstance(manager, axisID, false);
    }
    String[] fileNameList = new File(manager.getPropertyUserDir()).list(filter);
    // Extract iteration numbers, sort them, and add them to the pulldown list.
    int[] fileNumberList = null;
    // Ignore .sint## files - they cannot be resumed from.
    FileType fileType;
    if (subarea) {
      fileType = FileType.SIRT_SUBAREA_OUTPUT_TEMPLATE;
    }
    else {
      fileType = FileType.SIRT_OUTPUT_TEMPLATE;
    }
    String templateExt = fileType.getExtension(manager);
    if (fileNameList != null && fileNameList.length > 0) {
      fileNumberList = new int[fileNameList.length];
      for (int i = 0; i < fileNumberList.length; i++) {
        EtomoNumber fileNumber = new EtomoNumber();
        fileNumber.set(fileNameList[i]
            .substring(fileNameList[i].lastIndexOf(templateExt)).substring(
                templateExt.length()));
        // SirtOutputFileFilter only accepts files that end in a valid integer, so assume
        // the number is valid.
        fileNumberList[i] = fileNumber.getInt();
      }
      Arrays.sort(fileNumberList);
      // Add sorted numbers to the pulldown list.
      for (int i = fileNumberList.length - 1; i >= 0; i--) {
        EtomoNumber fileNumber = new EtomoNumber();
        fileNumber.set(fileNumberList[i]);
        if (i == fileNumberList.length - 1) {
          rbResumeFromLastIteration.setText(RESUME_FROM_LAST_ITERATION_LABEL + ": "
              + fileNumber);
        }
        cmbResumeFromIteration.addItem(fileNumber);
      }
      if (fileNumberList.length > 1) {
        cmbResumeFromIteration.setSelectedIndex(1);
      }
    }
    else {
      rbResumeFromLastIteration.setText(RESUME_FROM_LAST_ITERATION_LABEL);
    }
    // Keep numFiles up to date
    if (fileNumberList == null || fileNumberList.length == 0) {
      numFiles = 0;
    }
    else {
      numFiles = fileNumberList.length;
    }
    updateDisplay();
  }

  /**
   * Ask the user to one or more files to open in 3dmod.  If only one file is available,
   * open in 3dmod without asking.
   * @param run3dmodMenuOptions
   */
  private void openFilesInImod(final Run3dmodMenuOptions run3dmodMenuOptions) {
    // Don't open the file chooser if there is only one file to choose
    SirtOutputFileFilter sirtOutputFileFilter = new SirtOutputFileFilter(manager, axisID,
        true, true, true);
    File[] fileList = new File(manager.getPropertyUserDir())
        .listFiles((FilenameFilter) sirtOutputFileFilter);
    if (fileList == null || fileList.length != 1) {
      JFileChooser chooser = new FileChooser(new File(manager.getPropertyUserDir()));
      chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
      chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
      chooser.setMultiSelectionEnabled(true);
      chooser.setFileFilter(sirtOutputFileFilter);
      int returnVal = chooser.showOpenDialog(pnlRoot);
      if (returnVal != JFileChooser.APPROVE_OPTION) {
        return;
      }
      fileList = chooser.getSelectedFiles();
      if (fileList == null || fileList.length == 0) {
        return;
      }
    }
    manager.openFilesInImod(axisID, ImodManager.SIRT_KEY, fileList, run3dmodMenuOptions);
  }

  /**
   * Ask the user to pick a file to use.  If only one file is available to pick, ask the
   * user if they want to use that file.
   */
  public void useSirt() {
    SirtOutputFileFilter sirtOutputFileFilter = new SirtOutputFileFilter(manager, axisID,
        true, true, true);
    File[] fileList = new File(manager.getPropertyUserDir())
        .listFiles((FilenameFilter) sirtOutputFileFilter);
    if (fileList != null && fileList.length == 1) {
      if (UIHarness.INSTANCE.openYesNoDialog(manager, "Use " + fileList[0].getName()
          + " as the tomogram?", axisID)) {
        manager.useSirt(btnUseSirt, fileList[0], btnSirt.getUnformattedLabel(), axisID,
            dialogType);
      }
      return;
    }
    JFileChooser chooser = new FileChooser(new File(manager.getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    chooser.setFileFilter(sirtOutputFileFilter);
    int returnVal = chooser.showOpenDialog(pnlRoot);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    File file = chooser.getSelectedFile();
    if (file == null || !file.exists()) {
      return;
    }
    manager.useSirt(btnUseSirt, file, btnSirt.getUnformattedLabel(), axisID, dialogType);
  }

  void updateAdvanced(final boolean advanced) {
    ltfFlatFilterFraction.setVisible(advanced);
  }

  public void expand(final GlobalExpandButton button) {
  }

  public void expand(final ExpandButton button) {
    if (sirtSetupParamsHeader != null) {
      if (sirtSetupParamsHeader.equalsOpenClose(button)) {
        pnlSirtsetupParamsBody.setVisible(button.isExpanded());
      }
      else if (sirtSetupParamsHeader.equalsAdvancedBasic(button)) {
        updateAdvanced(button.isExpanded());
      }
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void resumeChanged() {
    boolean resume = isResume();
    Iterator<ResumeObserver> i = resumeObservers.iterator();
    while (i.hasNext()) {
      i.next().msgResumeChanged(resume);
    }
  }

  void checkpoint(TomogramState state) {
    ltfSubareaSize.checkpoint(state.getGenSirtsetupSubareaSize(axisID));
    ltfYOffsetOfSubarea.checkpoint(state.getGenSirtsetupyOffsetOfSubarea(axisID));
    updateDisplay();
  }

  /**
   * Checks the subarea fields.  Returns true if any of them are different from their
   * checkpoint or haven't been checkpointed. It is unnecessary to check whether a field
   * is active, because a disabled or invisible field returns false.
   * @return
   */
  boolean isDifferentFromCheckpoint() {
    if (ltfSubareaSize.isDifferentFromCheckpoint()
        || ltfYOffsetOfSubarea.isDifferentFromCheckpoint()) {
      return true;
    }
    return false;
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param actionCommand
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  private void action(final String actionCommand,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(btnSirt.getActionCommand())) {
      manager.sirtsetup(axisID, btnSirt, null, dialogType, parent.getProcessingMethod(),
          this);
    }
    else if (actionCommand.equals(btn3dmodSirt.getActionCommand())) {
      openFilesInImod(run3dmodMenuOptions);
    }
    else if (actionCommand.equals(btnUseSirt.getActionCommand())) {
      useSirt();
    }
    else if (actionCommand.equals(rbStartFromZero.getActionCommand())
        || actionCommand.equals(rbResumeFromLastIteration.getActionCommand())
        || actionCommand.equals(rbResumeFromIteration.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(cbSubarea.getActionCommand())) {
      loadResumeFrom();
    }
  }

  private void documentAction() {
    updateDisplay();
  }

  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.SIRTSETUP, axisID);
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
    cbSubarea.setToolTipText("Subarea to use from the aligned stack");
    ltfYOffsetOfSubarea.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.Y_OFFSET_OF_SUBAREA_KEY));
    ltfSubareaSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.SUBAREA_SIZE_KEY));
    ltfLeaveIterations.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.LEAVE_ITERATIONS_KEY));
    cbScaleToInteger.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.SCALE_TO_INTEGER_KEY));
    ltfFlatFilterFraction.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.FLAT_FILTER_FRACTION_KEY));
    btn3dmodSirt
        .setToolTipText("Opens a file chooser for picking SIRT iteration files to open together in 3dmod");
    cbCleanUpPastStart.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.CLEAN_UP_PAST_START_KEY));
    cbSkipVertSliceOutput.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.SKIP_VERT_SLICE_OUTPUT_KEY));
    rbStartFromZero.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.START_FROM_ZERO_KEY));
    rbResumeFromLastIteration
        .setToolTipText("Iterate from the last existing reconstruction.");
    String tooltip = EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.RESUME_FROM_ITERATION_KEY);
    rbResumeFromIteration.setToolTipText(tooltip);
    cmbResumeFromIteration.setToolTipText(tooltip);
    btnSirt
        .setToolTipText("Run sirtsetup, and then run the resulting .com files with processchunks.");
    btnUseSirt
        .setToolTipText("Use a SIRT result as the tomogram (change the extension to .rec).");

  }

  private static final class SirtActionListener implements ActionListener {
    private final SirtPanel adaptee;

    private SirtActionListener(final SirtPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  private static final class SirtDocumentListener implements DocumentListener {
    private final SirtPanel adaptee;

    private SirtDocumentListener(final SirtPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void changedUpdate(final DocumentEvent event) {
      adaptee.documentAction();
    }

    public void insertUpdate(final DocumentEvent event) {
      adaptee.documentAction();
    }

    public void removeUpdate(final DocumentEvent event) {
      adaptee.documentAction();
    }
  }

}
