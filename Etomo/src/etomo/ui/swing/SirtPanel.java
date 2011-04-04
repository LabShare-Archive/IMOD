package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SirtsetupParam;
import etomo.process.ImodManager;
import etomo.storage.LogFile;
import etomo.storage.SirtOutputFileFilter;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.MetaData;
import etomo.type.PanelId;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;

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
final class SirtPanel implements Run3dmodButtonContainer, SirtsetupDisplay, Observer,
    Expandable {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final CheckBox cbSubarea = new CheckBox("Reconstruct subarea");
  private final LabeledTextField ltfYOffsetOfSubarea = new LabeledTextField(
      " Offset in Y: ");
  private final LabeledTextField ltfSubareaSize = new LabeledTextField("Size in X and Y"
      + ": ");
  private final LabeledTextField ltfLeaveIterations = new LabeledTextField(
      "Iteration #'s to retain: ");
  private final CheckBox cbScaleToInteger = new CheckBox(
      "Scale retained volumes to integers");
  private final ActionListener listener = new SirtActionListener(this);
  private final Run3dmodButton btn3dmodSirt = Run3dmodButton.get3dmodInstance(
      "View Tomogram In 3dmod", this);
  private final CheckBox cbCleanUpPastStart = new CheckBox(
      "Delete existing reconstructions after starting point");
  private final LabeledTextField ltfFlatFilterFraction = new LabeledTextField(
      "Flat filter fraction: ");
  private final SpacedPanel pnlSirtsetupParamsBody = SpacedPanel.getInstance(true);

  private final AxisID axisID;
  private final ApplicationManager manager;
  private final Run3dmodButton btnSirt;
  private final MultiLineButton btnUseSirt;
  private final SirtParent parent;
  private final DialogType dialogType;
  private final RadialPanel radiusAndSigmaPanel;
  private final SirtStartFromPanel sirtStartFromPanel;
  private final PanelHeader sirtSetupParamsHeader;

  private SirtPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final SirtParent parent) {
    this.axisID = axisID;
    this.manager = manager;
    this.parent = parent;
    this.dialogType = dialogType;
    sirtStartFromPanel = SirtStartFromPanel.getInstance(manager, axisID);
    radiusAndSigmaPanel = RadialPanel.getInstance(manager, axisID, PanelId.SIRTSETUP);
    ProcessResultDisplayFactory factory = manager.getProcessResultDisplayFactory(axisID);
    btnSirt = (Run3dmodButton) factory.getSirtsetup();
    btnUseSirt = (MultiLineButton) factory.getUseSirt();
    sirtSetupParamsHeader = PanelHeader.getAdvancedBasicInstance("SIRT", this,
        dialogType, globalAdvancedButton);
  }

  static SirtPanel getInstance(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final SirtParent parent) {
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

  SirtStartFromPanel getSirtStartFromPanel() {
    return sirtStartFromPanel;
  }

  private void createPanel() {
    //initialize
    SpacedPanel pnlSubarea = SpacedPanel.getInstance();
    JPanel pnlSizeAndOffset = new JPanel();
    JPanel pnlSirtsetupParams = new JPanel();
    JPanel pnlScaleToInteger = new JPanel();
    JPanel pnlCleanUpPastStart = new JPanel();
    JPanel pnlButtons = new JPanel();
    btnSirt.setSize();
    btnSirt.setContainer(this);
    btnSirt.setDeferred3dmodButton(btn3dmodSirt);
    btn3dmodSirt.setSize();
    btnUseSirt.setSize();
    //root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(pnlSubarea.getContainer());
    pnlRoot.add(pnlSirtsetupParams);
    pnlRoot.add(pnlButtons);
    //SIRT panel
    //Subarea panel
    pnlSubarea.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSubarea.setBorder(BorderFactory.createEtchedBorder());
    pnlSubarea.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlSubarea.add(cbSubarea);
    pnlSubarea.add(pnlSizeAndOffset);
    //Offset and size panel
    pnlSizeAndOffset.setLayout(new BoxLayout(pnlSizeAndOffset, BoxLayout.X_AXIS));
    pnlSizeAndOffset.add(ltfSubareaSize.getContainer());
    pnlSizeAndOffset.add(ltfYOffsetOfSubarea.getContainer());
    //SIRT params panel
    pnlSirtsetupParams.setLayout(new BoxLayout(pnlSirtsetupParams, BoxLayout.Y_AXIS));
    pnlSirtsetupParams.setBorder(BorderFactory.createEtchedBorder());
    pnlSirtsetupParams.add(sirtSetupParamsHeader.getContainer());
    pnlSirtsetupParams.add(pnlSirtsetupParamsBody.getContainer());
    //SIRT params body panel
    pnlSirtsetupParamsBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSirtsetupParamsBody.add(radiusAndSigmaPanel.getRoot());
    pnlSirtsetupParamsBody.add(ltfLeaveIterations);
    pnlSirtsetupParamsBody.add(pnlScaleToInteger);
    pnlSirtsetupParamsBody.add(pnlCleanUpPastStart);
    pnlSirtsetupParamsBody.add(ltfFlatFilterFraction);
    pnlSirtsetupParamsBody.add(sirtStartFromPanel.getRoot());
    //ScaleToInteger panel
    pnlScaleToInteger.setLayout(new BoxLayout(pnlScaleToInteger, BoxLayout.X_AXIS));
    pnlScaleToInteger.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlScaleToInteger.add(cbScaleToInteger);
    pnlScaleToInteger.add(Box.createHorizontalGlue());
    //CleanUpPastStart panel
    pnlCleanUpPastStart.setLayout(new BoxLayout(pnlCleanUpPastStart, BoxLayout.X_AXIS));
    pnlCleanUpPastStart.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlCleanUpPastStart.add(cbCleanUpPastStart);
    pnlCleanUpPastStart.add(Box.createHorizontalGlue());
    //Buttons panel
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(btnSirt.getComponent());
    pnlButtons.add(btn3dmodSirt.getComponent());
    pnlButtons.add(btnUseSirt.getComponent());
    updateDisplay();
  }

  private void addListeners() {
    cbSubarea.addActionListener(listener);
    btnSirt.addActionListener(listener);
    btn3dmodSirt.addActionListener(listener);
    btnUseSirt.addActionListener(listener);
  }

  public void update(final Observable observable, final Object arg) {
    if (observable instanceof SirtStartFromPanel) {
      //This observable signals when the return value of sirtStartFromPanel.isResume()
      //may have changed.
      updateDisplay();
    }
  }

  private void updateDisplay() {
    boolean resume = sirtStartFromPanel.isResume();
    boolean subarea = cbSubarea.isSelected();
    cbSubarea.setEnabled(!resume);
    ltfSubareaSize.setEnabled(subarea && !resume);
    ltfYOffsetOfSubarea.setEnabled(subarea && !resume);
    radiusAndSigmaPanel.setEnabled(!resume);
  }

  public void msgSirtSucceeded() {
    sirtStartFromPanel.msgSirtSucceeded();
  }

  public final void setMethod(final TomogramGenerationDialog.MethodEnum method) {
    boolean sirt = method == TomogramGenerationDialog.MethodEnum.SIRT;
    pnlRoot.setVisible(sirt);
  }

  final void done() {
    btnSirt.removeActionListener(listener);
    btnUseSirt.removeActionListener(listener);
  }

  boolean isResume() {
    return sirtStartFromPanel.isResume();
  }

  void getParameters(final ReconScreenState screenState) {
    btnSirt.setButtonState(screenState.getButtonState(btnSirt.getButtonStateKey()));
    btnUseSirt.setButtonState(screenState.getButtonState(btnUseSirt.getButtonStateKey()));
    sirtSetupParamsHeader.getState(screenState.getTomoGenSirtHeaderState());
  }
  
  final void setParameters(final ReconScreenState screenState) {
    sirtSetupParamsHeader.setState(screenState.getTomoGenSirtHeaderState());
    btnSirt.setButtonState(screenState.getButtonState(btnSirt.getButtonStateKey()));
    btnUseSirt.setButtonState(screenState.getButtonState(btnUseSirt
        .getButtonStateKey()));
  }

  void getParameters(final MetaData metaData) {
    metaData.setGenSubarea(axisID, cbSubarea.isSelected());
    metaData.setGenSubareaSize(axisID, ltfSubareaSize.getText());
    metaData.setGenYOffsetOfSubarea(axisID, ltfYOffsetOfSubarea.getText());
    sirtStartFromPanel.getParameters(metaData);
  }

  void setParameters(final ConstMetaData metaData) {
    cbSubarea.setSelected(metaData.isGenSubarea(axisID));
    ltfSubareaSize.setText(metaData.getGenSubareaSize(axisID));
    ltfYOffsetOfSubarea.setText(metaData.getGenYOffsetOfSubarea(axisID));
    sirtStartFromPanel.setParameters(metaData);
  }

  public boolean getParameters(final SirtsetupParam param) {
    try {
      if (ltfLeaveIterations.isEmpty()) {
        UIHarness.INSTANCE.openMessageDialog(manager, ltfLeaveIterations.getLabel()
            + " is empty.", "Entry Error", axisID);
        return false;
      }
      param.setLeaveIterations(ltfLeaveIterations.getText());
      if (cbSubarea.isSelected()) {
        if (ltfSubareaSize.isEmpty()) {
          UIHarness.INSTANCE.openMessageDialog(manager, ltfSubareaSize.getLabel()
              + " is empty.", "Entry Error", axisID);
          return false;
        }
        param.setSubareaSize(ltfSubareaSize.getText());
        param.setYOffsetOfSubarea(ltfYOffsetOfSubarea.getText());
      }
      else {
        param.resetSubareaSize();
        param.resetYOffsetOfSubarea();
      }
      param.setScaleToInteger(cbScaleToInteger.isSelected());
      radiusAndSigmaPanel.getParameters(param);
      param.setCleanUpPastStart(cbCleanUpPastStart.isSelected());
      param.setFlatFilterFraction(ltfFlatFilterFraction.getText());
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, ltfSubareaSize.getLabel()
          + " is an invalid list of integers.", "Entry Error", axisID);
      return false;
    }
    if (!sirtStartFromPanel.getParameters(param)) {
      return false;
    }
    return true;
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
    sirtStartFromPanel.setParameters(param);
  }

  public void openFilesInImod(final Run3dmodMenuOptions run3dmodMenuOptions) {
    JFileChooser chooser = new FileChooser(new File(manager.getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    chooser.setMultiSelectionEnabled(true);
    chooser.setFileFilter(new SirtOutputFileFilter(manager, axisID, true));
    int returnVal = chooser.showOpenDialog(pnlRoot);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    File[] fileList = chooser.getSelectedFiles();
    if (fileList == null || fileList.length == 0) {
      return;
    }
    manager.openFilesInImod(axisID, ImodManager.SIRT_KEY, fileList, run3dmodMenuOptions);
  }

  public void useSirt() {
    JFileChooser chooser = new FileChooser(new File(manager.getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    chooser.setFileFilter(new SirtOutputFileFilter(manager, axisID, true));
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

  public final void expand(final GlobalExpandButton button) {
  }

  public final void expand(final ExpandButton button) {
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

  public final void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
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
  final void action(final String actionCommand,
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
    else {
      updateDisplay();
    }
  }

  /**
   * Right mouse button context menu
   */
  void popUpContextMenu(final String anchor, final Component rootPanel,
      final MouseEvent mouseEvent) {
    String[] manPagelabel = { "Sirtsetup", "3dmod" };
    String[] manPage = { "sirtsetup", "3dmod.html" };
    String[] logFileLabel = { "Sirtsetup" };
    String[] logFile = new String[1];
    logFile[0] = "sirtsetup" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent, anchor,
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
        axisID);
  }

  void setToolTipText() {
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
        .setToolTipText("Opens a file chooser for picker SIRT iteration files to bring up in 3dmod");
    cbCleanUpPastStart.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.CLEAN_UP_PAST_START_KEY));
    btnSirt
        .setToolTipText("Run sirtsetup, and then run the resulting .com files with processchunks.");
    btnUseSirt.setToolTipText("Use a SIRT result as the tomogram");
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
}
