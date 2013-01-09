package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.ParallelManager;
import etomo.ProcessingMethodMediator;
import etomo.comscript.ParallelParam;
import etomo.logic.DatasetTool;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.ParallelMetaData;
import etomo.type.ProcessingMethod;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ParallelDialog implements AbstractParallelDialog, ProcessInterface {
  public static final String rcsid = "$Id$";

  private static final DialogType DIALOG_TYPE = DialogType.PARALLEL;
  private static final String PROCESS_NAME_LABEL = "Process name: ";

  private final ImageIcon iconFolder = new ImageIcon(
      ClassLoader.getSystemResource("images/openFile.gif"));
  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final JPanel pnlProcessName = new JPanel();
  private final SimpleButton btnChunkComscript = new SimpleButton(iconFolder);
  private final LabeledTextField ltfProcessName = new LabeledTextField(FieldType.STRING,
      PROCESS_NAME_LABEL);
  private final MultiLineButton btnRunProcess = MultiLineButton
      .getToggleButtonInstance("Run Parallel Process");
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();

  private final ParallelActionListener actionListener;
  private final ParallelManager manager;
  private final AxisID axisID;
  private final ProcessingMethodMediator mediator;

  private File workingDir = null;

  private ParallelDialog(ParallelManager manager, AxisID axisID) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: " + DialogType.PARALLEL);
    actionListener = new ParallelActionListener(this);
    this.manager = manager;
    this.axisID = axisID;
    mediator = manager.getProcessingMethodMediator(axisID);
    // process name panel
    btnChunkComscript.setName(PROCESS_NAME_LABEL);
    // pnlProcessName.setBoxLayout(BoxLayout.X_AXIS);
    pnlProcessName.setLayout(layout);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    ltfProcessName.setTextPreferredSize(new Dimension(125 * (int) Math
        .round(UIParameters.INSTANCE.getFontSizeAdjustment()),
        FixedDim.folderButton.height));
    constraints.insets = new Insets(0, 0, 0, -1);
    layout.setConstraints(ltfProcessName.getContainer(), constraints);
    ltfProcessName.setTextPreferredWidth(125);
    pnlProcessName.add(ltfProcessName.getContainer());
    btnChunkComscript.setPreferredSize(FixedDim.folderButton);
    constraints.insets = new Insets(0, -1, 0, 0);
    layout.setConstraints(btnChunkComscript, constraints);
    pnlProcessName.add(btnChunkComscript);
    // root panel
    pnlRoot.setBorder(new BeveledBorder("Parallel Process").getBorder());
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.add(pnlProcessName);
    btnRunProcess.setSize();
    pnlRoot.add(btnRunProcess);
    pnlRoot.alignComponentsX(Component.CENTER_ALIGNMENT);
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

  public static ParallelDialog getInstance(ParallelManager manager, AxisID axisID) {
    ParallelDialog instance = new ParallelDialog(manager, axisID);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    btnChunkComscript.addActionListener(new ChunkComscriptActionListener(this));
    btnRunProcess.addActionListener(actionListener);
  }

  public Container getContainer() {
    return pnlRoot.getContainer();
  }

  public DialogType getDialogType() {
    return DIALOG_TYPE;
  }

  public File getWorkingDir() {
    return workingDir;
  }

  public void done() {
    btnRunProcess.removeActionListener(actionListener);
    manager.getProcessingMethodMediator(axisID).deregister(this);
  }

  public void setParameters(BaseScreenState screenState) {
    btnRunProcess.setButtonState(screenState.getButtonState(btnRunProcess
        .createButtonStateKey(DIALOG_TYPE)));
  }

  public void setParameters(ParallelMetaData metaData) {
    ltfProcessName.setText(metaData.getRootName());
  }

  public void getParameters(BaseScreenState screenState) {
    screenState.setButtonState(btnRunProcess.getButtonStateKey(),
        btnRunProcess.getButtonState());
  }

  public void getParameters(ParallelMetaData metaData) {
    metaData.setRootName(ltfProcessName.getText());
  }

  public void getParameters(ParallelParam param) {
  }

  public void updateDisplay(boolean setupMode) {
    ltfProcessName.setEditable(setupMode);
    btnChunkComscript.setEnabled(setupMode);
  }

  void action(ActionEvent event) {
    try {
      String command = event.getActionCommand();
      if (command.equals(btnRunProcess.getText())) {
        if (ltfProcessName.isEditable()
            && !DatasetTool.validateDatasetName(manager, axisID, workingDir,
                ltfProcessName.getText(true), DataFileType.PARALLEL, null, true)) {
          return;
        }
        manager.processchunks(btnRunProcess, null, ltfProcessName.getText(true), null,
            mediator.getRunMethodForProcessInterface(getProcessingMethod()), DIALOG_TYPE);
      }
    }
    catch (FieldValidationFailedException e) {
    }
  }

  void chunkComscriptAction() {
    File chunkComscript = BaseManager.chunkComscriptAction(pnlRoot.getContainer());
    if (chunkComscript != null) {
      try {
        String comFileName = chunkComscript.getName();
        ltfProcessName.setText(comFileName.substring(0, comFileName.lastIndexOf("-0")));
        workingDir = chunkComscript.getParentFile();
      }
      catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  private void setToolTipText() {
    ltfProcessName
        .setToolTipText("The process name is based on the name of the first comscript (-001.com or -001-sync.com).");
    btnChunkComscript
        .setToolTipText("Selects the first comscript (-001.com or -001-sync.com).");
    btnRunProcess.setToolTipText("Runs the process.");
  }

  private class ParallelActionListener implements ActionListener {
    ParallelDialog adaptee;

    ParallelActionListener(ParallelDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.action(event);
    }
  }

  private class ChunkComscriptActionListener implements ActionListener {
    ParallelDialog adaptee;

    ChunkComscriptActionListener(ParallelDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.chunkComscriptAction();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2011/02/03 06:21:30  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.24  2010/04/28 16:44:30  sueh
 * <p> bug# 1344 Added the output image file type to the manager's
 * <p> processchunks function.
 * <p>
 * <p> Revision 1.23  2009/09/20 21:33:14  sueh
 * <p> bug# 1268 Added timestamp and dialog identification to log.
 * <p>
 * <p> Revision 1.22  2009/02/26 19:56:11  sueh
 * <p> bug# 1185 Allowing a larger string of zeros in the comscript name.
 * <p>
 * <p> Revision 1.21  2009/01/20 20:18:28  sueh
 * <p> bug# 1102 Changing the icon button to a simple button and naming it.
 * <p>
 * <p> Revision 1.20  2008/09/30 22:01:14  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.19  2008/05/03 00:51:26  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.18  2007/12/10 22:43:19  sueh
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * <p> getParameters because it is required and has been added to the
 * <p> ProcesschunksParam constructor.
 * <p>
 * <p> Revision 1.17  2007/11/06 19:55:14  sueh
 * <p> bug# 1047 Switched to getInstance with an addListeners call (more thread-safe).
 * <p>
 * <p> Revision 1.16  2007/08/29 21:49:17  sueh
 * <p> bug# 1041 In chunkComscriptAction, calling BaseManager.chunkComscriptAction.
 * <p>
 * <p> Revision 1.15  2007/08/10 17:05:31  mast
 * <p> Set the process name properly if starting file is -001-sync.com
 * <p>
 * <p> Revision 1.14  2007/02/09 00:50:54  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.13  2006/11/07 22:50:00  sueh
 * <p> bug# 954 Added setToolTipText().
 * <p>
 * <p> Revision 1.12  2006/07/28 19:57:10  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing because isParallel is too similar to a standard get
 * <p> function.
 * <p>
 * <p> Revision 1.11  2006/04/06 20:52:26  sueh
 * <p> bug# 840 When getting the name of the parallel process, don't strip any
 * <p> dash but the last one off.
 * <p>
 * <p> Revision 1.10  2006/03/28 00:55:47  sueh
 * <p> bug# 437 Change getButtonStateKey(DialogType) to
 * <p> createButtonStateKey(DialogType).
 * <p>
 * <p> Revision 1.9  2006/03/20 18:05:32  sueh
 * <p> bug# 835 Dialog handling generic parallel processes.
 * <p> </p>
 */
