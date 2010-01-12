package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstWarpVolParam;
import etomo.comscript.WarpVolParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.ImageFileType;
import etomo.type.MetaData;
import etomo.type.Run3dmodMenuOptions;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
 * <p> Revision 1.6  2009/12/19 01:15:15  sueh
 * <p> bug# 1294 Change getFileTypeForSurfaceModel to getInputFileType.
 * <p>
 * <p> Revision 1.5  2009/11/20 17:12:02  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.  Added isMenuSaveEnabled to
 * <p> allow a save function to have the same limits as the save menu option.
 * <p>
 * <p> Revision 1.4  2009/10/01 18:50:38  sueh
 * <p> bug# 1239 Added getFlattenWarpDisplay.
 * <p>
 * <p> Revision 1.3  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.2  2009/06/11 16:51:57  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Implemented
 * <p> WarpVolDisplay.
 * <p>
 * <p> Revision 1.1  2009/06/05 02:11:23  sueh
 * <p> bug# 1219 Panel that can run warpvol using a file called flatten.com.
 * <p> </p>
 */
final class FlattenPanel implements Run3dmodButtonContainer, FlattenWarpParent,
    WarpVolDisplay {
  public static final String rcsid = "$Id$";

  private static final String OUTPUT_SIZE_Z_LABEL = "Output thickness in Z";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  ActionListener actionListener = new FlattenPanelActionListener(this);
  private final ButtonGroup bgInputFile = new ButtonGroup();
  private final RadioButton rbInputFileTrimVol = new RadioButton(
      "Flatten the trimvol output", bgInputFile);
  private final RadioButton rbInputFileSqueezeVol = new RadioButton(
      "Flatten the squeezevol output", bgInputFile);
  private final CheckBox cbInterpolationOrderLinear = new CheckBox(
      "Linear interpolation");
  private final LabeledTextField ltfOutputSizeZ = new LabeledTextField(
      OUTPUT_SIZE_Z_LABEL + ": ");
  private final Run3dmodButton btnImodFlatten = Run3dmodButton
      .get3dmodInstance("Open Flattened Tomogram", this);
  private final FileTextField ftfTemporaryDirectory = new FileTextField(
      "Temporary directory:");

  private final FlattenWarpPanel flattenWarpPanel;
  private final Run3dmodButton btnFlatten;

  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;

  private FlattenPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    flattenWarpPanel = FlattenWarpPanel.getInstance(manager, axisID,
        dialogType, this);
    btnFlatten = (Run3dmodButton) manager
        .getProcessResultDisplayFactory(axisID).getFlatten();
  }

  static FlattenPanel getInstance(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    FlattenPanel instance = new FlattenPanel(manager, axisID, dialogType);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  static Run3dmodButton getFlattenDisplay(DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Flatten", dialogType);
  }

  private void addListeners() {
    btnFlatten.addActionListener(actionListener);
    btnImodFlatten.addActionListener(actionListener);
    ftfTemporaryDirectory.addActionListener(actionListener);
  }

  void done() {
    btnFlatten.removeActionListener(actionListener);
    flattenWarpPanel.done();
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  private void createPanel() {
    JPanel pnlInputFile = new JPanel();
    JPanel pnlInterpolationOrder = new JPanel();
    SpacedPanel pnlFlatten = SpacedPanel.getInstance();
    //initialize
    rbInputFileTrimVol.setSelected(true);
    btnFlatten.setContainer(this);
    btnFlatten.setDeferred3dmodButton(btnImodFlatten);
    btnFlatten.setSize();
    btnImodFlatten.setSize();
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new BeveledBorder("Flatten Volume").getBorder());
    pnlRoot.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlRoot.add(pnlInputFile);
    pnlRoot.add(flattenWarpPanel.getComponent());
    pnlRoot.add(pnlInterpolationOrder);
    pnlRoot.add(ltfOutputSizeZ.getContainer());
    pnlRoot.add(ftfTemporaryDirectory);
    pnlRoot.add(pnlFlatten);
    //Input file panel
    pnlInputFile.setLayout(new BoxLayout(pnlInputFile, BoxLayout.Y_AXIS));
    pnlInputFile.setBorder(new BeveledBorder("Set Input File").getBorder());
    pnlInputFile.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlInputFile.add(rbInputFileTrimVol.getComponent());
    pnlInputFile.add(rbInputFileSqueezeVol.getComponent());
    //Interpolation order panel
    pnlInterpolationOrder.setLayout(new BoxLayout(pnlInterpolationOrder,
        BoxLayout.X_AXIS));
    pnlInterpolationOrder.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlInterpolationOrder.add(cbInterpolationOrderLinear);
    //Flatten panel
    pnlFlatten.setBoxLayout(BoxLayout.X_AXIS);
    pnlFlatten.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlFlatten.add(btnFlatten.getComponent());
    pnlFlatten.add(btnImodFlatten.getComponent());
  }

  public ImageFileType getInputFileType() {
    if (rbInputFileTrimVol.isSelected()) {
      return ImageFileType.TRIM_VOL_OUTPUT;
    }
    return ImageFileType.SQUEEZE_VOL_OUTPUT;
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnFlatten.getActionCommand())) {
      manager.flatten(btnFlatten, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType, axisID, this);
    }
    else if (command.equals(btnImodFlatten.getActionCommand())) {
      manager.imodFlatten(run3dmodMenuOptions, axisID);
    }
    else if (command.equals(ftfTemporaryDirectory.getActionCommand())) {
      temporaryDirectoryAction();
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  private void temporaryDirectoryAction() {
    //  Open up the file chooser in the current working directory
    JFileChooser chooser = new FileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(pnlRoot.getContainer());
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File directory = chooser.getSelectedFile();
      try {
        ftfTemporaryDirectory.setText(directory.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  void setParameters(ConstMetaData metaData) {
    rbInputFileTrimVol.setSelected(metaData.isPostFlattenWarpInputTrimVol());
    if (!rbInputFileTrimVol.isSelected()) {
      rbInputFileSqueezeVol.setSelected(true);
    }
    flattenWarpPanel.setParameters(metaData);
  }

  FlattenWarpDisplay getFlattenWarpDisplay() {
    return flattenWarpPanel;
  }

  void getParameters(MetaData metaData) {
    metaData.setPostFlattenWarpInputTrimVol(rbInputFileTrimVol.isSelected());
    flattenWarpPanel.getParameters(metaData);
  }

  public boolean getParameters(WarpVolParam param) {
    param.setInterpolationOrderLinear(cbInterpolationOrderLinear.isSelected());
    String errorMessage = param.setOutputSizeZ(ltfOutputSizeZ.getText());
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog("Error in " + OUTPUT_SIZE_Z_LABEL
          + ":  " + errorMessage, "Entry Error", axisID, manager
          .getManagerKey());
      return false;
    }
    //The model contains coordinates so it can match either input file.
    param.setInputFile(getInputFileType());
    param.setTemporaryDirectory(ftfTemporaryDirectory.getText());
    return true;
  }

  void setParameters(ConstWarpVolParam param) {
    cbInterpolationOrderLinear.setSelected(param.isInterpolationOrderLinear());
    ltfOutputSizeZ.setText(param.getOutputSizeZ());
    ftfTemporaryDirectory.setText(param.getTemporaryDirectory());
  }

  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.WARP_VOL, axisID,
          manager.getManagerKey());
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
    if (autodoc != null) {
      rbInputFileTrimVol.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          WarpVolParam.INPUT_FILE_OPTION));
      rbInputFileSqueezeVol.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          WarpVolParam.INPUT_FILE_OPTION));
      cbInterpolationOrderLinear.setToolTipText(EtomoAutodoc.getTooltip(
          autodoc, WarpVolParam.INTERPOLATION_ORDER_OPTION));
      ltfOutputSizeZ.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          WarpVolParam.OUTPUT_SIZE_X_Y_Z_OPTION));
      ftfTemporaryDirectory.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          WarpVolParam.TEMPORARY_DIRECTORY_OPTION));
    }
    btnFlatten.setToolTipText("Run warpvol.");
    btnImodFlatten.setToolTipText("Open warpvol output in 3dmod.");
  }

  private final class FlattenPanelActionListener implements ActionListener {
    private final FlattenPanel adaptee;

    private FlattenPanelActionListener(final FlattenPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
