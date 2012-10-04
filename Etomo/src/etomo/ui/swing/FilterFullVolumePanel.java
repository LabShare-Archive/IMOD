package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ParallelManager;
import etomo.comscript.AnisotropicDiffusionParam;
import etomo.comscript.ChunksetupParam;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.ParallelMetaData;
import etomo.type.Run3dmodMenuOptions;
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
 * <p> Revision 1.3  2011/02/22 18:09:55  sueh
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
 * <p> Revision 1.2  2010/04/28 16:37:07  sueh
 * <p> bug# 1344 In action removed call to manager.setupAnisotropicDiffusion.
 * <p>
 * <p> Revision 1.1  2010/01/21 21:31:25  sueh
 * <p> bug# 1305 Factored filter full volume panel out of AnisotropicDiffusionDialog.
 * <p> </p>
 */
final class FilterFullVolumePanel implements Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  static final String FILTER_FULL_VOLUME_LABEL = "Filter Full Volume";
  static final String MEMORY_PER_CHUNK_LABEL = "Memory per chunk";
  static final int MEMORY_PER_CHUNK_DEFAULT = 14 * ChunksetupParam.MEMORY_TO_VOXEL;
  static final String CLEANUP_LABEL = "Clean Up Subdirectory";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final Run3dmodButton btnRunFilterFullVolume = Run3dmodButton
      .getDeferred3dmodInstance(FILTER_FULL_VOLUME_LABEL, this);
  private final LabeledTextField ltfKValue = new LabeledTextField(
      FieldType.FLOATING_POINT, "K value: ");
  private final Spinner spIteration = Spinner.getLabeledInstance("Iterations: ", 10, 1,
      200);
  private final Spinner spMemoryPerChunk = Spinner.getLabeledInstance(
      MEMORY_PER_CHUNK_LABEL + " (MB): ", MEMORY_PER_CHUNK_DEFAULT,
      ChunksetupParam.MEMORY_TO_VOXEL, 30 * ChunksetupParam.MEMORY_TO_VOXEL,
      ChunksetupParam.MEMORY_TO_VOXEL);
  private final Run3dmodButton btnViewFilteredVolume = Run3dmodButton.get3dmodInstance(
      "View Filtered Volume", this);
  private final MultiLineButton btnCleanup = new MultiLineButton(CLEANUP_LABEL);
  private final CheckBox cbOverlapTimesFour = new CheckBox(
      "Overlap chunks by 4 times # of iterations");

  private final DialogType dialogType;
  private final ParallelManager manager;
  private final FilterFullVolumeParent parent;

  private FilterFullVolumePanel(final ParallelManager manager,
      final DialogType dialogType, final FilterFullVolumeParent parent) {
    this.dialogType = dialogType;
    this.manager = manager;
    this.parent = parent;
  }

  static FilterFullVolumePanel getInstance(final ParallelManager manager,
      final DialogType dialogType, final FilterFullVolumeParent parent) {
    FilterFullVolumePanel instance = new FilterFullVolumePanel(manager, dialogType,
        parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    ActionListener actionListener = new FilterFullVolumeActionListener(this);
    btnRunFilterFullVolume.addActionListener(actionListener);
    btnViewFilteredVolume.addActionListener(actionListener);
    btnCleanup.addActionListener(actionListener);
  }

  private void createPanel() {
    // initialization
    ltfKValue.setTextPreferredWidth(UIParameters.INSTANCE.getFourDigitWidth());
    btnRunFilterFullVolume.setSize();
    btnViewFilteredVolume.setSize();
    btnCleanup.setSize();
    // local panels
    SpacedPanel pnlFields = SpacedPanel.getInstance();
    SpacedPanel pnlButtons = SpacedPanel.getInstance();
    JPanel pnlCheckBox = new JPanel();
    // root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new EtchedBorder(FILTER_FULL_VOLUME_LABEL).getBorder());
    pnlRoot.add(pnlFields);
    pnlRoot.add(pnlCheckBox);
    pnlRoot.add(pnlButtons);
    // fields panel
    pnlFields.setBoxLayout(BoxLayout.X_AXIS);
    pnlFields.add(ltfKValue);
    pnlFields.add(spIteration);
    pnlFields.add(spMemoryPerChunk);
    pnlFields.add(Box.createHorizontalGlue());
    // checkbox panel
    pnlCheckBox.setLayout(new BoxLayout(pnlCheckBox, BoxLayout.X_AXIS));
    pnlCheckBox.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlCheckBox.add(cbOverlapTimesFour);
    pnlCheckBox.add(Box.createHorizontalGlue());
    // buttons panel
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    btnRunFilterFullVolume.setDeferred3dmodButton(btnViewFilteredVolume);
    pnlButtons.add(btnRunFilterFullVolume);
    pnlButtons.add(btnViewFilteredVolume);
    pnlButtons.add(btnCleanup);
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void getParameters(final ParallelMetaData metaData) {
    metaData.setKValue(ltfKValue.getText());
    metaData.setIteration(spIteration.getValue());
    metaData.setMemoryPerChunk(spMemoryPerChunk.getValue());
    metaData.setOverlapTimesFour(cbOverlapTimesFour.isSelected());
  }

  Number getMemoryPerChunk() {
    return spMemoryPerChunk.getValue();
  }

  void setParameters(final ParallelMetaData metaData) {
    ltfKValue.setText(metaData.getKValue());
    spIteration.setValue(metaData.getIteration());
    spMemoryPerChunk.setValue(metaData.getMemoryPerChunk());
    cbOverlapTimesFour.setSelected(metaData.isOverlapTimesFour());
  }

  boolean getParameters(final AnisotropicDiffusionParam param, final boolean doValidation) {
    try {
      param.setKValue(ltfKValue.getText(doValidation));
      param.setIteration(spIteration.getValue());
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void getParameters(final ChunksetupParam param) {
    param.setMemoryPerChunk(spMemoryPerChunk.getValue());
    param.setOverlap(spIteration.getValue());
    param.setOverlapTimesFour(cbOverlapTimesFour.isSelected());
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnRunFilterFullVolume.getActionCommand())) {
      if (!parent.initSubdir()) {
        return;
      }
      manager.chunksetup(null, deferred3dmodButton, run3dmodMenuOptions, dialogType,
          manager.getProcessingMethodMediator(AxisID.ONLY)
              .getRunMethodForProcessInterface(parent.getProcessingMethod()));
    }
    else if (command.equals(btnCleanup.getActionCommand())) {
      parent.cleanUp();
    }
    else if (command.equals(btnViewFilteredVolume.getActionCommand())) {
      manager.imod(FileType.ANISOTROPIC_DIFFUSION_OUTPUT, run3dmodMenuOptions,
          parent.isLoadWithFlipping());
    }
  }

  private void setTooltips() {
    btnRunFilterFullVolume
        .setToolTipText("Run diffusion on the full volume in chunks, creates "
            + "filename.nad");
    ltfKValue.setToolTipText("K threshold value for running on full volume");
    spIteration.setToolTipText("Number of iterations to run on full volume");
    spMemoryPerChunk.setToolTipText("Maximum memory in megabytes to use while running "
        + "diffusion on one chunk. Reduce if there is less memory per "
        + "processor or if you want to break the job into more chunks.  The"
        + " number of voxels in each chunk will be 1/36 of this memory "
        + "limit or less.");
    btnViewFilteredVolume.setToolTipText("View filtered volume (filename.nad) in 3dmod");
    btnCleanup.setToolTipText("Remove subdirectory with all temporary and test files "
        + "(naddir.filename).");
    cbOverlapTimesFour
        .setToolTipText("Increase overlap to 4 times # of iterations (default is "
            + "equal to # of iterations) to eliminate minor effects of cutting "
            + "volume into chunks.");
  }

  private static final class FilterFullVolumeActionListener implements ActionListener {
    private final FilterFullVolumePanel filterFullVolumePanel;

    private FilterFullVolumeActionListener(
        final FilterFullVolumePanel filterFullVolumePanel) {
      this.filterFullVolumePanel = filterFullVolumePanel;
    }

    public void actionPerformed(final ActionEvent event) {
      filterFullVolumePanel.action(event.getActionCommand(), null, null);
    }
  }
}
