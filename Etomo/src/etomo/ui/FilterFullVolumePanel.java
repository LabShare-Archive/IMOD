package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ParallelManager;
import etomo.comscript.AnisotropicDiffusionParam;
import etomo.comscript.ChunksetupParam;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.ParallelMetaData;
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
  private final LabeledTextField ltfKValue = new LabeledTextField("K value: ");
  private final Spinner spIteration = Spinner.getLabeledInstance(
      "Iterations: ", 10, 1, 200);
  private final Spinner spMemoryPerChunk = Spinner.getLabeledInstance(
      MEMORY_PER_CHUNK_LABEL + " (MB): ", MEMORY_PER_CHUNK_DEFAULT,
      ChunksetupParam.MEMORY_TO_VOXEL, 30 * ChunksetupParam.MEMORY_TO_VOXEL,
      ChunksetupParam.MEMORY_TO_VOXEL);
  private final Run3dmodButton btnViewFilteredVolume = Run3dmodButton
      .get3dmodInstance("View Filtered Volume", this);
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
    FilterFullVolumePanel instance = new FilterFullVolumePanel(manager,
        dialogType, parent);
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
    //initialization
    ltfKValue.setTextPreferredWidth(UIParameters.INSTANCE.getFourDigitWidth());
    btnRunFilterFullVolume.setSize();
    btnViewFilteredVolume.setSize();
    btnCleanup.setSize();
    //local panels
    SpacedPanel pnlFields = SpacedPanel.getInstance();
    SpacedPanel pnlButtons = SpacedPanel.getInstance();
    JPanel pnlCheckBox = new JPanel();
    //root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new EtchedBorder(FILTER_FULL_VOLUME_LABEL).getBorder());
    pnlRoot.add(pnlFields);
    pnlRoot.add(pnlCheckBox);
    pnlRoot.add(pnlButtons);
    //fields panel
    pnlFields.setBoxLayout(BoxLayout.X_AXIS);
    pnlFields.add(ltfKValue);
    pnlFields.add(spIteration);
    pnlFields.add(spMemoryPerChunk);
    //checkbox panel
    pnlCheckBox.setLayout(new BoxLayout(pnlCheckBox,BoxLayout.X_AXIS));
    pnlCheckBox.add(cbOverlapTimesFour);
    //buttons panel
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

  void getParameters(final AnisotropicDiffusionParam param) {
    param.setKValue(ltfKValue.getText());
    param.setIteration(spIteration.getValue());
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
      manager.chunksetup(null, deferred3dmodButton, run3dmodMenuOptions,
          dialogType);
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
    spMemoryPerChunk
        .setToolTipText("Maximum memory in megabytes to use while running "
            + "diffusion on one chunk. Reduce if there is less memory per "
            + "processor or if you want to break the job into more chunks.  The"
            + " number of voxels in each chunk will be 1/36 of this memory "
            + "limit or less.");
    btnViewFilteredVolume
        .setToolTipText("View filtered volume (filename.nad) in 3dmod");
    btnCleanup
        .setToolTipText("Remove subdirectory with all temporary and test files "
            + "(naddir.filename).");
    cbOverlapTimesFour
        .setToolTipText("Increase overlap to 4 times # of iterations (default is "
            + "equal to # of iterations) to eliminate minor effects of cutting "
            + "volume into chunks.");
  }

  private static final class FilterFullVolumeActionListener implements
      ActionListener {
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
