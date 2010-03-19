package etomo.ui;

import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstFindBeads3dParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.NewstParam;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.ProcessName;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.type.ViewType;

/**
 * <p>Description: Panel to use findbeads3d to find all the beads in an existing
 * or newly created aligned stack which is used to generate a tomogram.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 3.3  2010/03/03 05:01:27  sueh
 * <p> bug# 1311 Changed FileType.NEWST_OR_BLEND_OUTPUT to
 * <p> ALIGNED_STACK.  Added file types for patch tracking.
 * <p>
 * <p> Revision 3.2  2009/09/22 23:54:16  sueh
 * <p> bug# 1269 Added setEnabledTiltParameters.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
final class Beads3dFindPanel implements NewstackOrBlendmont3dFindParent,
    Tilt3dFindParent, Expandable {
  public static final String rcsid = "$Id$";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final JPanel pnlGenerateTomogramBody = new JPanel();
  private final NewstackOrBlendmont3dFindPanel newstackOrBlendmont3dFindPanel;
  private final Tilt3dFindPanel tilt3dFindPanel;
  private final FindBeads3dPanel findBeads3dPanel;
  private final ReprojectModelPanel reprojectModelPanel;
  private final ApplicationManager manager;
  private final AxisID axisID;
  private final DialogType dialogType;
  private final PanelHeader header;
  private final Beads3dFindParent parent;

  Beads3dFindPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, Beads3dFindParent parent,
      GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.parent = parent;
    header = PanelHeader.getInstance("Align Stack and Create Tomogram", this,
        dialogType);
    if (manager.getMetaData().getViewType() == ViewType.MONTAGE) {
      newstackOrBlendmont3dFindPanel = Blendmont3dFindPanel.getInstance(
          manager, axisID, dialogType, this);
    }
    else {
      newstackOrBlendmont3dFindPanel = Newstack3dFindPanel.getInstance(manager,
          axisID, dialogType, this);
    }
    tilt3dFindPanel = Tilt3dFindPanel.getInstance(manager, axisID, dialogType,
        this, newstackOrBlendmont3dFindPanel.get3dmodButton());
    findBeads3dPanel = FindBeads3dPanel.getInstance(manager, axisID,
        dialogType, globalAdvancedButton);
    reprojectModelPanel = ReprojectModelPanel.getInstance(manager, axisID,
        dialogType);
  }

  static Beads3dFindPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType, Beads3dFindParent parent,
      GlobalExpandButton globalAdvancedButton) {
    Beads3dFindPanel instance = new Beads3dFindPanel(manager, axisID,
        dialogType, parent, globalAdvancedButton);
    instance.createPanel();
    return instance;
  }

  void done() {
    tilt3dFindPanel.done();
    findBeads3dPanel.done();
    reprojectModelPanel.done();
  }

  void updateAdvanced(boolean advanced) {
    findBeads3dPanel.updateAdvanced(advanced);
  }

  public void expand(GlobalExpandButton button) {
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlGenerateTomogramBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  private void createPanel() {
    //Local panels
    JPanel pnlGenerateTomogram = new JPanel();
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.add(pnlGenerateTomogram);
    pnlRoot.add(findBeads3dPanel.getComponent());
    pnlRoot.add(reprojectModelPanel.getComponent());
    //Generate tomogram panel
    pnlGenerateTomogram.setLayout(new BoxLayout(pnlGenerateTomogram,
        BoxLayout.Y_AXIS));
    pnlGenerateTomogram.setBorder(BorderFactory.createEtchedBorder());
    pnlGenerateTomogram.add(header.getContainer());
    pnlGenerateTomogram.add(pnlGenerateTomogramBody);
    //Generate tomogram body panel
    pnlGenerateTomogramBody.setLayout(new BoxLayout(pnlGenerateTomogramBody,
        BoxLayout.Y_AXIS));
    pnlGenerateTomogramBody.add(newstackOrBlendmont3dFindPanel.getComponent());
    pnlGenerateTomogramBody.add(tilt3dFindPanel.getComponent());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  public void updateParallelProcess() {
    parent.updateParallelProcess();
  }

  boolean isAdvanced() {
    return findBeads3dPanel.isAdvanced();
  }

  NewstackDisplay getNewstack3dFindDisplay() {
    if (manager.getMetaData().getViewType() != ViewType.MONTAGE) {
      return (NewstackDisplay) newstackOrBlendmont3dFindPanel;
    }
    return null;
  }

  BlendmontDisplay getBlendmont3dFindDisplay() {
    if (manager.getMetaData().getViewType() == ViewType.MONTAGE) {
      return (BlendmontDisplay) newstackOrBlendmont3dFindPanel;
    }
    return null;
  }

  TiltDisplay getTilt3dFindDisplay() {
    return tilt3dFindPanel;
  }

  FindBeads3dDisplay getFindBeads3dDisplay() {
    return findBeads3dPanel;
  }

  public boolean usingParallelProcessing() {
    return tilt3dFindPanel.usingParallelProcessing();
  }
  
  public void setEnabledTiltParameters(TomogramState state,
      ConstMetaData metaData) {
    tilt3dFindPanel.setEnabledTiltParameters(state, metaData);
  }

  void setParameters(ConstTiltParam param, boolean initialize)
      throws FileNotFoundException, IOException, LogFile.LockException {
    tilt3dFindPanel.setParameters(param, initialize);
  }

  void setParameters(ConstFindBeads3dParam param, boolean initialize) {
    findBeads3dPanel.setParameters(param, initialize);
  }
  void setParameters(ConstTiltalignParam param, boolean initialize) {
    tilt3dFindPanel.setParameters(param, initialize);
  }
  void setParameters(ReconScreenState screenState) {
    header.setState(screenState.getStackAlignAndTiltHeaderState());
    findBeads3dPanel.setParameters(screenState);
    reprojectModelPanel.setParameters(screenState);
  }

  void getParameters(ReconScreenState screenState) {
    header.getState(screenState.getStackAlignAndTiltHeaderState());
    findBeads3dPanel.getParameters(screenState);
  }

  void setParameters(BlendmontParam param) {
    if (manager.getMetaData().getViewType() == ViewType.MONTAGE) {
      ((Blendmont3dFindPanel) newstackOrBlendmont3dFindPanel)
          .setParameters(param);
    }
  }

  void setParameters(NewstParam param) {
    if (manager.getMetaData().getViewType() != ViewType.MONTAGE) {
      ((Newstack3dFindPanel) newstackOrBlendmont3dFindPanel)
          .setParameters(param);
    }
  }

  void setTilt3dFindParameters(ConstTiltParam param, boolean intialize)
      throws LogFile.LockException, FileNotFoundException, IOException {
    tilt3dFindPanel.setParameters(param, intialize);
  }

  void setVisible(boolean visible) {
    pnlRoot.setVisible(visible);
  }

  boolean validate() {
    return newstackOrBlendmont3dFindPanel.validate();
  }

  void getParameters(MetaData metaData) {
    newstackOrBlendmont3dFindPanel.getParameters(metaData);
  }

  void setParameters(ConstMetaData metaData) {
    newstackOrBlendmont3dFindPanel.setParameters(metaData);
    tilt3dFindPanel.setParameters(metaData);
  }

  public String getBeadSize() {
    return findBeads3dPanel.getBeadSize();
  }

  public void tilt3dFindAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    //The parent (this class) is responsible for running tilt_3dfind because it
    //may have to run newst/blend_3dfind first.
    //Validate to make sure that the binned bead size in pixels is not too
    //small.
    if (!validate()) {
      return;
    }
    //If the binning of the existing full aligned stack is different from the
    //the binning requested here, run newst/blend_3dfind.com and then run
    //tilt_3dfind.
    if (!manager.equalsBinning(axisID, newstackOrBlendmont3dFindPanel
        .getBinning(), FileType.ALIGNED_STACK)) {
      ProcessSeries processSeries = new ProcessSeries(manager, dialogType,
          tilt3dFindPanel);
      processSeries.setNextProcess(ProcessName.TILT_3D_FIND.toString());
      newstackOrBlendmont3dFindPanel.runProcess(processSeries,
          run3dmodMenuOptions);
    }
    else {
      manager.getState().setStackUsingNewstOrBlend3dFindOutput(axisID, false);
      //Just run tilt_3dfind.
      tilt3dFindPanel
          .tilt3dFindAction(deferred3dmodButton, run3dmodMenuOptions);
    }
  }
}
