package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.*;

import javax.swing.BoxLayout;

import etomo.ApplicationManager;
import etomo.comscript.ConstSqueezevolParam;
import etomo.comscript.SqueezevolParam;
import etomo.comscript.TrimvolParam;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 */
public final class PostProcessingDialog extends ProcessDialog implements
    ContextMenu, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private TrimvolPanel trimvolPanel;

  private LabeledTextField ltfReductionFactorXY;
  private LabeledTextField ltfReductionFactorZ;
  private CheckBox cbLinearInterpolation;

  private final Run3dmodButton btnSqueezeVolume;
  private final Run3dmodButton btnImodSqueezedVolume = Run3dmodButton
      .get3dmodInstance("Open Squeezed Volume in 3dmod", this);

  private PostProcessingDialogActionListener actionListener = new PostProcessingDialogActionListener(
      this);

  public PostProcessingDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.ONLY, DialogType.POST_PROCESSING);
    fixRootPanel(rootSize);
    btnSqueezeVolume = (Run3dmodButton) appMgr.getProcessResultDisplayFactory(
        axisID).getSqueezeVolume();
    btnSqueezeVolume.setContainer(this);
    btnSqueezeVolume.setDeferred3dmodButton(btnImodSqueezedVolume);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("Post Processing").getBorder());
    trimvolPanel = new TrimvolPanel(applicationManager, axisID, dialogType);
    rootPanel.add(trimvolPanel.getContainer());
    rootPanel.add(createSqueezeVolPanel());
    addExitButtons();

    btnAdvanced.setVisible(false);
    btnExecute.setText("Done");

    // Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }

  private Container createSqueezeVolPanel() {
    SpacedPanel squeezeVolPanel = SpacedPanel.getInstance();
    squeezeVolPanel.setBoxLayout(BoxLayout.Y_AXIS);
    squeezeVolPanel.setBorder(new BeveledBorder("Squeeze Volume").getBorder());
    //first component
    SpacedPanel squeezeVolPanel1 = SpacedPanel.getInstance();
    squeezeVolPanel1.setBoxLayout(BoxLayout.X_AXIS);
    ltfReductionFactorXY = new LabeledTextField("Reduction factor in X and Y ");
    squeezeVolPanel1.add(ltfReductionFactorXY);
    ltfReductionFactorZ = new LabeledTextField("in Z ");
    squeezeVolPanel1.add(ltfReductionFactorZ);
    squeezeVolPanel.add(squeezeVolPanel1);
    //second component
    cbLinearInterpolation = new CheckBox("Linear interpolation");
    cbLinearInterpolation.setAlignmentX(Component.RIGHT_ALIGNMENT);
    squeezeVolPanel.add(cbLinearInterpolation);
    //third component
    SpacedPanel squeezeVolPanel2 = SpacedPanel.getInstance();
    squeezeVolPanel2.setBoxLayout(BoxLayout.X_AXIS);
    btnSqueezeVolume.addActionListener(actionListener);
    btnSqueezeVolume.setSize();
    squeezeVolPanel2.add(btnSqueezeVolume);
    squeezeVolPanel2.addHorizontalGlue();
    btnImodSqueezedVolume.addActionListener(actionListener);
    btnImodSqueezedVolume.setSize();
    squeezeVolPanel2.add(btnImodSqueezedVolume);
    squeezeVolPanel.add(squeezeVolPanel2);
    return squeezeVolPanel.getContainer();
  }

  public static ProcessResultDisplay getSqueezeVolumeDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Squeeze Volume",
        DialogType.POST_PROCESSING);
  }

  public static ProcessResultDisplay getTrimVolumeDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Trim Volume",
        DialogType.POST_PROCESSING);
  }

  /**
   * Set the panel values with the specified parameters
   * @param squeezevolParam
   */
  public void setParameters(ConstSqueezevolParam squeezevolParam) {
    ltfReductionFactorXY.setText(squeezevolParam.getReductionFactorX()
        .toString());
    if (isSqueezevolFlipped()) {
      ltfReductionFactorZ.setText(squeezevolParam.getReductionFactorZ()
          .toString());
    }
    else {
      ltfReductionFactorZ.setText(squeezevolParam.getReductionFactorY()
          .toString());
    }
    cbLinearInterpolation.setSelected(squeezevolParam.isLinearInterpolation());
  }

  public final void setParameters(ReconScreenState screenState) {
    trimvolPanel.setParameters(screenState);
    btnSqueezeVolume.setButtonState(screenState.getButtonState(btnSqueezeVolume
        .getButtonStateKey()));
  }

  /**
   * Get the panel values
   * @param squeezevolParam
   */
  public void getParameters(SqueezevolParam squeezevolParam) {
    squeezevolParam.setReductionFactorX(ltfReductionFactorXY.getText());
    TomogramState state = applicationManager.getState();
    boolean flipped = squeezevolParam.setFlipped(isTrimvolFlipped());
    if (flipped) {
      squeezevolParam.setReductionFactorY(ltfReductionFactorXY.getText());
      squeezevolParam.setReductionFactorZ(ltfReductionFactorZ.getText());
    }
    else {
      squeezevolParam.setReductionFactorY(ltfReductionFactorZ.getText());
      squeezevolParam.setReductionFactorZ(ltfReductionFactorXY.getText());
    }
    squeezevolParam.setLinearInterpolation(cbLinearInterpolation.isSelected());
  }

  /**
   * return true if the result of squeezevol is flipped.
   * If squeezevol hasn't been done, return true if the result of trimvol is
   * flipped.
   * @return
   */
  public boolean isSqueezevolFlipped() {
    TomogramState state = applicationManager.getState();
    if (!state.getSqueezevolFlipped().isNull()) {
      return state.getSqueezevolFlipped().is();
    }
    return isTrimvolFlipped();
  }

  /**
   * return true if the result of squeezevol is flipped.
   * If squeezevol hasn't been done, return true if the result of trimvol is
   * flipped.
   * @return
   */
  public boolean isTrimvolFlipped() {
    TomogramState state = applicationManager.getState();
    if (state.getTrimvolFlipped().isNull()) {
      return state.getBackwardCompatibleTrimvolFlipped();
    }
    return state.getTrimvolFlipped().is();
  }

  /**
   * Set the trimvol panel values with the specified parameters
   * @param trimvolParam
   */
  public void setTrimvolParams(TrimvolParam trimvolParam) {
    trimvolPanel.setParameters(trimvolParam);
  }

  /**
   * Get the trimvol parameter values from the panel 
   * @param trimvolParam
   */
  public boolean getTrimvolParams(TrimvolParam trimvolParam) {
    return trimvolPanel.getParameters(trimvolParam);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Trimvol", "Squeezevol" };
    String[] manPage = { "trimvol.html", "squeezevol.html" };

    //    ContextPopup contextPopup =
    new ContextPopup(rootPanel, mouseEvent, "POST-PROCESSING",
        ContextPopup.TOMO_GUIDE, manPagelabel, manPage, applicationManager,
        axisID);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnSqueezeVolume.getActionCommand())) {
      applicationManager.squeezevol(btnSqueezeVolume, null,
          deferred3dmodButton, run3dmodMenuOptions, dialogType);
    }
    else if (command.equals(btnImodSqueezedVolume.getActionCommand())) {
      applicationManager.imodSqueezedVolume(run3dmodMenuOptions);
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  boolean done() {
    if (applicationManager.donePostProcessing()) {
      btnSqueezeVolume.removeActionListener(actionListener);
      trimvolPanel.done();
      setDisplayed(false);
      return true;
    }
    return false;
  }

  private final class PostProcessingDialogActionListener implements
      ActionListener {
    private final PostProcessingDialog adaptee;

    private PostProcessingDialogActionListener(
        final PostProcessingDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  private void setToolTipText() {
    ltfReductionFactorXY.setToolTipText("Factor to squeeze by in X and Y.");
    ltfReductionFactorZ.setToolTipText("Factor to squeeze by in Z.");
    cbLinearInterpolation
        .setToolTipText("Use linear instead of quadratic interpolation for transforming the "
            + "volume with Matchvol.");
    btnSqueezeVolume
        .setToolTipText("Squeeze the trimmed volume by the given factors.");
    btnImodSqueezedVolume.setToolTipText("View the squeezed volume.");
  }

}
/**
 * <p> $Log$
 * <p> Revision 3.37  2008/05/28 02:50:30  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 3.36  2008/05/13 23:02:33  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.35  2008/05/03 00:52:10  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.34  2007/12/26 22:25:45  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 3.33  2007/11/06 20:30:25  sueh
 * <p> bug# 1047 Generalize TripvolPanel.
 * <p>
 * <p> Revision 3.32  2007/05/26 00:33:06  sueh
 * <p> bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * <p> Setting button size in UI.
 * <p>
 * <p> Revision 3.31  2007/02/09 00:51:17  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.30  2006/08/14 18:34:02  sueh
 * <p> bug# 890 Returning a success/failure boolean from getTrimvolParams.  Passing
 * <p> axisID to TrimvoPanel constructor.
 * <p>
 * <p> Revision 3.29  2006/07/04 20:41:51  sueh
 * <p> bug# 898 Don't remove action listeners unless the done dialog function
 * <p> succeeds.
 * <p>
 * <p> Revision 3.28  2006/06/30 20:02:18  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.
 * <p>
 * <p> Revision 3.27  2006/06/21 15:54:19  sueh
 * <p> bug# 581 Passing manager and axis to ContextPopup, so that imodqtassist can
 * <p> be run.
 * <p>
 * <p> Revision 3.26  2006/01/31 21:00:03  sueh
 * <p> bug# 521 Managing trimvol and squeezevol buttons in
 * <p> ProcessResultDisplayFactory.  Made trimvol a toggle button.
 * <p>
 * <p> Revision 3.25  2006/01/26 22:05:54  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.24  2006/01/03 23:42:56  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox
 * <p>
 * <p> Revision 3.23  2005/11/14 22:14:53  sueh
 * <p> bug# 762 Made action() protected.
 * <p>
 * <p> Revision 3.22  2005/08/11 23:57:00  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 3.21  2005/08/10 20:45:24  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.20  2005/08/09 20:27:18  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.
 * <p>
 * <p> Revision 3.19  2005/08/04 20:14:57  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.18  2005/07/06 23:46:25  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 3.17  2005/04/21 20:45:54  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.16  2005/04/16 02:00:52  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.15  2005/03/30 21:06:40  sueh
 * <p> bug# 621 Putting a titled border around the root panel.
 * <p>
 * <p> Revision 3.14  2005/03/24 17:53:59  sueh
 * <p> bug# 621 Moved the clean up panel in to a separate dialog.
 * <p>
 * <p> Revision 3.13  2005/03/21 19:22:35  sueh
 * <p> bug# 620 Added beveled border and tooltips for Squeeze volume
 * <p>
 * <p> Revision 3.12  2005/01/21 23:46:00  sueh
 * <p> bug# 509 bug# 591  Using EtomoNumber.isNull() instead of isSet().
 * <p>
 * <p> Revision 3.11  2005/01/14 03:07:40  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.10  2005/01/12 00:45:42  sueh
 * <p> bug# 579 Renamed TomogramState.getBackwordCompatible...() functions
 * <p> to ...BackwardCompatible...
 * <p>
 * <p> Revision 3.9  2005/01/10 23:56:32  sueh
 * <p> bug# 578 Modified isSqueezevolFlipped() and isTrimvolFlipped().
 * <p>
 * <p> Revision 3.8  2005/01/08 01:55:32  sueh
 * <p> bug# 578 Calling all backword compatible functions in TomogramState
 * <p> "getBackwordCompatible...".
 * <p>
 * <p> Revision 3.7  2004/12/16 02:33:05  sueh
 * <p> bug# 564 Taking whether trimvol output and squeezevol output are flipped
 * <p> or not when getting and setting Squeezevol parameters.
 * <p>
 * <p> Revision 3.6  2004/12/14 21:50:57  sueh
 * <p> bug# 557 Made separate variables for x and y reduction factors to handle
 * <p> an unflipped tomogram.
 * <p>
 * <p> Revision 3.5  2004/12/04 01:27:19  sueh
 * <p> bug# 557 Added call to imodSqueezedVolume().
 * <p>
 * <p> Revision 3.4  2004/12/02 20:41:48  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.3  2004/12/02 18:30:50  sueh
 * <p> bug# 557 Added the Squeeze Volume panel.  Added an action for the
 * <p> Squeeze Volume button.
 * <p>
 * <p> Revision 3.2  2004/12/01 03:47:37  sueh
 * <p> bug# 557 Added ui fields to use with squeezevol.
 * <p>
 * <p> Revision 3.1  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/10/30 21:05:06  rickg
 * <p> Bug# 340 Added context menu
 * <p>
 * <p> Revision 2.4  2003/04/17 23:07:20  rickg
 * <p> Added cleanup panel
 * <p>
 * <p> Revision 2.3  2003/04/16 00:15:01  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 2.2  2003/04/14 23:57:34  rickg
 * <p> Trimvol management changes
 * <p>
 * <p> Revision 2.1  2003/04/10 23:43:23  rickg
 * <p> Added trimvol panel
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.4  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.3  2002/10/17 22:39:55  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
