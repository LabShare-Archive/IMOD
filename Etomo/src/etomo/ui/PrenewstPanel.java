/**
 * <p>Description: Panel to modify the newstack parameters in prenewst</p>
 * 
 * <p>Copyright: Copyright (c) 2002-2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.29  2009/01/20 20:19:11  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 1.28  2008/05/28 02:50:37  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 1.27  2008/05/13 23:02:42  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 1.26  2008/05/07 00:01:44  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 1.25  2008/05/03 00:52:17  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.24  2007/09/10 20:43:45  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 1.23  2007/05/01 22:29:54  sueh
 * <p> bug# 964 In LabeledSpinner, saving SpinnerNumberModel so that the
 * <p> maximum can be changed.
 * <p>
 * <p> Revision 1.22  2007/02/09 00:51:33  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.21  2006/07/20 17:20:46  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 1.20  2006/07/04 18:47:52  sueh
 * <p> bug# 893 Added updateAdvanced(boolean) to change the header when the
 * <p> advanced button is pressed.
 * <p>
 * <p> Revision 1.19  2006/06/21 15:54:30  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 1.18  2006/03/30 16:48:27  sueh
 * <p> bug# 437 Added header and btnCoarseAlign
 * <p>
 * <p> Revision 1.17  2006/03/28 00:56:13  sueh
 * <p> bug# 803 Fixed the tooltips.
 * <p>
 * <p> Revision 1.16  2006/03/23 21:10:43  sueh
 * <p> bug# 803 byteModeToOutput and meanFloatDensities are not available in
 * <p> blendmont.
 * <p>
 * <p> Revision 1.15  2006/03/22 21:28:59  sueh
 * <p> bug# 803 Improved tooltips.
 * <p>
 * <p> Revision 1.14  2006/03/22 18:37:22  sueh
 * <p> bug# 803 Added cbMeanFloatDensities.
 * <p>
 * <p> Revision 1.13  2006/03/22 18:00:52  sueh
 * <p> bug# 803 Added cbByteModeToOutput.
 * <p>
 * <p> Revision 1.12  2005/07/14 22:09:03  sueh
 * <p> bug# 626 Enabling binning for montage view.  Setting binning in
 * <p> set and getParameters(BlendmontParam).
 * <p>
 * <p> Revision 1.11  2005/03/11 01:37:43  sueh
 * <p> bug# 533 Change title of panel to Blendmont Parameters when montage is
 * <p> set.
 * <p>
 * <p> Revision 1.10  2005/03/02 00:12:45  sueh
 * <p> bug# 533 disabled binning for montaging.
 * <p>
 * <p> Revision 1.9  2004/12/02 20:41:58  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 1.8  2004/11/20 00:01:23  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.7.4.1  2004/10/11 02:16:51  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 1.7  2004/06/17 18:49:05  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 1.6  2004/05/25 23:25:28  rickg
 * <p> Bug #391 moved fiducialess parameters to the parent dialog
 * <p>
 * <p> Revision 1.5  2004/04/07 21:03:10  rickg
 * <p> Fixed layout using UIUtilities
 * <p>
 * <p> Revision 1.4  2004/04/06 17:00:21  rickg
 * <p> Implemented basic fiducialess alignment interface
 * <p>
 * <p> Revision 1.3  2004/03/13 00:33:13  rickg
 * <p> Bug# 390 Add set/get Parameters and context menu
 * <p>
 * <p> Revision 1.2  2004/03/12 00:11:02  rickg
 * <p> Disables binning until xfproduct command is implemented
 * <p>
 * <p> Revision 1.1  2004/02/05 04:36:44  rickg
 * <p> Bug# 390 Initial revision
 * <p> </p>
 */

package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ViewType;

final class PrenewstPanel implements ContextMenu, Expandable,
    Run3dmodButtonContainer, NewstackDisplay, BlendmontDisplay {
  public static final String rcsid = "$Id$";

  private final EtomoPanel pnlPrenewst = new EtomoPanel();
  private final JPanel pnlBody = new JPanel();
  private final JPanel pnlCheckBoxes = new JPanel();
  private final ApplicationManager applicationManager;

  private final LabeledSpinner spinBinning;
  private final CheckBox cbByteModeToOutput = new CheckBox("Convert to bytes");
  private final CheckBox cbMeanFloatDensities = new CheckBox(
      "Float intensities to mean");

  private final AxisID axisID;
  private final Run3dmodButton btnCoarseAlign;
  private final ActionListener actionListener;
  private final PanelHeader header;
  private final CoarseAlignDialog parent;
  private final DialogType dialogType;

  PrenewstPanel(ApplicationManager applicationManager, AxisID id,
      DialogType dialogType, CoarseAlignDialog parent) {
    this.parent = parent;
    axisID = id;
    this.applicationManager = applicationManager;
    this.dialogType = dialogType;
    btnCoarseAlign = (Run3dmodButton) applicationManager
        .getProcessResultDisplayFactory(axisID).getCoarseAlign();
    btnCoarseAlign.setContainer(this);

    pnlPrenewst.setLayout(new BoxLayout(pnlPrenewst, BoxLayout.Y_AXIS));
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlCheckBoxes.setLayout(new BoxLayout(pnlCheckBoxes, BoxLayout.Y_AXIS));

    //  Construct the binning spinner
    SpinnerNumberModel integerModel = new SpinnerNumberModel(1, 1, 8, 1);
    spinBinning = new LabeledSpinner("Coarse aligned image stack binning ",
        integerModel);
    spinBinning
        .setTextMaxmimumSize(UIParameters.INSTANCE.getSpinnerDimension());
    //if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
    //  spinBinning.setEnabled(false);
    //}
    UIUtilities.addWithYSpace(pnlBody, spinBinning.getContainer());
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      header = PanelHeader.getAdvancedBasicInstance("Blendmont", this,
          dialogType);
    }
    else {
      header = PanelHeader.getAdvancedBasicInstance("Newstack", this,
          dialogType);
      UIUtilities.addWithYSpace(pnlCheckBoxes, cbByteModeToOutput);
      UIUtilities.addWithYSpace(pnlCheckBoxes, cbMeanFloatDensities);
    }
    pnlBody.add(pnlCheckBoxes);
    btnCoarseAlign.setSize();
    UIUtilities.addWithYSpace(pnlBody, btnCoarseAlign.getComponent());

    //  Align the UI objects along their left sides
    UIUtilities.alignComponentsX(pnlBody, Component.CENTER_ALIGNMENT);
    UIUtilities.alignComponentsX(pnlCheckBoxes, Component.LEFT_ALIGNMENT);
    pnlPrenewst.setBorder(BorderFactory.createEtchedBorder());
    pnlPrenewst.add(header);
    pnlPrenewst.add(pnlBody);

    //  Mouse adapter for context menu
    actionListener = new PrenewstPanelActionListener(this);
    btnCoarseAlign.addActionListener(actionListener);
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlPrenewst.addMouseListener(mouseAdapter);
    setToolTipText();
  }

  /**
   * Set deferred 3dmod buttons that are members of another class.  Caller must
   * make sure that buttons exist.
   */
  void setDeferred3dmodButtons() {
    btnCoarseAlign.setDeferred3dmodButton(parent.btnImod);
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      setAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Update the header with the current advanced state
   */
  void updateAdvanced(boolean isAdvanced) {
    header.setAdvanced(isAdvanced);
  }

  void setAdvanced(boolean state) {
    spinBinning.setVisible(state);
    cbByteModeToOutput.setVisible(state);
    cbMeanFloatDensities.setVisible(state);
  }

  public static ProcessResultDisplay getCoarseAlignDisplay(DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        "Generate Coarse Aligned Stack", dialogType);
  }

  public void done() {
    btnCoarseAlign.removeActionListener(actionListener);
  }

  JPanel getPanel() {
    return pnlPrenewst;
  }

  void setAlignmentX(float align) {
    pnlPrenewst.setAlignmentX(align);
  }

  public void setParameters(ConstNewstParam prenewstParams) {
    int binning = prenewstParams.getBinByFactor();
    if (binning > 1) {
      spinBinning.setValue(binning);
    }
    cbByteModeToOutput
        .setSelected(prenewstParams.getModeToOutput() == ConstNewstParam.DATA_MODE_BYTE);
    cbMeanFloatDensities
        .setSelected(prenewstParams.getFloatDensities() == ConstNewstParam.FLOAT_DENSITIES_MEAN);
  }

  public void setParameters(BaseScreenState screenState) {
    //btnCoarseAlign.setButtonState(screenState.getButtonState(btnCoarseAlign
    //   .getButtonStateKey()));
    header.setButtonStates(screenState);
  }

  public void getParameters(BaseScreenState screenState) {
    header.getButtonStates(screenState);
  }

  public void setParameters(BlendmontParam blendmontParams) {
    ConstEtomoNumber binning = blendmontParams.getBinByFactor();
    if (!binning.isNull()) {
      spinBinning.setValue(binning);
    }
  }

  public void getParameters(NewstParam prenewstParams) {
    int binning = ((Integer) spinBinning.getValue()).intValue();

    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    if (binning > 1) {
      prenewstParams.setBinByFactor(binning);
    }
    else {
      prenewstParams.setBinByFactor(Integer.MIN_VALUE);
    }
    if (cbByteModeToOutput.isSelected()) {
      prenewstParams.setModeToOutput(ConstNewstParam.DATA_MODE_BYTE);
    }
    else {
      prenewstParams.setModeToOutput(ConstNewstParam.DATA_MODE_DEFAULT);
    }
    if (cbMeanFloatDensities.isSelected()) {
      prenewstParams.setFloatDensities(ConstNewstParam.FLOAT_DENSITIES_MEAN);
    }
    else {
      prenewstParams.setFloatDensities(ConstNewstParam.FLOAT_DENSITIES_DEFAULT);
    }
  }

  public void getParameters(BlendmontParam blendmontParam) {
    blendmontParam
        .setBinByFactor(((Integer) spinBinning.getValue()).intValue());
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Newstack" };
    String[] manPage = { "newstack.html" };
    String[] logFileLabel = { "Prenewst" };
    String[] logFile = new String[1];
    logFile[0] = "prenewst" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlPrenewst, mouseEvent,
        "COARSE ALIGNMENT", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    spinBinning
        .setToolTipText("Binning for the image stack used to generate and fix the fiducial model.");
    cbByteModeToOutput
        .setToolTipText("Set the storage mode of the output file to bytes.  When unchecked the storage mode is the same as that of the first input file.  This option should be turned off when the dynamic range is still too poor after X ray removal.  Command:  "
            + ConstNewstParam.DATA_MODE_OPTION
            + " "
            + ConstNewstParam.DATA_MODE_BYTE);
    cbMeanFloatDensities
        .setToolTipText("Adjust densities of sections individually.  Scale sections to common mean and standard deviation.  Command:  "
            + ConstNewstParam.FLOAT_DENSITIES_OPTION
            + " "
            + ConstNewstParam.FLOAT_DENSITIES_MEAN);
    btnCoarseAlign
        .setToolTipText("Use transformations to produce stack of aligned images.");
  }

  public void action(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        menuOptions);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  void buttonAction(String command, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnCoarseAlign.getActionCommand())) {
      applicationManager.coarseAlign(axisID, btnCoarseAlign, null,
          deferred3dmodButton, menuOptions, dialogType, this, this);
    }
  }

  class PrenewstPanelActionListener implements ActionListener {
    PrenewstPanel adaptee;

    PrenewstPanelActionListener(PrenewstPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null, null);
    }
  }
}