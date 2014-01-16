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
 * <p> Revision 1.4  2011/07/19 20:01:14  sueh
 * <p> Bug# 1459 Wrapped checkboxes in a panel and used glue to left justify them.  Prevented spinners
 * <p> which have a value when they are first displayed from going all the way to the right.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:19:35  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:14:25  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.33  2010/03/08 21:12:29  sueh
 * <p> bug# 1311 Hooking the imod button to the run button.
 * <p>
 * <p> Revision 1.32  2009/09/20 21:33:38  sueh
 * <p> bug# 1268 Added a default value to LabeledSpinner.
 * <p>
 * <p> Revision 1.31  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.30  2009/06/12 19:50:45  sueh
 * <p> bug# 1221 Factored running prenewst, making it independent of the
 * <p> coarse align dialog.
 * <p>
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

package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.MetaData;
import etomo.type.ProcessName;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ViewType;

final class PrenewstPanel implements ContextMenu, Expandable, Run3dmodButtonContainer,
    NewstackDisplay, BlendmontDisplay {
  public static final String rcsid = "$Id$";

  private final EtomoPanel pnlPrenewst = new EtomoPanel();
  private final JPanel pnlBody = new JPanel();
  private final JPanel pnlCheckBoxes = new JPanel();
  private final CheckBox cbByteModeToOutput = new CheckBox("Convert to bytes");
  private final CheckBox cbMeanFloatDensities = new CheckBox("Float intensities to mean");
  private final Run3dmodButton btnImod = Run3dmodButton.get3dmodInstance(
      "View Aligned Stack In 3dmod", this);

  private final ApplicationManager applicationManager;
  private final LabeledSpinner spinBinning;
  private final AxisID axisID;
  private final Run3dmodButton btnCoarseAlign;
  private final ActionListener actionListener;
  private final PanelHeader header;
  private final CoarseAlignDialog parent;
  private final DialogType dialogType;
  private final CheckBox cbAntialiasFilter;
  private final EtomoNumber antialiasFilterValue;

  PrenewstPanel(final ApplicationManager applicationManager, final AxisID id,
      final DialogType dialogType, final CoarseAlignDialog parent,
      final GlobalExpandButton globalAdvancedButton) {
    this.parent = parent;
    axisID = id;
    this.applicationManager = applicationManager;
    this.dialogType = dialogType;
    btnCoarseAlign = (Run3dmodButton) applicationManager.getProcessResultDisplayFactory(
        axisID).getCoarseAlign();
    boolean montage = applicationManager.getMetaData().getViewType() == ViewType.MONTAGE;
    JPanel pnlAntialiasFilter;
    if (!montage) {
      pnlAntialiasFilter = new JPanel();
      cbAntialiasFilter = new CheckBox("Reduce size with antialiasing filter");
      antialiasFilterValue = new EtomoNumber();
    }
    else {
      pnlAntialiasFilter = null;
      cbAntialiasFilter = null;
      antialiasFilterValue = null;
    }

    btnCoarseAlign.setContainer(this);
    pnlPrenewst.setLayout(new BoxLayout(pnlPrenewst, BoxLayout.Y_AXIS));
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlCheckBoxes.setLayout(new BoxLayout(pnlCheckBoxes, BoxLayout.Y_AXIS));

    // Construct the binning spinner
    spinBinning = LabeledSpinner.getInstance("Coarse aligned image stack binning ", 1, 1,
        8, 1);
    spinBinning.setTextMaxmimumSize(UIParameters.INSTANCE.getSpinnerDimension());
    JPanel pnlBinning = new JPanel();
    pnlBinning.setLayout(new BoxLayout(pnlBinning, BoxLayout.X_AXIS));
    pnlBinning.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlBinning.add(spinBinning.getContainer());
    pnlBinning.add(Box.createHorizontalGlue());
    UIUtilities.addWithYSpace(pnlBody, pnlBinning);
    if (cbAntialiasFilter != null) {
      pnlAntialiasFilter.setLayout(new BoxLayout(pnlAntialiasFilter, BoxLayout.X_AXIS));
      pnlAntialiasFilter.add(cbAntialiasFilter);
      pnlAntialiasFilter.add(Box.createHorizontalGlue());
      UIUtilities.addWithYSpace(pnlBody, pnlAntialiasFilter);
    }
    if (montage) {
      header = PanelHeader.getAdvancedBasicInstance("Blendmont", this, dialogType,
          globalAdvancedButton);
    }
    else {
      header = PanelHeader.getAdvancedBasicInstance("Newstack", this, dialogType,
          globalAdvancedButton);
      JPanel pnlByteModeToOutput = new JPanel();
      pnlByteModeToOutput.setLayout(new BoxLayout(pnlByteModeToOutput, BoxLayout.X_AXIS));
      pnlByteModeToOutput.setAlignmentX(Box.CENTER_ALIGNMENT);
      pnlByteModeToOutput.add(cbByteModeToOutput);
      pnlByteModeToOutput.add(Box.createHorizontalGlue());
      UIUtilities.addWithYSpace(pnlCheckBoxes, pnlByteModeToOutput);
      UIUtilities.addWithYSpace(pnlCheckBoxes, cbMeanFloatDensities);
    }
    pnlBody.add(pnlCheckBoxes);
    btnCoarseAlign.setSize();
    btnCoarseAlign.setDeferred3dmodButton(btnImod);
    btnImod.setSize();
    JPanel pnlButtons = new JPanel();
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    pnlButtons.add(Box.createHorizontalGlue());
    pnlButtons.add(btnCoarseAlign.getComponent());
    pnlButtons.add(Box.createHorizontalGlue());
    pnlButtons.add(btnImod.getComponent());
    pnlButtons.add(Box.createHorizontalGlue());
    UIUtilities.addWithYSpace(pnlBody, pnlButtons);

    // Align the UI objects along their left sides
    UIUtilities.alignComponentsX(pnlBody, Component.CENTER_ALIGNMENT);
    UIUtilities.alignComponentsX(pnlCheckBoxes, Component.LEFT_ALIGNMENT);
    pnlPrenewst.setBorder(BorderFactory.createEtchedBorder());
    pnlPrenewst.add(header);
    pnlPrenewst.add(pnlBody);

    // Mouse adapter for context menu
    actionListener = new PrenewstPanelActionListener(this);
    btnCoarseAlign.addActionListener(actionListener);
    btnImod.addActionListener(actionListener);
    spinBinning.addChangeListener(new PrenewstBinningChangeListener(this));
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlPrenewst.addMouseListener(mouseAdapter);
    setToolTipText();
  }

  public void expand(final GlobalExpandButton button) {
  }

  public void expand(final ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      updateAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  void updateAdvanced(final boolean state) {
    spinBinning.setVisible(state);
    if (cbAntialiasFilter != null) {
      cbAntialiasFilter.setVisible(state);
    }
    cbByteModeToOutput.setVisible(state);
    cbMeanFloatDensities.setVisible(state);
  }

  private void updateEnabled() {
    if (cbAntialiasFilter != null) {
      Number value = spinBinning.getValue();
      cbAntialiasFilter.setEnabled(value != null && value.intValue() > 1);
    }
  }

  public void done() {
    btnCoarseAlign.removeActionListener(actionListener);
  }

  JPanel getPanel() {
    return pnlPrenewst;
  }

  void setAlignmentX(final float align) {
    pnlPrenewst.setAlignmentX(align);
  }

  void setParameters(final ConstMetaData metaData) {
    if (!metaData.isAntialiasFilterNull(dialogType, axisID)) {
      antialiasFilterValue.set(metaData.getAntialiasFilter(dialogType, axisID));
    }
  }

  public void setParameters(final ConstNewstParam prenewstParams) {
    int binning = prenewstParams.getBinByFactor();
    if (binning > 1) {
      spinBinning.setValue(binning);
    }
    boolean antialiasFilter = !prenewstParams.isAntialiasFilterNull();
    cbAntialiasFilter.setSelected(antialiasFilter);
    if (antialiasFilter) {
      antialiasFilterValue.set(prenewstParams.getAntialiasFilter());
    }
    cbByteModeToOutput
        .setSelected(prenewstParams.getModeToOutput() == NewstParam.DATA_MODE_BYTE);
    cbMeanFloatDensities
        .setSelected(prenewstParams.getFloatDensities() == NewstParam.FLOAT_DENSITIES_MEAN);
    updateEnabled();
  }

  public void setParameters(final BaseScreenState screenState) {
    // btnCoarseAlign.setButtonState(screenState.getButtonState(btnCoarseAlign
    // .getButtonStateKey()));
    header.setButtonStates(screenState);
  }

  public void getParameters(final BaseScreenState screenState) {
    header.getButtonStates(screenState);
  }

  public void getParameters(final MetaData metaData) {
    metaData.setAntialiasFilter(dialogType, axisID, antialiasFilterValue);
  }

  public void setParameters(final BlendmontParam blendmontParams) {
    ConstEtomoNumber binning = blendmontParams.getBinByFactor();
    if (!binning.isNull()) {
      spinBinning.setValue(binning);
    }
  }

  public boolean getParameters(final NewstParam prenewstParams, final boolean doValidation) {
    int binning = ((Integer) spinBinning.getValue()).intValue();

    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script
    if (binning > 1) {
      prenewstParams.setBinByFactor(binning);
    }
    else {
      prenewstParams.setBinByFactor(Integer.MIN_VALUE);
    }
    // Save when field is disabled
    boolean antialiasFilter = cbAntialiasFilter.isSelected();
    prenewstParams.setAntialiasFilter(antialiasFilter);
    if (antialiasFilter) {
      prenewstParams.setAntialiasFilterValue(antialiasFilterValue);
    }
    if (cbByteModeToOutput.isSelected()) {
      prenewstParams.setModeToOutput(NewstParam.DATA_MODE_BYTE);
    }
    else {
      prenewstParams.setModeToOutput(NewstParam.DATA_MODE_DEFAULT);
    }
    if (cbMeanFloatDensities.isSelected()) {
      prenewstParams.setFloatDensities(NewstParam.FLOAT_DENSITIES_MEAN);
    }
    else {
      prenewstParams.setFloatDensities(NewstParam.FLOAT_DENSITIES_DEFAULT);
    }
    return true;
  }

  public ProcessName getProcessName() {
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      return ProcessName.PREBLEND;
    }
    return ProcessName.PRENEWST;
  }

  public boolean getParameters(final BlendmontParam blendmontParam,
      final boolean doValidation) {
    blendmontParam.setBinByFactor(((Integer) spinBinning.getValue()).intValue());
    return true;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String[] manPagelabel = { "Newstack" };
    String[] manPage = { "newstack.html" };
    String[] logFileLabel = { "Prenewst" };
    String[] logFile = new String[1];
    logFile[0] = "prenewst" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(pnlPrenewst, mouseEvent,
        "COARSE ALIGNMENT", ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel,
        logFile, applicationManager, axisID);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    spinBinning
        .setToolTipText("Binning for the image stack used to generate and fix the fiducial model.");
    cbByteModeToOutput
        .setToolTipText("Set the storage mode of the output file to bytes.  When unchecked the storage mode is the same as that of the first input file.  This option should be turned off when the dynamic range is still too poor after X ray removal.  Command:  "
            + NewstParam.DATA_MODE_OPTION + " " + NewstParam.DATA_MODE_BYTE);
    cbMeanFloatDensities
        .setToolTipText("Adjust densities of sections individually.  Scale sections to common mean and standard deviation.  Command:  "
            + NewstParam.FLOAT_DENSITIES_OPTION + " " + NewstParam.FLOAT_DENSITIES_MEAN);
    btnCoarseAlign
        .setToolTipText("Use transformations to produce stack of aligned images.");
    btnImod.setToolTipText("Use 3dmod to view the coarsely aligned images.");
    if (cbAntialiasFilter != null) {
      cbAntialiasFilter
          .setToolTipText("Use antialiased image reduction instead binning with the "
              + "default filter in Newstack; useful for data from direct detection "
              + "cameras.");
    }
  }

  public void action(final Run3dmodButton button, final Run3dmodMenuOptions menuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(), menuOptions);
  }

  public boolean validate() {
    return true;
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
  void buttonAction(final String command, final Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnCoarseAlign.getActionCommand())) {
      applicationManager.coarseAlign(axisID, btnCoarseAlign, null, deferred3dmodButton,
          menuOptions, dialogType, this, this);
    }
    else if (command.equals(btnImod.getActionCommand())) {
      applicationManager.imodCoarseAlign(axisID, menuOptions, null, false);
    }
  }

  private static final class PrenewstPanelActionListener implements ActionListener {
    private final PrenewstPanel adaptee;

    private PrenewstPanelActionListener(final PrenewstPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null, null);
    }
  }

  private static final class PrenewstBinningChangeListener implements ChangeListener {
    private final PrenewstPanel panel;

    private PrenewstBinningChangeListener(final PrenewstPanel panel) {
      this.panel = panel;
    }

    public void stateChanged(final ChangeEvent event) {
      panel.updateEnabled();
    }
  }
}