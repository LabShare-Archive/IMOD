package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.MetaData;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ViewType;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.InvalidParameterException;

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
 * <p> Revision 1.5  2011/07/19 20:01:14  sueh
 * <p> Bug# 1459 Wrapped checkboxes in a panel and used glue to left justify them.  Prevented spinners
 * <p> which have a value when they are first displayed from going all the way to the right.
 * <p>
 * <p> Revision 1.4  2011/03/02 00:00:12  sueh
 * <p> bug# 1452 Removing image rotation conversion between float and
 * <p> double.  Using string where possible.
 * <p>
 * <p> Revision 1.3  2011/02/24 23:37:23  sueh
 * <p> bug# 1452 imageRotation needs to be double everywhere.
 * <p>
 * <p> Revision 1.2  2011/02/22 18:16:19  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.5  2010/04/28 16:43:34  sueh
 * <p> bug# 1344 In NewstParam removed the manager parameter from
 * <p> setSizeToOutputInXandY because manager is stored in the class.
 * <p>
 * <p> Revision 3.4  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.3  2009/09/20 21:32:51  sueh
 * <p> bug# 1268 Added a default value to LabeledSpinner.
 * <p>
 * <p> Revision 3.2  2009/09/17 19:12:58  sueh
 * <p> bug# 1257 In NewstParam.setSizeToOutputInXandY forgot to read the
 * <p> header.  Adding read call and throwing InvalidParameterException and
 * <p> IOException.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.4  2009/06/16 22:54:49  sueh
 * <p> bug# 1221 Runs newst and blendmont.
 * <p>
 * <p> Revision 1.3  2009/06/15 20:26:46  sueh
 * <p> bug# 1221 Reformatted.
 * <p>
 * <p> Revision 1.2  2009/06/12 19:50:09  sueh
 * <p> bug# 1221 Factored running newst, making it independent of the
 * <p> final aligned dialog and expert.
 * <p>
 * <p> Revision 1.1  2009/06/10 22:17:14  sueh
 * <p> bug# 1221 Factoring Newstack and blendmont into NewstackPanel.
 * <p>
 */
final class NewstackAndBlendmontParamPanel implements FiducialessParams {
  public static final String rcsid = "$Id$";

  private static final String SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL = "Size to output";
  static final String BINNING_LABEL = "Aligned image stack binning";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance(true);
  private final LabeledSpinner spinBinning = LabeledSpinner.getInstance(BINNING_LABEL
      + ": ", 1, 1, 8, 1);
  private final LabeledTextField ltfSizeToOutputInXandY = new LabeledTextField(
      FieldType.INTEGER_PAIR, SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + " (X,Y - unbinned): ");
  private final LabeledTextField ltfRotation = new LabeledTextField(
      FieldType.FLOATING_POINT, "Tilt axis rotation: ");
  private final CheckBox cbFiducialess = new CheckBox("Fiducialless alignment");
  private final CheckBox cbUseLinearInterpolation = new CheckBox(
      "Use linear interpolation");

  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;
  private final CheckBox cbAntialiasFilter;
  private final EtomoNumber antialiasFilterValue;

  private NewstackAndBlendmontParamPanel(final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    if (manager.getMetaData().getViewType() != ViewType.MONTAGE) {
      cbAntialiasFilter = new CheckBox("Reduce size with antialiasing filter");
      antialiasFilterValue = new EtomoNumber();
    }
    else {
      cbAntialiasFilter = null;
      antialiasFilterValue = null;
    }
  }

  static NewstackAndBlendmontParamPanel getInstance(final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType) {
    NewstackAndBlendmontParamPanel instance = new NewstackAndBlendmontParamPanel(manager,
        axisID, dialogType);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  private void addListeners() {
    cbFiducialess
        .addActionListener(new NewstackAndBlendmontParamPanelActionListener(this));
    spinBinning.addChangeListener(new NewstackAndBlendmontBinningChangeListener(this));
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  private void createPanel() {
    // Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setComponentAlignmentX(Box.LEFT_ALIGNMENT);
    pnlRoot.add(cbUseLinearInterpolation);
    JPanel pnlBinning = new JPanel();
    pnlBinning.setLayout(new BoxLayout(pnlBinning, BoxLayout.X_AXIS));
    pnlBinning.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlBinning.add(spinBinning.getContainer());
    pnlBinning.add(Box.createHorizontalGlue());
    pnlRoot.add(pnlBinning);
    if (cbAntialiasFilter != null) {
      pnlRoot.add(cbAntialiasFilter);
    }
    pnlRoot.add(cbFiducialess);
    pnlRoot.add(ltfRotation);
    pnlRoot.add(ltfSizeToOutputInXandY);
    // update
    updateFiducialess();
  }

  void setVisible(final boolean visible) {
    pnlRoot.setVisible(visible);
  }

  public void setParameters(final BlendmontParam blendmontParam) {
    cbUseLinearInterpolation.setSelected(blendmontParam.isLinearInterpolation());
  }

  public void setParameters(final ConstNewstParam newstParam) {
    cbUseLinearInterpolation.setSelected(newstParam.isLinearInterpolation());
    if (cbAntialiasFilter != null) {
      boolean antialiasFilter = !newstParam.isAntialiasFilterNull();
      cbAntialiasFilter.setSelected(antialiasFilter);
      if (antialiasFilter) {
        antialiasFilterValue.set(newstParam.getAntialiasFilter());
      }
    }
  }

  /**
   * The Metadata values that are from the setup dialog should not be overrided
   * by this dialog unless the Metadata values are empty.
   * Must save data from the two instances under separate keys.
   * @param metaData
   * @throws FortranInputSyntaxException
   */
  void getParameters(final MetaData metaData) throws FortranInputSyntaxException {
    metaData.setSizeToOutputInXandY(axisID, ltfSizeToOutputInXandY.getText());
    metaData.setStackBinning(axisID, getBinning());
    metaData.setAntialiasFilter(dialogType, axisID, antialiasFilterValue);
  }

  public boolean getParameters(final BlendmontParam blendmontParam,
      final boolean doValidation) throws FortranInputSyntaxException,
      InvalidParameterException, IOException {
    try {
      blendmontParam.setBinByFactor(getBinning());
      blendmontParam.setLinearInterpolation(cbUseLinearInterpolation.isSelected());
      try {
        if (!blendmontParam.convertToStartingAndEndingXandY(
            ltfSizeToOutputInXandY.getText(doValidation), manager.getMetaData()
                .getImageRotation(axisID).getDouble(), ltfSizeToOutputInXandY.getLabel())) {
          return false;
        }
      }
      catch (FortranInputSyntaxException e) {
        e.printStackTrace();
        throw new FortranInputSyntaxException(
            NewstackAndBlendmontParamPanel.SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + ":  "
                + e.getMessage());
      }
      blendmontParam.setFiducialess(cbFiducialess.isSelected());
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  // Copy the newstack parameters from the GUI to the NewstParam object
  public boolean getParameters(final NewstParam newstParam, final boolean doValidation)
      throws FortranInputSyntaxException, InvalidParameterException, IOException {
    try {
      int binning = getBinning();
      // Only explicitly write out the binning if its value is something other than
      // the default of 1 to keep from cluttering up the com script
      if (binning > 1) {
        newstParam.setBinByFactor(binning);
      }
      else {
        newstParam.setBinByFactor(Integer.MIN_VALUE);
      }
      // Save when field is disabled
      boolean antialiasFilter = cbAntialiasFilter.isSelected();
      newstParam.setAntialiasFilter(antialiasFilter);
      if (antialiasFilter) {
        newstParam.setAntialiasFilterValue(antialiasFilterValue);
      }
      newstParam.setLinearInterpolation(cbUseLinearInterpolation.isSelected());
      return newstParam.setSizeToOutputInXandY(
          ltfSizeToOutputInXandY.getText(doValidation), getBinning(), manager
              .getMetaData().getImageRotation(axisID).getDouble(),
          ltfSizeToOutputInXandY.getLabel());
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void setParameters(final ConstMetaData metaData) {
    spinBinning.setValue(metaData.getStackBinning(axisID));
    if (!metaData.isAntialiasFilterNull(dialogType, axisID)) {
      antialiasFilterValue.set(metaData.getAntialiasFilter(dialogType, axisID));
    }
    ltfSizeToOutputInXandY
        .setText(metaData.getSizeToOutputInXandY(axisID).toString(true));
    updateFiducialess();
    updateEnabled();
  }

  void setFiducialessAlignment(final boolean input) {
    cbFiducialess.setSelected(input);
    updateFiducialess();
  }

  void setImageRotation(final String input) {
    ltfRotation.setText(input);
  }

  void setBinning(final ConstEtomoNumber binning) {
    spinBinning.setValue(binning);
    updateEnabled();
  }

  private int getBinning() {
    return ((Integer) spinBinning.getValue()).intValue();
  }

  private void updateEnabled() {
    if (cbAntialiasFilter != null) {
      Number value = spinBinning.getValue();
      cbAntialiasFilter.setEnabled(value != null && value.intValue() > 1);
    }
  }

  public boolean isFiducialess() {
    return cbFiducialess.isSelected();
  }

  private void updateFiducialess() {
    ltfRotation.setEnabled(cbFiducialess.isSelected());
  }

  public String getImageRotation(final boolean doValidation)
      throws FieldValidationFailedException {
    return ltfRotation.getText(doValidation);
  }

  void updateAdvanced(final boolean advanced) {
    ltfSizeToOutputInXandY.setVisible(advanced);
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
  void action(final String command, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(cbFiducialess.getActionCommand())) {
      updateFiducialess();
    }
  }

  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.NEWSTACK, axisID);
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
      if (ltfSizeToOutputInXandY != null) {
        ltfSizeToOutputInXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
            NewstParam.SIZE_TO_OUTPUT_IN_X_AND_Y));
      }
    }
    if (cbUseLinearInterpolation != null) {
      cbUseLinearInterpolation
          .setToolTipText("Make aligned stack with linear instead of cubic "
              + "interpolation to  reduce noise.");
    }
    spinBinning.setToolTipText("Set the binning for the aligned image stack and "
        + "tomogram.  With a binned tomogram, all of the thickness, position, "
        + "and size parameters in Tomogram Generation are still entered in "
        + "unbinned pixels.");
    if (cbFiducialess != null) {
      cbFiducialess.setToolTipText("Use cross-correlation alignment only.");
    }
    if (ltfRotation != null) {
      ltfRotation.setToolTipText("Rotation angle of tilt axis for generating aligned "
          + "stack from " + "cross-correlation alignment only.");
    }
    if (cbAntialiasFilter != null) {
      cbAntialiasFilter
          .setToolTipText("Use antialiased image reduction instead binning with the "
              + "default filter in Newstack; useful for data from direct detection "
              + "cameras.");
    }
  }

  private static final class NewstackAndBlendmontParamPanelActionListener implements
      ActionListener {
    private final NewstackAndBlendmontParamPanel adaptee;

    private NewstackAndBlendmontParamPanelActionListener(
        final NewstackAndBlendmontParamPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  private static final class NewstackAndBlendmontBinningChangeListener implements
      ChangeListener {
    private final NewstackAndBlendmontParamPanel panel;

    private NewstackAndBlendmontBinningChangeListener(
        final NewstackAndBlendmontParamPanel panel) {
      this.panel = panel;
    }

    public void stateChanged(final ChangeEvent event) {
      panel.updateEnabled();
    }
  }
}