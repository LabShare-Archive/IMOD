package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.SpinnerNumberModel;

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
import etomo.type.MetaData;
import etomo.type.Run3dmodMenuOptions;
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
  private final LabeledSpinner spinBinning = new LabeledSpinner(BINNING_LABEL
      + ": ", new SpinnerNumberModel(1, 1, 8, 1), 1);

  private final LabeledTextField ltfSizeToOutputInXandY = new LabeledTextField(
      SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + " (X,Y - unbinned): ");
  private final LabeledTextField ltfRotation = new LabeledTextField(
      "Tilt axis rotation: ");
  private final CheckBox cbFiducialess = new CheckBox("Fiducialless alignment");
  private final CheckBox cbUseLinearInterpolation = new CheckBox(
      "Use linear interpolation");
  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;

  private NewstackAndBlendmontParamPanel(ApplicationManager manager,
      AxisID axisID, DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
  }

  static NewstackAndBlendmontParamPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType) {
    NewstackAndBlendmontParamPanel instance = new NewstackAndBlendmontParamPanel(
        manager, axisID, dialogType);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  private void addListeners() {
    NewstackAndBlendmontParamPanelActionListener actionListener = new NewstackAndBlendmontParamPanelActionListener(
        this);
    cbFiducialess.addActionListener(actionListener);
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  private void createPanel() {
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setComponentAlignmentX(Box.LEFT_ALIGNMENT);
    pnlRoot.add(cbUseLinearInterpolation);
    pnlRoot.add(spinBinning);
    pnlRoot.add(cbFiducialess);
    pnlRoot.add(ltfRotation);
    pnlRoot.add(ltfSizeToOutputInXandY);
    //update
    updateFiducialess();
  }

  void setVisible(boolean visible) {
    pnlRoot.setVisible(visible);
  }

  public void setParameters(BlendmontParam blendmontParam) {
    cbUseLinearInterpolation
        .setSelected(blendmontParam.isLinearInterpolation());
  }

  public void setParameters(ConstNewstParam newstParam) {
    cbUseLinearInterpolation.setSelected(newstParam.isLinearInterpolation());
  }

  /**
   * The Metadata values that are from the setup dialog should not be overrided
   * by this dialog unless the Metadata values are empty.
   * Must save data from the two instances under separate keys.
   * @param metaData
   * @throws FortranInputSyntaxException
   */
  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    metaData.setSizeToOutputInXandY(axisID, ltfSizeToOutputInXandY.getText());
    metaData.setStackBinning(axisID, getBinning());
  }

  public void getParameters(BlendmontParam blendmontParam)
      throws FortranInputSyntaxException, InvalidParameterException,
      IOException {
    blendmontParam.setBinByFactor(getBinning());
    blendmontParam
        .setLinearInterpolation(cbUseLinearInterpolation.isSelected());
    try {
      blendmontParam.convertToStartingAndEndingXandY(ltfSizeToOutputInXandY
          .getText(), manager.getMetaData().getImageRotation(axisID));
    }
    catch (FortranInputSyntaxException e) {
      e.printStackTrace();
      throw new FortranInputSyntaxException(
          NewstackAndBlendmontParamPanel.SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL
              + ":  " + e.getMessage());
    }
    blendmontParam.setFiducialess(cbFiducialess.isSelected());
  }

  //  Copy the newstack parameters from the GUI to the NewstParam object
  public void getParameters(NewstParam newstParam)
      throws FortranInputSyntaxException, InvalidParameterException,
      IOException {
    int binning = getBinning();
    // Only explicitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    if (binning > 1) {
      newstParam.setBinByFactor(binning);
    }
    else {
      newstParam.setBinByFactor(Integer.MIN_VALUE);
    }
    newstParam.setLinearInterpolation(cbUseLinearInterpolation.isSelected());
    newstParam.setSizeToOutputInXandY(ltfSizeToOutputInXandY.getText(),
        getBinning(), manager.getMetaData().getImageRotation(axisID));
  }

  void setParameters(ConstMetaData metaData) {
    spinBinning.setValue(metaData.getStackBinning(axisID));
    ltfSizeToOutputInXandY.setText(metaData.getSizeToOutputInXandY(axisID)
        .toString(true));
    updateFiducialess();
  }

  void setFiducialessAlignment(boolean input) {
    cbFiducialess.setSelected(input);
    updateFiducialess();
  }

  void setImageRotation(float input) {
    ltfRotation.setText(input);
  }

  void setBinning(ConstEtomoNumber binning) {
    spinBinning.setValue(binning);
  }

  private int getBinning() {
    return ((Integer) spinBinning.getValue()).intValue();
  }

  public boolean isFiducialess() {
    return cbFiducialess.isSelected();
  }

  private void updateFiducialess() {
    ltfRotation.setEnabled(cbFiducialess.isSelected());
  }

  public float getImageRotation() throws NumberFormatException {
    return Float.parseFloat(ltfRotation.getText());
  }

  void updateAdvanced(boolean advanced) {
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
  void action(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(cbFiducialess.getActionCommand())) {
      updateFiducialess();
    }
  }

  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.NEWSTACK,
          axisID);
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
    spinBinning
        .setToolTipText("Set the binning for the aligned image stack and "
            + "tomogram.  With a binned tomogram, all of the thickness, position, "
            + "and size parameters in Tomogram Generation are still entered in "
            + "unbinned pixels.");
    if (cbFiducialess != null) {
      cbFiducialess.setToolTipText("Use cross-correlation alignment only.");
    }
    if (ltfRotation != null) {
      ltfRotation
          .setToolTipText("Rotation angle of tilt axis for generating aligned "
              + "stack from " + "cross-correlation alignment only.");
    }
  }

  private static final class NewstackAndBlendmontParamPanelActionListener
      implements ActionListener {
    private final NewstackAndBlendmontParamPanel adaptee;

    private NewstackAndBlendmontParamPanelActionListener(
        final NewstackAndBlendmontParamPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
