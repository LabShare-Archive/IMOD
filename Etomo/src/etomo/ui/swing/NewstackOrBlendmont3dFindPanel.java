package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.6  2010/04/09 03:01:46  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.5  2010/03/27 05:07:48  sueh
 * <p> bug# 1334 Added setParameters(boolean) to set binning.
 * <p>
 * <p> Revision 3.4  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.3  2009/09/21 17:56:30  sueh
 * <p> bug# 1267 Had been saving the 3dfindbinning to the wrong place in
 * <p> getParameters(MetaData) - corrected the problem.
 * <p>
 * <p> Revision 3.2  2009/09/20 21:33:02  sueh
 * <p> bug# 1268 Added a default value to LabeledSpinner.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
abstract class NewstackOrBlendmont3dFindPanel implements Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final ActionListener actionListener = new NewstackOrBlendmont3dFindPanelActionListener(
      this);
  private final LabeledSpinner spinBinning = LabeledSpinner.getInstance(
      NewstackAndBlendmontParamPanel.BINNING_LABEL + ": ", 1, 1, 8, 1);
  private final Run3dmodButton btn3dmodFull = Run3dmodButton.get3dmodInstance(
      "View Full Aligned Stack", this);

  private final NewstackOrBlendmont3dFindParent parent;
  final AxisID axisID;
  final ApplicationManager manager;
  final DialogType dialogType;

  NewstackOrBlendmont3dFindPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, NewstackOrBlendmont3dFindParent parent) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.parent = parent;
  }

  final void addListeners() {
    btn3dmodFull.addActionListener(actionListener);
  }

  final Component getComponent() {
    return pnlRoot;
  }

  final void createPanel() {
    // Initialize
    btn3dmodFull.setSize();
    pnlRoot.add(spinBinning.getContainer());
  }

  final Component get3dmodButton() {
    return btn3dmodFull.getComponent();
  }

  final String get3dmodFullButtonActionCommand() {
    return btn3dmodFull.getActionCommand();
  }

  final int getBinning() {
    return spinBinning.getValue().intValue();
  }

  final void setBinning(int input) {
    spinBinning.setValue(input);
  }

  /**
   * The Metadata values that are from the setup dialog should not be overrided
   * by this dialog unless the Metadata values are empty.
   * Must save data from the two instances under separate keys.
   * @param metaData
   * @throws FortranInputSyntaxException
   */
  final void getParameters(final MetaData metaData) {
    metaData.setStack3dFindBinning(axisID, spinBinning.getValue().intValue());
  }

  final void setParameters(final ConstMetaData metaData) {
    if (metaData.isStack3dFindBinningSet(axisID)) {
      spinBinning.setValue(metaData.getStack3dFindBinning(axisID));
    }
  }

  void initialize() {
    EtomoNumber beadSize = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    beadSize.set(manager.calcUnbinnedBeadDiameterPixels());
    if (!beadSize.isNull() && beadSize.isValid()) {
      spinBinning.setValue(Math.max((int) Math.round((beadSize.getDouble() / 5f)), 1));
    }
  }

  public final boolean validate() {
    int binning = spinBinning.getValue().intValue();
    // Warn if the pixel size is too small
    if (binning > 1) {
      EtomoNumber beadSize = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      beadSize.set(parent.getBeadSize());
      if (!beadSize.isNull() && beadSize.isValid() && beadSize.getDouble() / binning < 4) {
        if (!UIHarness.INSTANCE.openYesNoWarningDialog(manager,
            "The binned fiducial diameter will be less then 4 pixels.  Do you "
                + "want to continue?", axisID)) {
          return false;
        }
      }
    }
    return true;
  }

  abstract void runProcess(final ProcessResultDisplay processResultDisplay,
      final ProcessSeries processSeries, final Run3dmodMenuOptions run3dmodMenuOptions);

  public final void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
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
  abstract void action(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions);

  void setToolTipText() {
    spinBinning.setToolTipText("Set the binning for the aligned image stack and "
        + "tomogram to use with findbeads3d.");
    btn3dmodFull.setToolTipText("Open the complete aligned stack in 3dmod");
  }

  private static final class NewstackOrBlendmont3dFindPanelActionListener implements
      ActionListener {
    private final NewstackOrBlendmont3dFindPanel adaptee;

    private NewstackOrBlendmont3dFindPanelActionListener(
        final NewstackOrBlendmont3dFindPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }
}
