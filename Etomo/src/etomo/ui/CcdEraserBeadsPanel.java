package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EnumeratedType;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
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
 * <p> Revision 3.3  2010/03/12 04:09:51  sueh
 * <p> bug# 1325 Changed the erase diameter field title.
 * <p>
 * <p> Revision 3.2  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/06/16 22:46:46  sueh
 * <p> bug# 1221 Factored out of final aligned stack dialog.
 * <p> </p>
 */
final class CcdEraserBeadsPanel implements Run3dmodButtonContainer,
    CcdEraserDisplay {
  public static final String rcsid = "$Id$";

  static final String CCD_ERASER_LABEL = "Erase Beads";
  static final String USE_ERASED_STACK_LABEL = "Use Erased Stack";

  private final CcdEraserPanelActionListener actionListener = new CcdEraserPanelActionListener(
      this);
  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final LabeledTextField ltfFiducialDiameter = new LabeledTextField(
      "Diameter to erase (pixels): ");
  private final ButtonGroup bgPolynomialOrder = new ButtonGroup();
  private final RadioButton rbPolynomialOrderUseMean = new RadioButton(
      "Use mean of surrounding points", PolynomialOrder.USE_MEAN,
      bgPolynomialOrder);
  private final RadioButton rbPolynomialOrderFitAPlane = new RadioButton(
      "Fit a plane to surrounding points", PolynomialOrder.FIT_A_PLANE,
      bgPolynomialOrder);
  private final Run3dmodButton btn3dmodCcdEraser = Run3dmodButton
      .get3dmodInstance("View Erased Stack", this);

  private final Run3dmodButton btnCcdEraser;
  private final MultiLineButton btnUseCcdEraser;
  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;

  private CcdEraserBeadsPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    ProcessResultDisplayFactory displayFactory = manager
        .getProcessResultDisplayFactory(axisID);
    btnCcdEraser = (Run3dmodButton) displayFactory.getCcdEraserBeads();
    btnUseCcdEraser = (MultiLineButton) displayFactory.getUseCcdEraserBeads();
  }

  static CcdEraserBeadsPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType) {
    CcdEraserBeadsPanel instance = new CcdEraserBeadsPanel(manager, axisID,
        dialogType);
    instance.createPanel();
    instance.addListeners();
    instance.setToolTipText();
    return instance;
  }

  private void addListeners() {
    btnCcdEraser.addActionListener(actionListener);
    btn3dmodCcdEraser.addActionListener(actionListener);
    btnUseCcdEraser.addActionListener(actionListener);
  }

  private void createPanel() {
    //local panels
    SpacedPanel ccdEraserParameterPanel = SpacedPanel.getInstance();
    JPanel polynomialOrderPanel = new JPanel();
    SpacedPanel ccdEraserButtonPanel = SpacedPanel.getInstance();
    //initalization
    btnCcdEraser.setContainer(this);
    btnCcdEraser.setDeferred3dmodButton(btn3dmodCcdEraser);
    btnCcdEraser.setSize();
    btn3dmodCcdEraser.setSize();
    btnUseCcdEraser.setSize();
    //Root
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new EtchedBorder("Erase Beads").getBorder());
    pnlRoot.add(ccdEraserParameterPanel);
    pnlRoot.add(ccdEraserButtonPanel);
    //ccderaser parameters
    ccdEraserParameterPanel.setBoxLayout(BoxLayout.X_AXIS);
    ccdEraserParameterPanel.add(ltfFiducialDiameter.getContainer());
    ccdEraserParameterPanel.add(polynomialOrderPanel);
    //polynomial order
    polynomialOrderPanel.setLayout(new BoxLayout(polynomialOrderPanel,
        BoxLayout.Y_AXIS));
    polynomialOrderPanel.setBorder(new EtchedBorder("Polynomial Order")
        .getBorder());
    polynomialOrderPanel.add(rbPolynomialOrderUseMean.getComponent());
    polynomialOrderPanel.add(rbPolynomialOrderFitAPlane.getComponent());
    //buttons
    ccdEraserButtonPanel.setBoxLayout(BoxLayout.X_AXIS);
    ccdEraserButtonPanel.add(btnCcdEraser.getComponent());
    ccdEraserButtonPanel.add(btn3dmodCcdEraser.getComponent());
    ccdEraserButtonPanel.add(btnUseCcdEraser.getComponent());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  String getPolynomialOrder() {
    return ((RadioButton.RadioButtonModel) bgPolynomialOrder.getSelection())
        .getEnumeratedType().toString();
  }

  void setPolynomialOrder(EnumeratedType enumeratedType) {
    if (rbPolynomialOrderUseMean.getEnumeratedType() == enumeratedType) {
      rbPolynomialOrderUseMean.setSelected(true);
    }
    else if (rbPolynomialOrderFitAPlane.getEnumeratedType() == enumeratedType) {
      rbPolynomialOrderFitAPlane.setSelected(true);
    }
  }

  /**
   * The Metadata values that are from the setup dialog should not be overrided
   * by this dialog unless the Metadata values are empty.
   * @param metaData
   * @throws FortranInputSyntaxException
   */
  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    metaData.setFinalStackFiducialDiameter(axisID, ltfFiducialDiameter
        .getText());
    metaData.setFinalStackPolynomialOrder(axisID, getPolynomialOrder());
  }

  void setParameters(ConstMetaData metaData) {
    if (!metaData.isFinalStackFiducialDiameterNull(axisID)) {
      ltfFiducialDiameter.setText((metaData
          .getFinalStackFiducialDiameter(axisID)));
    }
    else if (!metaData.isFinalStackBetterRadiusEmpty(axisID)) {
      //backwards compatibility - used to save better radius, convert it to
      //fiducial diameter in pixels
      EtomoNumber betterRadius = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      betterRadius.set(metaData.getFinalStackBetterRadius(axisID));
      ltfFiducialDiameter.setText(Math
          .round(betterRadius.getDouble() * 2 * 10.0) / 10.0);
    }
    else {
      //Currently not allowing an empty value to be saved.
      //Default fiducialDiameter to fiducialDiameter from setup / pixel size
      //(convert to pixels).  Round to 1 decimal place.
      ltfFiducialDiameter.setText((Math.round(metaData.getFiducialDiameter()
          / metaData.getPixelSize() * 10.0) / 10.0));
    }
    setPolynomialOrder(PolynomialOrder.getInstance(metaData
        .getFinalStackPolynomialOrder(axisID)));
  }

  public boolean getParameters(CCDEraserParam param) {
    param.setInputFile(DatasetFiles
        .getFullAlignedStackFileName(manager, axisID));
    param.setModelFile(FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(
        manager, axisID));
    param.setOutputFile(DatasetFiles
        .getErasedFiducialsFileName(manager, axisID));
    EtomoNumber fiducialDiameter = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    fiducialDiameter.set(ltfFiducialDiameter.getText());
    param.setBetterRadius(fiducialDiameter.getDouble() / 2.0);
    param.setPolynomialOrder(getPolynomialOrder());
    return param.validate();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnCcdEraser.getActionCommand())) {
      manager.ccdEraser(btnCcdEraser, null, deferred3dmodButton,
          run3dmodMenuOptions, axisID, dialogType, this);
    }
    else if (command.equals(btn3dmodCcdEraser.getActionCommand())) {
      manager.imodErasedFiducials(run3dmodMenuOptions, axisID);
    }
    else if (command.equals(btnUseCcdEraser.getActionCommand())) {
      manager.useCcdEraser(btnUseCcdEraser, axisID, dialogType,
          CCD_ERASER_LABEL);
    }
  }

  void done() {
    btnCcdEraser.removeActionListener(actionListener);
    btnUseCcdEraser.removeActionListener(actionListener);
  }

  final void setParameters(ReconScreenState screenState) {
    btnCcdEraser.setButtonState(screenState.getButtonState(btnCcdEraser
        .getButtonStateKey()));
    btnUseCcdEraser.setButtonState(screenState.getButtonState(btnUseCcdEraser
        .getButtonStateKey()));
  }

  private void setToolTipText() {
    ltfFiducialDiameter
        .setToolTipText("The diameter, in pixels of the aligned stack, that will "
            + "be erased around each point.");
    rbPolynomialOrderUseMean
        .setToolTipText("Fill the erased pixel with the mean of surrounding "
            + "points.");
    rbPolynomialOrderFitAPlane
        .setToolTipText("Fill the erased pixels with a gradient based on plane "
            + "fit to surrounding points.");
    btnCcdEraser
        .setToolTipText("Run Ccderaser on the aligned stack to erase around "
            + "model points.");
    btn3dmodCcdEraser
        .setToolTipText("View the results of running Ccderaser on the aligned "
            + "stack along with the _erase.fid model.");
    btnUseCcdEraser
        .setToolTipText("Replace the full aligned stack (.ali) with the erased "
            + "stack (_erase.ali).");
  }

  private final class CcdEraserPanelActionListener implements ActionListener {
    private final CcdEraserBeadsPanel adaptee;

    private CcdEraserPanelActionListener(final CcdEraserBeadsPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  private static final class PolynomialOrder implements EnumeratedType {
    private static final PolynomialOrder USE_MEAN = new PolynomialOrder(0);
    private static final PolynomialOrder FIT_A_PLANE = new PolynomialOrder(1);
    private static final PolynomialOrder DEFAULT = USE_MEAN;

    private final EtomoNumber value = new EtomoNumber();

    private PolynomialOrder(final int value) {
      this.value.set(value);
    }

    private static PolynomialOrder getInstance(final int value) {
      if (USE_MEAN.value.equals(value)) {
        return USE_MEAN;
      }
      if (FIT_A_PLANE.value.equals(value)) {
        return FIT_A_PLANE;
      }
      return DEFAULT;
    }

    public ConstEtomoNumber getValue() {
      return value;
    }

    public boolean isDefault() {
      return this == DEFAULT;
    }

    public String toString() {
      return value.toString();
    }
  }

}
