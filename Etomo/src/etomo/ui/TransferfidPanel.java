package etomo.ui;

import java.awt.Component;
import java.awt.Container;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.TransferfidParam;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003, 2004, 2005</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 */
public final class TransferfidPanel implements Expandable {
  public static final String rcsid = "$Id$";

  private final JPanel panelTransferfid = new JPanel();
  private final JPanel panelTransferfidBody = new JPanel();
  private MultiLineButton buttonTransferfid = null;
  private boolean includeButton = false;
  private final PanelHeader header;

  private final CheckBox cbRunMidas = new CheckBox("Run midas");
  private final LabeledTextField ltfCenterViewA = new LabeledTextField(
      "Center view A: ");
  private final LabeledTextField ltfCenterViewB = new LabeledTextField(
      "Center view B: ");
  private final LabeledTextField ltfNumberViews = new LabeledTextField(
      "Number of views in the search: ");
  private final CheckBox cbMirrorInX = new CheckBox(
      "Mirror one image around the X axis");

  private final JPanel panelSearchDirection = new JPanel();
  private final ButtonGroup bgSearchDirection = new ButtonGroup();
  private final RadioButton rbSearchBoth = new RadioButton("Both directions");
  private final RadioButton rbSearchPlus90 = new RadioButton("+90 (CCW) only");
  private final RadioButton rbSearchMinus90 = new RadioButton("-90 (CW) only");
  private final MetaData metaData;
  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DialogType dialogType;

  public TransferfidPanel(ApplicationManager manager, AxisID axisID,
      boolean inclButton, DialogType dialogType) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    metaData = manager.getMetaData();
    includeButton = inclButton;
    header = PanelHeader.getAdvancedBasicInstance("Transfer Fiducials", this,
        dialogType);
    setup();
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      panelTransferfidBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      setAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public static ProcessResultDisplay getTransferFiducialsDisplay(
      DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance(
        "Transfer Fiducials From Other Axis", dialogType);
  }

  private void setup() {
    panelTransferfidBody
        .setLayout(new BoxLayout(panelTransferfidBody, BoxLayout.Y_AXIS));
    cbRunMidas.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfidBody.add(cbRunMidas);

    //  Add a horizontal strut to keep the panel a minimum size    
    panelTransferfidBody.add(Box.createHorizontalStrut(300));
    panelTransferfidBody.add(ltfCenterViewA.getContainer());
    panelTransferfidBody.add(ltfCenterViewB.getContainer());
    panelTransferfidBody.add(ltfNumberViews.getContainer());

    bgSearchDirection.add(rbSearchBoth);
    bgSearchDirection.add(rbSearchPlus90);
    bgSearchDirection.add(rbSearchMinus90);
    panelSearchDirection.setLayout(new BoxLayout(panelSearchDirection,
        BoxLayout.Y_AXIS));
    panelSearchDirection.setBorder(new EtchedBorder("Search Direction")
        .getBorder());
    panelSearchDirection.add(rbSearchBoth);
    panelSearchDirection.add(rbSearchPlus90);
    panelSearchDirection.add(rbSearchMinus90);
    panelSearchDirection.setAlignmentX(Component.CENTER_ALIGNMENT);
    panelTransferfidBody.add(panelSearchDirection);
    cbMirrorInX.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfidBody.add(cbMirrorInX);
    panelTransferfidBody.add(Box.createRigidArea(FixedDim.x0_y5));

    if (includeButton) {
      buttonTransferfid = (MultiLineButton) manager
          .getProcessResultDisplayFactory(axisID).getTransferFiducials();
      buttonTransferfid.setAlignmentX(Component.CENTER_ALIGNMENT);
      buttonTransferfid.setSize();
      panelTransferfidBody.add(buttonTransferfid.getComponent());
      panelTransferfidBody.add(Box.createRigidArea(FixedDim.x0_y5));
    }
    panelTransferfid.setLayout(new BoxLayout(panelTransferfid, BoxLayout.Y_AXIS));
    panelTransferfid.setBorder(BorderFactory.createEtchedBorder());
    panelTransferfid.add(header.getContainer());
    panelTransferfid.add(panelTransferfidBody);

    setToolTipText();
  }

  /**
   * Set the values of the panel using a TransferfidParam parameter
   * object
   */
  public void setParameters() {
    TransferfidParam params = new TransferfidParam(manager, axisID);
    params.initialize();
    if (axisID == AxisID.SECOND) {
      metaData.getTransferfidBFields(params);
    }
    else {
      metaData.getTransferfidAFields(params);
    }
    cbRunMidas.setSelected(params.getRunMidas().is());
    ltfCenterViewA.setText(params.getCenterViewA().toString());
    ltfCenterViewB.setText(params.getCenterViewB().toString());
    ltfNumberViews.setText(params.getNumberViews().toString());

    if (params.getSearchDirection().isNull()) {
      rbSearchBoth.setSelected(true);
    }
    if (params.getSearchDirection().isNegative()) {
      rbSearchMinus90.setSelected(true);
    }
    if (params.getSearchDirection().isPositive()) {
      rbSearchPlus90.setSelected(true);
    }
    cbMirrorInX.setSelected(params.getMirrorInX().is());
  }

  public void setParameters(ReconScreenState screenState) {
    if (buttonTransferfid != null) {
      buttonTransferfid.setButtonState(screenState
          .getButtonState(buttonTransferfid.getButtonStateKey()));
    }
    header.setButtonStates(screenState);
  }
  
  public void getParameters(BaseScreenState screenState) {
    header.getButtonStates(screenState);
  }

  public void getParameters() {
    getParameters(new TransferfidParam(manager, axisID));
  }

  /**
   * Get the values from the panel filling in the TransferfidParam object
   */
  public void getParameters(TransferfidParam params) {
    params.setRunMidas(cbRunMidas.isSelected());
    params.setCenterViewA(ltfCenterViewA.getText());
    params.setCenterViewB(ltfCenterViewB.getText());
    if (rbSearchBoth.isSelected()) {
      params.getSearchDirection().reset();
    }
    if (rbSearchPlus90.isSelected()) {
      params.setSearchDirection(1);
    }
    if (rbSearchMinus90.isSelected()) {
      params.setSearchDirection(-1);
    }
    params.setNumberViews(ltfNumberViews.getText());
    params.setMirrorInX(cbMirrorInX.isSelected());
    if (axisID == AxisID.SECOND) {
      metaData.setTransferfidBFields(params);
    }
    else {
      metaData.setTransferfidAFields(params);
    }
  }

  public Container getContainer() {
    return panelTransferfid;
  }

  public void setAdvanced(boolean isAdvanced) {
    ltfCenterViewA.setVisible(isAdvanced);
    ltfCenterViewB.setVisible(isAdvanced);
    ltfNumberViews.setVisible(isAdvanced);
    panelSearchDirection.setVisible(isAdvanced);
    cbMirrorInX.setVisible(isAdvanced);
  }

  public void setEnabled(boolean isEnabled) {
    buttonTransferfid.setEnabled(isEnabled);
    cbRunMidas.setEnabled(isEnabled);
    ltfCenterViewA.setEnabled(isEnabled);
    ltfCenterViewB.setEnabled(isEnabled);
    ltfNumberViews.setEnabled(isEnabled);
    rbSearchBoth.setEnabled(isEnabled);
    rbSearchPlus90.setEnabled(isEnabled);
    rbSearchMinus90.setEnabled(isEnabled);
    cbMirrorInX.setEnabled(isEnabled);
  }

  public MultiLineButton getButton() {
    if (includeButton) {
      return buttonTransferfid;
    }

    return null;
  }

  //  ToolTip string setup
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "Run Midas to adjust initial alignment manually.";
    cbRunMidas.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View from A around which to search for the best pair of views.";
    ltfCenterViewA.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View from B around which to search for the best pair of views.";
    ltfCenterViewB.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Number of views from each axis to consider in searching for best pair.";
    ltfNumberViews.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Try both +90 and -90 degree rotations in searching for best pair of "
        + "views.";
    rbSearchBoth.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Try only +90 degree rotations in searching for best pair of views.";
    rbSearchPlus90.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Try only -90 degree rotations in searching for best pair of views.";
    rbSearchMinus90.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Run Transferfid to make a seed model for this axis from fiducial model for "
        + "the other axis.";
    buttonTransferfid.setToolTipText(tooltipFormatter.setText(text).format());

    cbMirrorInX.setToolTipText(tooltipFormatter.setText(
        "Mirror one image around the X axis before rotating by 90 degrees.")
        .format());
  }
}

/**
 * <p> $Log$
 * <p> Revision 3.10  2006/06/15 16:20:03  sueh
 * <p> bug# 873 Added tooltip for mirrorInX.
 * <p>
 * <p> Revision 3.9  2006/06/14 21:25:33  sueh
 * <p> bug# 873 Added cbMirrorInX.
 * <p>
 * <p> Revision 3.8  2006/02/06 21:22:53  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.7  2006/01/26 22:09:09  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.6  2006/01/04 00:02:28  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted
 * <p> JRadioButton's to RadioButton.
 * <p>
 * <p> Revision 3.5  2005/08/10 20:48:38  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.4  2005/07/29 00:54:52  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.3  2005/06/01 21:28:58  sueh
 * <p> bug# 667 Getting meta data from the manager instead of EtomoDirector.
 * <p>
 * <p> Revision 3.2  2005/01/26 00:07:10  sueh
 * <p> Removed script oriented functionality from EtomoNumber.
 * <p>
 * <p> Revision 3.1  2005/01/22 04:08:36  sueh
 * <p> bug# 509, bug# 591  In getParameters(), set transferfid fields in MetaData
 * <p> after updating the TransferfidParam parameter.  In setParameters(),
 * <p> create the TransferfidParam instance, initialize it, copy fields from
 * <p> MetaData, and then set the screen fields.
 * <p>
 */

