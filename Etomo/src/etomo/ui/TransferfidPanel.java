package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;

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
final class TransferfidPanel implements Expandable, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private final JPanel panelTransferfid = new JPanel();
  private final JPanel panelTransferfidBody = new JPanel();
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
  private final TransferfidPanelActionListener actionListener = new TransferfidPanelActionListener(
      this);

  private final PanelHeader header;
  private final Run3dmodButton buttonTransferfid;
  private final AxisID axisID;
  private final ApplicationManager manager;
  private final FiducialModelDialog parent;

  private TransferfidPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, FiducialModelDialog parent) {
    this.manager = manager;
    this.axisID = axisID;
    this.parent = parent;
    header = PanelHeader.getAdvancedBasicInstance("Transfer Fiducials", this,
        dialogType);
    panelTransferfidBody.setLayout(new BoxLayout(panelTransferfidBody,
        BoxLayout.Y_AXIS));
    cbRunMidas.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfidBody.add(cbRunMidas);

    //  Add a horizontal strut to keep the panel a minimum size    
    panelTransferfidBody.add(Box.createHorizontalStrut(300));
    panelTransferfidBody.add(ltfCenterViewA.getContainer());
    panelTransferfidBody.add(ltfCenterViewB.getContainer());
    panelTransferfidBody.add(ltfNumberViews.getContainer());

    bgSearchDirection.add(rbSearchBoth.getAbstractButton());
    bgSearchDirection.add(rbSearchPlus90.getAbstractButton());
    bgSearchDirection.add(rbSearchMinus90.getAbstractButton());
    panelSearchDirection.setLayout(new BoxLayout(panelSearchDirection,
        BoxLayout.Y_AXIS));
    panelSearchDirection.setBorder(new EtchedBorder("Search Direction")
        .getBorder());
    panelSearchDirection.add(rbSearchBoth.getComponent());
    panelSearchDirection.add(rbSearchPlus90.getComponent());
    panelSearchDirection.add(rbSearchMinus90.getComponent());
    panelSearchDirection.setAlignmentX(Component.CENTER_ALIGNMENT);
    panelTransferfidBody.add(panelSearchDirection);
    cbMirrorInX.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfidBody.add(cbMirrorInX);
    panelTransferfidBody.add(Box.createRigidArea(FixedDim.x0_y5));

    buttonTransferfid = (Run3dmodButton) manager
        .getProcessResultDisplayFactory(axisID).getTransferFiducials();
    buttonTransferfid.setContainer(this);
    buttonTransferfid.setAlignmentX(Component.CENTER_ALIGNMENT);
    buttonTransferfid.setSize();
    panelTransferfidBody.add(buttonTransferfid.getComponent());
    panelTransferfidBody.add(Box.createRigidArea(FixedDim.x0_y5));
    panelTransferfid
        .setLayout(new BoxLayout(panelTransferfid, BoxLayout.Y_AXIS));
    panelTransferfid.setBorder(BorderFactory.createEtchedBorder());
    panelTransferfid.add(header.getContainer());
    panelTransferfid.add(panelTransferfidBody);

    setToolTipText();
  }

  static TransferfidPanel getInstance(ApplicationManager manager,
      AxisID axisID, DialogType dialogType, FiducialModelDialog parent) {
    TransferfidPanel instance = new TransferfidPanel(manager, axisID,
        dialogType, parent);
    instance.addListeners();
    return instance;
  }
  
  void setDeferred3dmodButtons() {
    buttonTransferfid.setDeferred3dmodButton(parent.btnSeed);
  }

  public void action(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),menuOptions);
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
  private void action(final String command,Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(buttonTransferfid.getActionCommand())) {
      manager.transferfid(axisID, buttonTransferfid, null, deferred3dmodButton,
          run3dmodMenuOptions);
    }
  }

  Container getContainer() {
    return panelTransferfid;
  }

  /**
   * Update the header with the current advanced state
   */
  void updateAdvanced(boolean isAdvanced) {
    header.setAdvanced(isAdvanced);
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

  static ProcessResultDisplay getTransferFiducialsDisplay(DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        "Transfer Fiducials From Other Axis", dialogType);
  }

  private void setup() {
  }

  /**
   * Set the values of the panel using a TransferfidParam parameter
   * object
   */
  void setParameters() {
    TransferfidParam params = new TransferfidParam(manager, axisID);
    params.initialize();
    if (axisID == AxisID.SECOND) {
      manager.getMetaData().getTransferfidBFields(params);
    }
    else {
      manager.getMetaData().getTransferfidAFields(params);
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

  void setParameters(ReconScreenState screenState) {
    header.setButtonStates(screenState);
  }

  void getParameters(BaseScreenState screenState) {
    header.getButtonStates(screenState);
  }

  void getParameters() {
    getParameters(new TransferfidParam(manager, axisID));
  }

  /**
   * Get the values from the panel filling in the TransferfidParam object
   */
  void getParameters(TransferfidParam params) {
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
      manager.getMetaData().setTransferfidBFields(params);
    }
    else {
      manager.getMetaData().setTransferfidAFields(params);
    }
  }

  private void addListeners() {
    buttonTransferfid.addActionListener(actionListener);
  }

  void done() {
    buttonTransferfid.removeActionListener(actionListener);
  }

  private void setAdvanced(boolean isAdvanced) {
    ltfCenterViewA.setVisible(isAdvanced);
    ltfCenterViewB.setVisible(isAdvanced);
    ltfNumberViews.setVisible(isAdvanced);
    panelSearchDirection.setVisible(isAdvanced);
    cbMirrorInX.setVisible(isAdvanced);
  }

  void setEnabled(boolean isEnabled) {
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

  //  ToolTip string setup
  private void setToolTipText() {
    cbRunMidas
        .setToolTipText("Run Midas to adjust initial alignment manually.");
    ltfCenterViewA
        .setToolTipText("View from A around which to search for the best pair of views.");
    ltfCenterViewB
        .setToolTipText("View from B around which to search for the best pair of views.");
    ltfNumberViews
        .setToolTipText("Number of views from each axis to consider in searching for best pair.");
    rbSearchBoth
        .setToolTipText("Try both +90 and -90 degree rotations in searching for best pair of "
            + "views.");
    rbSearchPlus90
        .setToolTipText("Try only +90 degree rotations in searching for best pair of views.");
    rbSearchMinus90
        .setToolTipText("Try only -90 degree rotations in searching for best pair of views.");
    buttonTransferfid
        .setToolTipText("Run Transferfid to make a seed model for this axis from fiducial model for "
            + "the other axis.");
    cbMirrorInX
        .setToolTipText("Mirror one image around the X axis before rotating by 90 degrees.");
  }

  private final class TransferfidPanelActionListener implements ActionListener {
    private final TransferfidPanel adaptee;

    private TransferfidPanelActionListener(final TransferfidPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null,null);
    }
  }
}

/**
 * <p> $Log$
 * <p> Revision 3.16  2008/05/07 00:28:15  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.  Make transfer
 * <p> fid panel responsible for its own actions.
 * <p>
 * <p> Revision 3.15  2007/09/10 20:43:59  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.14  2007/03/07 21:16:45  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.
 * <p>
 * <p> Revision 3.13  2007/02/09 00:54:58  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.12  2006/07/04 18:48:01  sueh
 * <p> bug# 893 Added updateAdvanced(boolean) to change the header when the
 * <p> advanced button is pressed.
 * <p>
 * <p> Revision 3.11  2006/06/16 15:27:07  sueh
 * <p> bug# 734 Added open/close and adv/basic buttons.
 * <p>
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

